#include "llvm/CodeGen/Passes.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace llvm;

#define GC_NAME     "rog"
#define GCWB_SW     "rog_gcwb_switch"
#define GCWB_FN     "rog_write_barrier"
#define DEBUG_TYPE  "rog-gc-lowering"

#define assert_x(x) assert(x)

namespace {
struct ROGGCLowering : public FunctionPass {
    static char ID;
    ROGGCLowering();

public:
    bool runOnFunction(Function &fn) override;

private:
    void insertGCWB(CallInst *ir, Value *mem, Value *val);
    void replaceStore(CallInst *ir);
    void replaceAtomicCAS(CallInst *ir);
    void replaceAtomicSwap(CallInst *ir);
};
}

static llvm::SmallDenseMap<StringRef, AtomicOrdering> AtomicOrderingMap = {
    { "not_atomic" , AtomicOrdering::NotAtomic              },
    { "unordered"  , AtomicOrdering::Unordered              },
    { "monotonic"  , AtomicOrdering::Monotonic              },
    { "acquire"    , AtomicOrdering::Acquire                },
    { "release"    , AtomicOrdering::Release                },
    { "acq_rel"    , AtomicOrdering::AcquireRelease         },
    { "seq_cst"    , AtomicOrdering::SequentiallyConsistent },
};

char ROGGCLowering::ID = 0;
char &llvm::ROGGCLoweringID = ROGGCLowering::ID;

INITIALIZE_PASS_BEGIN(ROGGCLowering, DEBUG_TYPE, "ROG GC Lowering", false, false)
INITIALIZE_PASS_DEPENDENCY(GCModuleInfo)
INITIALIZE_PASS_END(ROGGCLowering, DEBUG_TYPE, "ROG GC Lowering", false, false)

FunctionPass *llvm::createROGGCLoweringPass() {
    return new ROGGCLowering();
}

ROGGCLowering::ROGGCLowering() : FunctionPass(ID) {
    initializeROGGCLoweringPass(*PassRegistry::getPassRegistry());
}

bool ROGGCLowering::runOnFunction(Function &fn) {
    bool isGC        = fn.hasGC() && fn.getGC() == GC_NAME;
    bool madeChanges = false;

    /* check for GC strategy */
    if (!isGC) {
        return false;
    }

    /* process all the instructions */
    for (auto &bb : fn) {
        for (auto &ins : bb) {
            bool       ok = false;
            CallInst * ir = dyn_cast<CallInst>(&ins);

            /* not a function call */
            if (ir == nullptr) {
                continue;
            }

            /* check for intrinsic ID */
            if (auto *fp = ir->getCalledFunction()) {
                switch (fp->getIntrinsicID()) {
                    case Intrinsic::gcwrite       : ok = true; replaceStore(ir); break;
                    case Intrinsic::gcatomic_cas  : ok = true; replaceAtomicCAS(ir); break;
                    case Intrinsic::gcatomic_swap : ok = true; replaceAtomicSwap(ir); break;
                }
            }

            /* the lowering will split the basic block, so there will
             * be no instructions after this point */
            if (ok) {
                madeChanges = true;
                break;
            }
        }
    }

    /* align the function to a pointer boundary */
    fn.setAlignment(Align::Of<void *>());
    return madeChanges;
}

void ROGGCLowering::insertGCWB(CallInst *ir, Value *mem, Value *val) {
    CallInst::Create(
        ir->getModule()->getOrInsertFunction(
            GCWB_FN,
            Type::getVoidTy(ir->getContext()),
            Type::getInt8PtrTy(ir->getContext())->getPointerTo(),
            Type::getInt8PtrTy(ir->getContext())
        ),
        { mem, val },
        "",
        SplitBlockAndInsertIfThen(
            CmpInst::Create(
                Instruction::ICmp,
                CmpInst::ICMP_NE,
                new LoadInst(
                    Type::getInt32Ty(ir->getContext()),
                    ir->getModule()->getOrInsertGlobal(GCWB_SW, Type::getInt32Ty(ir->getContext())),
                    "",
                    true,
                    ir
                ),
                ConstantInt::get(Type::getInt32Ty(ir->getContext()), 0),
                "",
                ir
            ),
            ir,
            false,
            MDBuilder(ir->getContext()).createBranchWeights({
                1,
                INT32_MAX >> 1,
            })
        )
    )->setCallingConv(CallingConv::Cold);
}

void ROGGCLowering::replaceStore(CallInst *ir) {
    Value *   val   = ir->getArgOperand(0);
    Value *   mem   = ir->getArgOperand(2);
    Attribute attr  = ir->getFnAttr("atomic_ordering");

    /* insert the store instruction */
    auto align = Align::Of<void **>();
    auto store = new StoreInst(val, mem, false, align);

    /* insert the write barrier check with a branch weight that represents "very unlikely"
     * and call barrier marker function in the new branch */
    insertGCWB(ir, mem, val);
    ReplaceInstWithInst(ir, store);

    /* check for atomic ordering */
    if (!attr.isStringAttribute()) {
        assert(!attr.isValid() && "Invalid atomic_ordering attribute");
        return;
    }

    /* find the atomic ordering */
    auto name = attr.getValueAsString();
    auto iter = AtomicOrderingMap.find(name);

    /* check & set the atomic ordering */
    assert(iter != AtomicOrderingMap.end() && "Invalid atomic_ordering value");
    store->setAtomic(iter->second);
}

void ROGGCLowering::replaceAtomicCAS(CallInst *ir) {
    Value *mem = ir->getArgOperand(0);
    Value *cmp = ir->getArgOperand(1);
    Value *val = ir->getArgOperand(2);

    /* insert a new CAS instruction before the intrinsic */
    auto *cas = new AtomicCmpXchgInst(
        mem,
        cmp,
        val,
        Align::Of<void *>(),
        AtomicOrdering::SequentiallyConsistent,
        AtomicOrdering::SequentiallyConsistent,
        SyncScope::System
    );

    /* insert the write barrier check with a branch weight that represents "very unlikely"
     * and call barrier marker function in the new branch */
    insertGCWB(ir, mem, val);
    ReplaceInstWithInst(ir, cas);
}

void ROGGCLowering::replaceAtomicSwap(CallInst *ir) {
    Value *mem = ir->getArgOperand(0);
    Value *val = ir->getArgOperand(1);

    /* insert a new swap instruction before the intrinsic */
    auto *cas = new AtomicRMWInst(
        AtomicRMWInst::Xchg,
        mem,
        val,
        Align::Of<void *>(),
        AtomicOrdering::SequentiallyConsistent,
        SyncScope::System
    );

    /* insert the write barrier check with a branch weight that represents "very unlikely"
     * and call barrier marker function in the new branch */
    insertGCWB(ir, mem, val);
    ReplaceInstWithInst(ir, cas);
}
