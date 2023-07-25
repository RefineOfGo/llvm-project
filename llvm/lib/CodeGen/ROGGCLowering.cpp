#include "llvm/CodeGen/Passes.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/ROGGC.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace llvm;

namespace {
struct ROGGCLowering : public FunctionPass {
    static char ID;
    ROGGCLowering();

public:
    bool runOnFunction(Function &fn) override;

private:
    void invokeBefore(CallInst *ir, ArrayRef<Value *> args, FunctionCallee fn);
    void insertUnitBarrier(CallInst *ir, Value *mem, Value *val);
    void insertBulkBarrier(CallInst *ir, Value *dest, Value *src, Value *size);

private:
    void replaceStore(CallInst *ir);
    void replaceMemClr(CallInst *ir);
    void replaceMemOps(CallInst *ir, Intrinsic::ID iid);
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

INITIALIZE_PASS(
    ROGGCLowering,
    "rog-gc-lowering",
    "ROG GC Lowering",
    false,  // cfg
    false   // analysis
)

static bool isOptionEnabled(Attribute attr) {
    auto val = attr.getValueAsString();
    assert(val.empty() || val == "true" || val == "false");
    return val != "false";
}

FunctionPass *llvm::createROGGCLoweringPass() {
    return new ROGGCLowering();
}

ROGGCLowering::ROGGCLowering() : FunctionPass(ID) {
    initializeROGGCLoweringPass(*PassRegistry::getPassRegistry());
}

bool ROGGCLowering::runOnFunction(Function &fn) {
    bool isGC        = fn.hasGC() && fn.getGC() == ROG_GC_NAME;
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
                    case Intrinsic::gcmemclr      : ok = true; replaceMemClr(ir); break;
                    case Intrinsic::gcmemcpy      : ok = true; replaceMemOps(ir, Intrinsic::memcpy); break;
                    case Intrinsic::gcmemmove     : ok = true; replaceMemOps(ir, Intrinsic::memmove); break;
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

void ROGGCLowering::invokeBefore(CallInst *ir, ArrayRef<Value *> args, FunctionCallee fn) {
    CallInst::Create(fn, args, "", SplitBlockAndInsertIfThen(
        CmpInst::Create(
            Instruction::ICmp,
            CmpInst::ICMP_NE,
            new LoadInst(
                Type::getInt32Ty(ir->getContext()),
                ir->getModule()->getOrInsertGlobal(ROG_GCWB_SW, Type::getInt32Ty(ir->getContext())),
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
    ))->setCallingConv(CallingConv::Cold);
}

void ROGGCLowering::insertUnitBarrier(CallInst *ir, Value *mem, Value *val) {
    invokeBefore(ir, { mem, val }, ir->getModule()->getOrInsertFunction(
        ROG_GCWB_ONE,
        Type::getVoidTy(ir->getContext()),
        Type::getInt8PtrTy(ir->getContext())->getPointerTo(),
        Type::getInt8PtrTy(ir->getContext())
    ));
}

void ROGGCLowering::insertBulkBarrier(CallInst *ir, Value *dest, Value *src, Value *size) {
    invokeBefore(ir, { dest, src, size }, ir->getModule()->getOrInsertFunction(
        ROG_GCWB_BULK,
        Type::getVoidTy(ir->getContext()),
        Type::getInt8PtrTy(ir->getContext()),
        Type::getInt8PtrTy(ir->getContext()),
        Type::getInt64Ty(ir->getContext())
    ));
}

void ROGGCLowering::replaceStore(CallInst *ir) {
    Value *   val = ir->getArgOperand(0);
    Value *   mem = ir->getArgOperand(2);
    Attribute ord = ir->getFnAttr("order");
    Attribute vlt = ir->getFnAttr("volatile");

    /* create a store instruction */
    auto store = new StoreInst(
        val,
        mem,
        vlt.isStringAttribute() && isOptionEnabled(vlt),
        Align::Of<void **>()
    );

    /* insert the write barrier check with a branch weight that represents "very unlikely"
     * and call barrier marker function in the new branch */
    insertUnitBarrier(ir, mem, val);
    ReplaceInstWithInst(ir, store);

    /* check for atomic ordering */
    if (ord.isStringAttribute()) {
        auto iter = AtomicOrderingMap.find(ord.getValueAsString());
        assert(iter != AtomicOrderingMap.end() && "Invalid store order");
        store->setAtomic(iter->second);
    }
}

void ROGGCLowering::replaceMemClr(CallInst *ir) {
    Value *mem = ir->getArgOperand(0);
    Value *len = ir->getArgOperand(1);
    Value *vlt = ir->getArgOperand(2);
    Value *val = ConstantInt::get(Type::getInt8Ty(ir->getContext()), 0);
    Value *nil = ConstantPointerNull::get(cast<PointerType>(mem->getType()));

    /* create a function call to the memset intrinsic */
    auto *fn = CallInst::Create(
        Intrinsic::getDeclaration(ir->getModule(), Intrinsic::memset, { mem->getType(), len->getType() }),
        { mem, val, len, vlt }
    );

    /* insert the write barrier check with a branch weight that represents "very unlikely"
     * and call bulk barrier marker function in the new branch */
    insertBulkBarrier(ir, mem, nil, len);
    ReplaceInstWithInst(ir, fn);
}

void ROGGCLowering::replaceMemOps(CallInst *ir, Intrinsic::ID iid) {
    Value *mem = ir->getArgOperand(0);
    Value *src = ir->getArgOperand(1);
    Value *len = ir->getArgOperand(2);
    Value *vlt = ir->getArgOperand(3);

    /* create a function call to the mem{cpy,move} intrinsic */
    auto *fn = CallInst::Create(
        Intrinsic::getDeclaration(ir->getModule(), iid, { mem->getType(), src->getType(), len->getType() }),
        { mem, src, len, vlt }
    );

    /* insert the write barrier check with a branch weight that represents "very unlikely"
     * and call bulk barrier marker function in the new branch */
    insertBulkBarrier(ir, mem, src, len);
    ReplaceInstWithInst(ir, fn);
}

void ROGGCLowering::replaceAtomicCAS(CallInst *ir) {
    Value *   mem = ir->getArgOperand(0);
    Value *   cmp = ir->getArgOperand(1);
    Value *   val = ir->getArgOperand(2);
    Value *   opt = ir->getArgOperand(3);
    Value *   vlt = ir->getArgOperand(4);
    Attribute ord = ir->getFnAttr("order");
    Attribute fao = ir->getFnAttr("failure_order");

    /* default CAS ordering */
    AtomicOrdering successOrder = AtomicOrdering::SequentiallyConsistent;
    AtomicOrdering failureOrder = AtomicOrdering::SequentiallyConsistent;

    /* use specified atomic ordering if any */
    if (ord.isStringAttribute()) {
        auto it = AtomicOrderingMap.find(ord.getValueAsString());
        assert(it != AtomicOrderingMap.end() && "Invalid CAS order");
        successOrder = it->second;
    }

    /* use specified failure ordering if any */
    if (fao.isStringAttribute()) {
        auto it = AtomicOrderingMap.find(fao.getValueAsString());
        assert(it != AtomicOrderingMap.end() && "Invalid CAS failure order");
        failureOrder = it->second;
    }

    /* insert a new CAS instruction before the intrinsic */
    auto *cas = new AtomicCmpXchgInst(
        mem,
        cmp,
        val,
        Align::Of<void *>(),
        successOrder,
        failureOrder,
        SyncScope::System
    );

    /* mark as weak CAS if any */
    if (cast<ConstantInt>(*opt).isOne()) {
        cas->setWeak(true);
    }

    /* mark as volatile CAS if any */
    if (cast<ConstantInt>(*vlt).isOne()) {
        cas->setVolatile(true);
    }

    /* insert the write barrier check with a branch weight that represents "very unlikely"
     * and call barrier marker function in the new branch */
    insertUnitBarrier(ir, mem, val);
    ReplaceInstWithInst(ir, cas);
}

void ROGGCLowering::replaceAtomicSwap(CallInst *ir) {
    Value *        mem = ir->getArgOperand(0);
    Value *        val = ir->getArgOperand(1);
    Value *        vlt = ir->getArgOperand(2);
    Attribute      opt = ir->getFnAttr("order");
    AtomicOrdering ord = AtomicOrdering::SequentiallyConsistent;

    /* use specified atomic ordering if any */
    if (opt.isStringAttribute()) {
        auto it = AtomicOrderingMap.find(opt.getValueAsString());
        assert(it != AtomicOrderingMap.end() && "Invalid Xchg order");
        ord = it->second;
    }

    /* insert a new swap instruction before the intrinsic */
    auto *rmw = new AtomicRMWInst(
        AtomicRMWInst::Xchg,
        mem,
        val,
        Align::Of<void *>(),
        ord,
        SyncScope::System
    );

    /* mark as volatile xchg if any */
    if (cast<ConstantInt>(*vlt).isOne()) {
        rmw->setVolatile(true);
    }

    /* insert the write barrier check with a branch weight that represents "very unlikely"
     * and call barrier marker function in the new branch */
    insertUnitBarrier(ir, mem, val);
    ReplaceInstWithInst(ir, rmw);
}
