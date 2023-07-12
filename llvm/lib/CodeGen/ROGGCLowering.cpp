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
#define GCWB_SW     "rog_gcwb_enabled"
#define GCWB_MARK   "rog_gcwb_mark_atomic"
#define GCWB_STORE  "rog_gcwb_store_ptr"
#define DEBUG_TYPE  "rog-gc-lowering"

#define assert_x(x) assert(x)

namespace {
struct ROGGCLowering : public FunctionPass {
    static char ID;
    ROGGCLowering();

public:
    bool runOnFunction(Function &fn) override;

private:
    void           barrierCall(FunctionCallee fn, ArrayRef<Value *> args, Instruction *insertBefore);
    Value *        barrierCond(LLVMContext &ctx, Module *mod, Instruction *insertBefore);
    FunctionCallee barrierMarkFn(LLVMContext &ctx, Module *mod);
    FunctionCallee barrierStoreFn(LLVMContext &ctx, Module *mod);

private:
    void replaceStore(CallInst *ir);
    void replaceAtomicCAS(CallInst *ir);
    void replaceAtomicSwap(CallInst *ir);
};
}

char ROGGCLowering::ID = 0;
char &llvm::ROGGCLoweringID = ROGGCLowering::ID;

INITIALIZE_PASS_BEGIN(ROGGCLowering, DEBUG_TYPE, "ROG GC Lowering", false, false)
INITIALIZE_PASS_DEPENDENCY(GCModuleInfo)
INITIALIZE_PASS_END(ROGGCLowering, DEBUG_TYPE, "ROG GC Lowering", false, false)

static MDNode *makeVeryUnlikely(LLVMContext &ctx) {
    return MDBuilder(ctx).createBranchWeights({ 1, INT32_MAX >> 1 });
}

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

    dbgs() << fn << "\n";
    /* align the function to a pointer boundary */
    fn.setAlignment(Align::Of<void *>());
    return madeChanges;
}

void ROGGCLowering::barrierCall(FunctionCallee fn, ArrayRef<Value *> args, Instruction *insertBefore) {
    CallInst::Create(fn, args, "", insertBefore)->setCallingConv(CallingConv::Cold);
}

Value *ROGGCLowering::barrierCond(LLVMContext &ctx, Module *mod, Instruction *insertBefore) {
    return CmpInst::Create(
        Instruction::ICmp,
        CmpInst::ICMP_NE,
        new LoadInst(
            Type::getInt32Ty(ctx),
            mod->getOrInsertGlobal(GCWB_SW, Type::getInt32Ty(ctx)),
            "",
            true,
            insertBefore
        ),
        ConstantInt::get(Type::getInt32Ty(ctx), 0),
        "",
        insertBefore
    );
}

FunctionCallee ROGGCLowering::barrierMarkFn(LLVMContext &ctx, Module *mod) {
    return mod->getOrInsertFunction(
        GCWB_MARK,
        Type::getVoidTy(ctx),
        Type::getInt8PtrTy(ctx)->getPointerTo(),
        Type::getInt8PtrTy(ctx)
    );
}

FunctionCallee ROGGCLowering::barrierStoreFn(LLVMContext &ctx, Module *mod) {
    return mod->getOrInsertFunction(
        GCWB_STORE,
        Type::getVoidTy(ctx),
        Type::getInt8PtrTy(ctx)->getPointerTo(),
        Type::getInt8PtrTy(ctx)
    );
}

void ROGGCLowering::replaceStore(CallInst *ir) {
    auto *        mod   = ir->getModule();
    auto &        ctx   = ir->getContext();
    Value *       val   = ir->getArgOperand(0);
    Value *       mem   = ir->getArgOperand(2);
    Instruction * taken = nullptr;
    Instruction * other = nullptr;

    /* insert the write barrier check with a branch weight that represents "very unlikely" */
    SplitBlockAndInsertIfThenElse(
        barrierCond(ctx, mod, ir),
        ir,
        &taken,
        &other,
        makeVeryUnlikely(ctx)
    );

    /* remove the original instruction, and insert instructions for either cases */
    ir->eraseFromParent();
    barrierCall(barrierStoreFn(ctx, mod), { mem, val }, taken);
    new StoreInst(val, mem, false, Align::Of<void **>(), other);
}

void ROGGCLowering::replaceAtomicCAS(CallInst *ir) {
    auto *  mod = ir->getModule();
    auto &  ctx = ir->getContext();
    Value * mem = ir->getArgOperand(0);
    Value * cmp = ir->getArgOperand(1);
    Value * val = ir->getArgOperand(2);

    /* insert the write barrier check with a branch weight that represents "very unlikely"
     * and call barrier marker function in the new branch */
    barrierCall(barrierMarkFn(ctx, mod), { mem, val }, SplitBlockAndInsertIfThen(
        barrierCond(ctx, mod, ir),
        ir,
        false,
        makeVeryUnlikely(ctx)
    ));

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

    /* replace the intrinsic with a CAS instruction */
    cas->takeName(ir);
    ReplaceInstWithInst(ir, cas);
}

void ROGGCLowering::replaceAtomicSwap(CallInst *ir) {
    auto *  mod = ir->getModule();
    auto &  ctx = ir->getContext();
    Value * mem = ir->getArgOperand(0);
    Value * val = ir->getArgOperand(1);

    /* insert the write barrier check with a branch weight that represents "very unlikely"
     * and call barrier marker function in the new branch */
    barrierCall(barrierMarkFn(ctx, mod), { mem, val }, SplitBlockAndInsertIfThen(
        barrierCond(ctx, mod, ir),
        ir,
        false,
        makeVeryUnlikely(ctx)
    ));

    /* insert a new swap instruction before the intrinsic */
    auto *cas = new AtomicRMWInst(
        AtomicRMWInst::Xchg,
        mem,
        val,
        Align::Of<void *>(),
        AtomicOrdering::SequentiallyConsistent,
        SyncScope::System
    );

    /* replace the intrinsic with a swap instruction */
    cas->takeName(ir);
    ReplaceInstWithInst(ir, cas);
}
