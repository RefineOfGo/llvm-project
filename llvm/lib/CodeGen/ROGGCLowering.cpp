#include "llvm/CodeGen/Passes.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace llvm;

#define GC_NAME     "rog"
#define VAR_NAME    "writeBarrier"
#define FUNC_NAME   "gcWriteBarrier"
#define DEBUG_TYPE  "rog-gc-lowering"

namespace {
struct ROGGCLowering : public FunctionPass {
    static char ID;
    ROGGCLowering();

public:
    bool runOnFunction(Function &fn) override;

private:
    void lowerWriteBarrier(Function &fn);
};
}

char ROGGCLowering::ID = 0;
char &llvm::ROGGCLoweringID = ROGGCLowering::ID;

INITIALIZE_PASS_BEGIN(ROGGCLowering, DEBUG_TYPE, "ROG GC Lowering", false, false)
INITIALIZE_PASS_DEPENDENCY(GCModuleInfo)
INITIALIZE_PASS_END(ROGGCLowering, DEBUG_TYPE, "ROG GC Lowering", false, false)

static Function *asWriteBarrierFn(CallInst *p) {
    auto *fn = p ? p->getCalledFunction() : nullptr;
    return fn && fn->getIntrinsicID() == Intrinsic::gcwrite ? fn : nullptr;
}

FunctionPass *llvm::createROGGCLoweringPass() {
    return new ROGGCLowering();
}

ROGGCLowering::ROGGCLowering() : FunctionPass(ID) {
    initializeROGGCLoweringPass(*PassRegistry::getPassRegistry());
}

bool ROGGCLowering::runOnFunction(Function &fn) {
    if (!fn.hasGC() || fn.getGC() != GC_NAME) {
        return false;
    } else {
        lowerWriteBarrier(fn);
        return true;
    }
}

void ROGGCLowering::lowerWriteBarrier(Function &fn) {
    for (auto &bb : fn) {
        auto          it  = bb.begin();
        Module *      mod = bb.getModule();
        LLVMContext & ctx = bb.getContext();

        /* scan every instruction */
        while (it != bb.end()) {
            CallInst *ir = dyn_cast<CallInst>(&*it++);
            Function *wb = asWriteBarrierFn(ir);

            /* must be a function call to "@llvm.gcwrite" */
            if (wb == nullptr) {
                continue;
            }

            /* sanity check */
            assert(wb->arg_size() == 3 && "Invalid @llvm.gcwrite call!");
            assert(ir->arg_size() == 3 && "Invalid @llvm.gcwrite call!");

            /* extract the operands */
            Value *val = ir->getArgOperand(0);
            Value *obj = ir->getArgOperand(1);
            Value *mem = ir->getArgOperand(2);

            /* they must all be pointers */
            assert(val->getType()->isPtrOrPtrVectorTy() && "Value is not a pointer!");
            assert(obj->getType()->isPtrOrPtrVectorTy() && "Object is not a pointer!");
            assert(mem->getType()->isPtrOrPtrVectorTy() && "Memory slot is not a pointer!");

            /* find the write barrier function and variable */
            auto *wbfn  = mod->getFunction(FUNC_NAME);
            auto *wbvar = mod->getGlobalVariable(VAR_NAME, true);

            /* they must exist */
            assert(wbfn != nullptr && "Barrier function \"" FUNC_NAME "\" does not exist!");
            assert(wbvar != nullptr && "Barrier state variable \"" VAR_NAME "\" does not exist!");

            /* resolve the types */
            Type *        i8  = Type::getInt8Ty(ctx);
            Type *        i32 = Type::getInt32Ty(ctx);
            Instruction * then;
            Instruction * other;

            /* load and test the write barrier flags */
            CmpInst *cond = CmpInst::Create(
                Instruction::ICmp,
                CmpInst::ICMP_NE,
                new LoadInst(i8, wbvar, "", true, ir),
                ConstantInt::get(i8, APInt::getZero(8)),
                "",
                ir
            );

            /* construct a branch weight that represents "very unlikely" */
            MDNode *weights = MDNode::get(ctx, ArrayRef<Metadata *>({
                MDString::get(ctx, "branch_weights"),
                ConstantAsMetadata::get(ConstantInt::get(i32, APInt::getZero(32))),
                ConstantAsMetadata::get(ConstantInt::get(i32, APInt::getMaxValue(32))),
            }));

            /* insert the write barrier */
            SplitBlockAndInsertIfThenElse(cond, ir, &then, &other, weights);
            ir->eraseFromParent();

            /* emit instructions for either cases */
            new StoreInst(val, mem, false, Align::Of<void **>(), other);
            CallInst::Create(FunctionCallee(wbfn), ArrayRef<Value *>({ mem, val }), "", then);
            break;
        }
    }
}
