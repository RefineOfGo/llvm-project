#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/ROGPasses.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "ROGFunctionUtils.h"

using namespace llvm;

#define ROG_CHECKPOINT_FN       "rog_checkpoint_abi"
#define ROG_CHECKPOINT_SW       "rog_checkpoint_switch"
#define ROG_CHECKPOINT_ATTR     "rog-checkpoint"

namespace {
struct ROGCheckPointInsertionImpl {
    static bool run(Function &fn, LoopInfo &li);

private:
    static void insertCheckPointAt(Instruction *ir);
    static void insertCheckPointForLoops(LoopInfo &li);
};
}

bool ROGCheckPointInsertionImpl::run(Function &fn, LoopInfo &li) {
    auto  it = fn.begin();
    auto *ir = &*it->begin();

    /* check if this function needs check-point */
    if (!fn.hasFnAttribute(ROG_CHECKPOINT_ATTR)) {
        return false;
    }

    /* insert checkpoint for this function */
    insertCheckPointAt(ir);
    insertCheckPointForLoops(li);
    return true;
}

void ROGCheckPointInsertionImpl::insertCheckPointAt(Instruction *ir) {
    CallInst::Create(
        rog::getOrInsertFunction(
            ir->getModule(),
            ROG_CHECKPOINT_FN,
            Type::getVoidTy(ir->getContext())
        ),
        {},
        "",
        SplitBlockAndInsertIfThen(
            CmpInst::Create(
                Instruction::ICmp,
                CmpInst::ICMP_NE,
                new LoadInst(
                    Type::getInt32Ty(ir->getContext()),
                    ir->getModule()->getOrInsertGlobal(ROG_CHECKPOINT_SW, Type::getInt32Ty(ir->getContext()), [&] {
                        return new GlobalVariable(
                            *ir->getModule(),
                            Type::getInt32Ty(ir->getContext()),
                            false,
                            GlobalVariable::LinkOnceODRLinkage,
                            ConstantInt::get(Type::getInt32Ty(ir->getContext()), 0),
                            ROG_CHECKPOINT_SW
                        );
                    }),
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

void ROGCheckPointInsertionImpl::insertCheckPointForLoops(LoopInfo &li) {
    for (auto loop : li) {
        BasicBlock *                 head;
        SmallVector<BasicBlock *, 4> back;

        /* get the head & latches */
        head = loop->getHeader();
        loop->getLoopLatches(back);

        /* split the latch if needed */
        for (auto bb : back) {
            if (bb->getUniqueSuccessor()) {
                insertCheckPointAt(&*bb->rbegin());
            } else {
                insertCheckPointAt(&*SplitEdge(bb, head)->rbegin());
            }
        }
    }
}

/** New Pass Manager **/

PreservedAnalyses ROGCheckPointInsertionPass::run(Function &fn, FunctionAnalysisManager &am) {
    if (!ROGCheckPointInsertionImpl::run(fn, am.getResult<LoopAnalysis>(fn))) {
        return PreservedAnalyses::all();
    } else {
        return PreservedAnalyses::none();
    }
}

/** Legacy Pass Manager **/

namespace {
struct ROGCheckPointInsertion : public FunctionPass {
    static char ID;
    ROGCheckPointInsertion();

public:
    bool runOnFunction(Function &fn) override;
    void getAnalysisUsage(AnalysisUsage &au) const override;
};
}

char ROGCheckPointInsertion::ID = 0;
char &llvm::ROGCheckPointInsertionID = ROGCheckPointInsertion::ID;

INITIALIZE_PASS(
    ROGCheckPointInsertion,
    "rog-checkpoint-insertion",
    "ROG Check Point Insertion",
    false,
    false
)

FunctionPass *llvm::createROGCheckPointInsertionPass() {
    return new ROGCheckPointInsertion();
}

ROGCheckPointInsertion::ROGCheckPointInsertion() : FunctionPass(ID) {
    initializeROGCheckPointInsertionPass(*PassRegistry::getPassRegistry());
}

bool ROGCheckPointInsertion::runOnFunction(Function &fn) {
    auto &li = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
    return ROGCheckPointInsertionImpl::run(fn, li);
}

void ROGCheckPointInsertion::getAnalysisUsage(AnalysisUsage &au) const {
    au.addRequiredTransitive<LoopInfoWrapperPass>();
}
