#include "llvm/CodeGen/Passes.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Module.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include "llvm/Target/ROGStackCheckOptions.h"
#include "llvm/TargetParser/Triple.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace llvm;

namespace {
struct ROGStackCheckPreparing : public ModulePass {
    static char ID;
    ROGStackCheckPreparing();

public:
    bool runOnModule(Module &mod) override;
};
}

char ROGStackCheckPreparing::ID = 0;

INITIALIZE_PASS(
    ROGStackCheckPreparing,
    "rog-stack-check-preparing",
    "Preparations and verifications for ROG Stack Check",
    false,  // cfg
    false   // analysis
)

ModulePass *llvm::createROGStackCheckPreparingPass() {
    return new ROGStackCheckPreparing();
}

ROGStackCheckPreparing::ROGStackCheckPreparing() : ModulePass(ID) {
    initializeROGStackCheckPreparingPass(*PassRegistry::getPassRegistry());
}

bool ROGStackCheckPreparing::runOnModule(Module &mod) {
    bool             ok  = false;
    bool             ret = false;
    Type *           i64 = Type::getInt64Ty(mod.getContext());
    Triple           out = Triple(mod.getTargetTriple());
    GlobalVariable * var;

    /* only insert stack limit global for AArch64 Linux */
    if (out.getOS() != Triple::Linux || out.getArch() != Triple::aarch64) {
        return false;
    }

    /* check if any function needs stack checking */
    for (auto &fn : mod.functions()) {
        if (fn.hasFnAttribute(kROGStackCheckAttr)) {
            ok = true;
            break;
        }
    }

    /* none, just don't insert the stack limit global */
    if (!ok) {
        return false;
    }

    /* create one if not exists */
    if (!(var = mod.getNamedGlobal(kROGStackLimit))) {
        ret = true;
        var = new GlobalVariable(
            mod,
            i64,
            false,
            GlobalValue::LinkOnceODRLinkage,
            ConstantInt::get(i64, 0),
            kROGStackLimit,
            nullptr,
            GlobalValue::LocalExecTLSModel
        );
    }

    /* verify the stack limit variable type */
    assert(var->getValueType() == i64 && "Invalid type for ROG stack limit");
    assert(var->getLinkage() == GlobalValue::LinkOnceODRLinkage && "ROG stack limit must be linkonce_odr");
    assert(var->getThreadLocalMode() == GlobalValue::LocalExecTLSModel && "ROG stack limit must be thread_local(localexec)");
    return ret;
}
