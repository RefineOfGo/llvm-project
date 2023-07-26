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
    GlobalVariable * var;

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

    /* only insert stack limit global for Linux,
     * macOS uses hard-coded slot index for stack limit */
    switch (Triple(mod.getTargetTriple()).getOS()) {
        case Triple::Linux  : break;
        case Triple::Darwin : return false;
        case Triple::MacOSX : return false;
        default             : report_fatal_error("ROG Stack Growing not supported on this platform.");
    }

    /* create one if not exists */
    if (!(var = mod.getNamedGlobal(kROGStackLimit))) {
        ret = true;
        var = new GlobalVariable(
            mod,
            i64,
            false,
            GlobalValue::ExternalLinkage,
            ConstantInt::get(i64, 0),
            kROGStackLimit,
            nullptr,
            GlobalValue::LocalExecTLSModel
        );
    }

    /* verify the stack limit variable type */
    assert(var->getValueType() == i64 && "Invalid type for ROG stack limit");
    assert(var->getLinkage() == GlobalValue::ExternalLinkage && "ROG stack limit must be externaly visible");
    assert(var->getThreadLocalMode() == GlobalValue::LocalExecTLSModel && "ROG stack limit must be thread_local(localexec)");
    return ret;
}
