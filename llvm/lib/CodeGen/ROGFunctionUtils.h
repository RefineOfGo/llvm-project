#ifndef LLVM_LIB_CODEGEN_ROGFUNCTIONUTILS_H
#define LLVM_LIB_CODEGEN_ROGFUNCTIONUTILS_H

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Module.h"

namespace rog {
using namespace llvm;

template <typename ...Args>
static FunctionCallee getOrInsertFunction(Module *mod, StringRef name, Type *ret, Args... args) {
    GlobalValue *fp = mod->getNamedValue(name);
    FunctionType *ty = FunctionType::get(ret, SmallVector<Type*, sizeof...(Args)> { args... }, false);

    /* already exists */
    if (fp != nullptr) {
        return FunctionCallee(ty, fp);
    }

    /* create a new one if not exist */
    Function *fn = Function::Create(
        ty,
        GlobalValue::ExternalWeakLinkage,
        name,
        *mod
    );

    /* mark as cold function */
    fn->setCallingConv(CallingConv::Cold);
    return FunctionCallee(ty, fn);
}
}

#endif
