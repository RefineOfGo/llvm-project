#include "llvm/IR/GCStrategy.h"
#include "llvm/IR/ROGGC.h"

using namespace llvm;

namespace {
struct ROGGC : public GCStrategy {
    ROGGC() = default;
};
}

static GCRegistry::Add<ROGGC> _(
    "rog",
    "GC Strategy for the ROG project"
);

void llvm::linkROGGC() {
    /* provide hooks to ensure the containing library is fully loaded */
}
