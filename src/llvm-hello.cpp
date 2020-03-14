#include "llvm/IR/CallingConv.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/Module.h>

using namespace llvm;

static LLVMContext llvmContext;
Module *makeLLVMModuleMulAdd() {
  Module *mod = new Module("test", llvmContext);

  Constant *c = mod->getOrInsertFunction(
      "mul_add",
      /* return type*/
      IntegerType::get(llvmContext, 32),
      /* args */
      IntegerType::get(llvmContext, 32), IntegerType::get(llvmContext, 32),
      IntegerType::get(llvmContext, 32));
  Function *mul_add = cast<Function>(c);
  mul_add->setCallingConv(CallingConv::C);

  Function::arg_iterator args = mul_add->arg_begin();
  Value* x = args++;
  x->setName("x");
  Value* y = args++;
  y->setName("y");
  Value* z = args++;
  z->setName("z");

  BasicBlock* block = BasicBlock::Create(llvmContext, "entry", mul_add);
  IRBuilder<> builder(block);
  Value* tmp = builder.CreateMul(x, y, "tmp");
  Value* tmp2 = builder.CreateAdd(tmp, z, "tmp2");

  builder.CreateRet(tmp2);

  return mod;
}
int main() {
  Module *mod = makeLLVMModuleMulAdd();
  verifyModule(*mod);

  legacy::PassManager PM;
  ModulePass *modPass = createPrintModulePass(outs());
  PM.add(modPass);
  PM.run(*mod);

  delete mod;
  return 0;
}