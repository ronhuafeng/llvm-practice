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

Module *makeLLVMModuleGCD(Module *exitedMod = nullptr) {
  Module *mod =
      exitedMod == nullptr ? new Module("test", llvmContext) : exitedMod;

  Constant *c = mod->getOrInsertFunction("gcd",
                                         /* return type*/
                                         IntegerType::get(llvmContext, 32),
                                         /* args */
                                         IntegerType::get(llvmContext, 32),
                                         IntegerType::get(llvmContext, 32));
  Function *gcd = cast<Function>(c);
  gcd->setCallingConv(CallingConv::C);

  Function::arg_iterator args = gcd->arg_begin();
  Value *x = args++;
  x->setName("x");
  Value *y = args++;
  y->setName("y");

  BasicBlock *entry = BasicBlock::Create(llvmContext, "entry", gcd);
  BasicBlock *ret = BasicBlock::Create(llvmContext, "return", gcd);
  BasicBlock *cond_false = BasicBlock::Create(llvmContext, "cond_false", gcd);
  BasicBlock *cond_true = BasicBlock::Create(llvmContext, "cond_true", gcd);
  BasicBlock *cond_false_2 =
      BasicBlock::Create(llvmContext, "cond_false_2", gcd);

  IRBuilder<> builder(entry);
  Value *xEqualsY = builder.CreateICmpEQ(x, y, "tmp");
  builder.CreateCondBr(xEqualsY, ret, cond_false);

  builder.SetInsertPoint(ret);
  // return 1
  builder.CreateRet(x);

  builder.SetInsertPoint(cond_false);
  Value *xLessThanY = builder.CreateICmpULT(x, y, "tmp");
  builder.CreateCondBr(xLessThanY, cond_true, cond_false_2);

  builder.SetInsertPoint(cond_true);
  Value *yMinusX = builder.CreateSub(y, x, "tmp");
  std::vector<Value *> args1;
  args1.push_back(x);
  args1.push_back(yMinusX);

  Value *recur_1 = builder.CreateCall(gcd, args1, "tmp");
  // return 2
  builder.CreateRet(recur_1);

  builder.SetInsertPoint(cond_false_2);
  Value *xMinusY = builder.CreateSub(x, y, "tmp");
  Value *recur_2 =
      builder.CreateCall(gcd, std::vector<Value *>{x, xMinusY}, "tmp");
  builder.CreateRet(recur_2);

  return mod;
}

Module *makeLLVMModuleMulAdd(Module *exitedMod = nullptr) {
  Module *mod =
      exitedMod == nullptr ? new Module("test", llvmContext) : exitedMod;

  Constant *c = mod->getOrInsertFunction("mul_add",
                                         /* return type*/
                                         IntegerType::get(llvmContext, 32),
                                         /* args */
                                         IntegerType::get(llvmContext, 32),
                                         IntegerType::get(llvmContext, 32),
                                         IntegerType::get(llvmContext, 32));
  Function *mul_add = cast<Function>(c);
  mul_add->setCallingConv(CallingConv::C);

  Function::arg_iterator args = mul_add->arg_begin();
  Value *x = args++;
  x->setName("x");
  Value *y = args++;
  y->setName("y");
  Value *z = args++;
  z->setName("z");

  BasicBlock *block = BasicBlock::Create(llvmContext, "entry", mul_add);
  IRBuilder<> builder(block);
  Value *tmp = builder.CreateMul(x, y, "tmp");
  Value *tmp2 = builder.CreateAdd(tmp, z, "tmp2");

  builder.CreateRet(tmp2);

  return mod;
}
int main() {
  Module *mod = makeLLVMModuleMulAdd();
  mod = makeLLVMModuleGCD(mod);
  verifyModule(*mod);

  legacy::PassManager PM;
  ModulePass *modPass = createPrintModulePass(outs());
  PM.add(modPass);
  PM.run(*mod);

  delete mod;
  return 0;
}