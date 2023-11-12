//--------------------------------------------------------------------------------------------------
/// @brief SnuPL abstract syntax tree (methods related to semantic analysis)
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2023/09/26 Bernhard Egger created (move semantic analysis code from ast.cpp to this file)
///
/// @section license_section License
/// Copyright (c) 2012-2023, Computer Systems and Platforms Laboratory, SNU
/// All rights reserved.
///
/// Redistribution and use in source and binary forms, with or without modification, are permitted
/// provided that the following conditions are met:
///
/// - Redistributions of source code must retain the above copyright notice, this list of condi-
///   tions and the following disclaimer.
/// - Redistributions in binary form must reproduce the above copyright notice, this list of condi-
///   tions and the following disclaimer in the documentation and/or other materials provided with
///   the distribution.
///
/// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
/// IMPLIED WARRANTIES,  INCLUDING, BUT NOT LIMITED TO,  THE IMPLIED WARRANTIES OF MERCHANTABILITY
/// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
/// CONTRIBUTORS BE LIABLE FOR ANY DIRECT,  INDIRECT,  INCIDENTAL,  SPECIAL,  EXEMPLARY, OR CONSE-
/// QUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
/// LOSS OF USE, DATA,  OR PROFITS;  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
/// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT  (INCLUDING NEGLIGENCE OR OTHERWISE)
/// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
/// DAMAGE.
//--------------------------------------------------------------------------------------------------

#include <iostream>
#include <cassert>
#include <cstring>

#include <typeinfo>

#include "ast.h"
using namespace std;


//--------------------------------------------------------------------------------------------------
// CAstNode
//
const CType* CAstNode::GetType(void) const
{
  return CTypeManager::Get()->GetNull();
}


//------------------------------------------------------------------------------
// CAstScope
//
bool CAstScope::TypeCheck(CToken *t, string *msg) const
{
  bool result = true;

  try {
    CAstStatement *s = _statseq;
    while (result && (s != NULL)) {
      result = s->TypeCheck(t, msg);
      s = s->GetNext();
    }

    vector<CAstScope*>::const_iterator it = _children.begin();
    while (result && (it != _children.end())) {
      result = (*it)->TypeCheck(t, msg);
      it++;
    }
  } catch (int e) {
    result = false;
  }

  return result;
}


//--------------------------------------------------------------------------------------------------
// CAstModule
//


//--------------------------------------------------------------------------------------------------
// CAstProcedure
//
const CType* CAstProcedure::GetType(void) const
{
  return GetSymbol()->GetDataType();
}


//--------------------------------------------------------------------------------------------------
// CAstType
//
const CType* CAstType::GetType(void) const
{
  return _type;
}


//--------------------------------------------------------------------------------------------------
// CAstStatement
//


//--------------------------------------------------------------------------------------------------
// CAstStatAssign
//
bool CAstStatAssign::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)
  bool result = _lhs->TypeCheck(t, msg) && _rhs->TypeCheck(t, msg);
  
  result = result && _lhs->GetType()->Match((_rhs->GetType()));

  return result;
}

const CType* CAstStatAssign::GetType(void) const
{
  return _lhs->GetType();
}


//--------------------------------------------------------------------------------------------------
// CAstStatCall
//
bool CAstStatCall::TypeCheck(CToken *t, string *msg)
{
  return GetCall()->TypeCheck(t, msg);
}


//--------------------------------------------------------------------------------------------------
// CAstStatReturn
//
bool CAstStatReturn::TypeCheck(CToken *t, string *msg) 
{
  const CType *st = GetScope()->GetType();
  CAstExpression *e = GetExpression();

  if (st->Match(CTypeManager::Get()->GetNull())) {
    if (e != NULL) {
      if (t != NULL) *t = e->GetToken();
      if (msg != NULL) *msg = "superfluous expression after return.";
      return false;
    }
  } else {
    if (e == NULL) {
      if (t != NULL) *t = GetToken();
      if (msg != NULL) *msg = "expression expected after return.";
      return false;
    }

    if (!e->TypeCheck(t, msg)) return false;

    if (!st->Match(e->GetType())) {
      if (t != NULL) *t = e->GetToken();
      if (msg != NULL) *msg = "return type mismatch.";
      return false;
    }
  }

  return true;
}

const CType* CAstStatReturn::GetType(void) const
{
  const CType *t = NULL;

  if (GetExpression() != NULL) {
    t = GetExpression()->GetType();
  } else {
    t = CTypeManager::Get()->GetNull();
  }

  return t;
}


//--------------------------------------------------------------------------------------------------
// CAstStatIf
//
bool CAstStatIf::TypeCheck(CToken *t, string *msg)
{
  return GetCondition()->TypeCheck(t, msg) && GetIfBody()->TypeCheck(t, msg) && GetElseBody()->TypeCheck(t, msg);
}


//--------------------------------------------------------------------------------------------------
// CAstStatWhile
//
bool CAstStatWhile::TypeCheck(CToken *t, string *msg)
{
  return GetCondition()->TypeCheck(t, msg) && GetBody()->TypeCheck(t, msg);
}


//--------------------------------------------------------------------------------------------------
// CAstExpression
//
const CDataInitializer* CAstExpression::Evaluate(void) const
{
  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstOperation
//


//--------------------------------------------------------------------------------------------------
// CAstBinaryOp
//
bool CAstBinaryOp::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)
  const CType *tt = GetType();
  if(tt == NULL){
    if(t != NULL) *t = GetToken();
    if(msg != NULL) *msg = "type mismatch.";
    return false;
  }
  return true;
}

const CType* CAstBinaryOp::GetType(void) const
{
  // TODO (phase 3)
  // expression ::= simpleexpr [ relOp simpleexpr ]. op : =, #, <, <=, >, >=
  // simpleexpr ::= ["+"|"-"] term { termOp term }. op : +, -, ||
  // term ::= factor { ("*"|"/" | "&&") factor }. op : *, /, &&
  const CType *t = NULL;
  const CType *lt = GetLeft()->GetType();
  const CType *rt = GetRight()->GetType();
  
  const EOperation op = GetOperation();
  
  if(op == opEqual || op == opNotEqual || op == opLessThan || op == opLessEqual || op == opBiggerThan || op == opBiggerEqual) {
    if(lt->IsBoolean() && rt->IsBoolean()) {
      if(op == opEqual || op == opNotEqual) t = CTypeManager::Get()->GetBool();
      else t = NULL;
    } else if(lt->IsInteger() && rt->IsInteger()) {
      t = CTypeManager::Get()->GetBool();
    } else if(lt->IsLongint() && rt->IsLongint()) {
      t = CTypeManager::Get()->GetBool();
    } else if (lt->IsLongint() && rt->IsInteger()){
      t = CTypeManager::Get()->GetBool();
    } else if (lt->IsInteger() && rt->IsLongint()){
      t = CTypeManager::Get()->GetBool();
    }  else if (lt->IsChar() && rt->IsChar()) {
      t = CTypeManager::Get()->GetBool();
    } else {
      t = NULL;
    }
  } else if(op == opAdd || op == opSub || op == opMul || op == opDiv) {
    if(lt->IsInteger() || rt->IsInteger()){
      t = CTypeManager::Get()->GetInteger();
    } else if(lt->IsLongint() && rt->IsLongint()) {
      t = CTypeManager::Get()->GetLongint();
    } else if (lt->IsLongint() && rt->IsInteger()){
      t = CTypeManager::Get()->GetLongint();
    } else if (lt->IsInteger() && rt->IsLongint()){
      t = CTypeManager::Get()->GetLongint();
    } else{
      t = NULL;
    }
  } else if(op == opAnd || op == opOr) {
    if(lt->IsBoolean() && rt->IsBoolean()) {
      t = CTypeManager::Get()->GetBool();
    } else {
      t = NULL;
    }
  } else {
    t = NULL;
  }

  return t;
}

const CDataInitializer* CAstBinaryOp::Evaluate(void) const
{
  // TODO (phase 3)
  const CType *t = GetLeft()->GetType();
  const CType *t2 = GetRight()->GetType();
  const CDataInitializer *l = GetLeft()->Evaluate();
  const CDataInitializer *r = GetRight()->Evaluate();
  CDataInitializer *result = NULL;
  const EOperation op = GetOperation();

  if(t->IsBoolean()){
    bool lval = dynamic_cast<const CDataInitBoolean*>(l)->GetData();
    bool rval = dynamic_cast<const CDataInitBoolean*>(r)->GetData();
    if(op == opEqual){
      result = new CDataInitBoolean(lval == rval);
    }else if(op == opNotEqual){
      result = new CDataInitBoolean(lval != rval);
    }
  } else if(t->IsLongint() || t2->IsLongint()){
    long long lval;
    long long rval;
    if(t->IsLongint()) lval = dynamic_cast<const CDataInitLongint*>(l)->GetData();
    else lval = dynamic_cast<const CDataInitLongint*>(r)->GetData();

    if(t2->IsLongint()) rval = dynamic_cast<const CDataInitLongint*>(l)->GetData();
    else rval = dynamic_cast<const CDataInitLongint*>(r)->GetData();
    if(op == opEqual){
      result = new CDataInitBoolean(lval == rval);
    }else if(op == opNotEqual){
      result = new CDataInitBoolean(lval != rval);
    } else if(op == opLessThan){
      result = new CDataInitBoolean(lval < rval);
    } else if(op == opLessEqual){
      result = new CDataInitBoolean(lval <= rval);
    } else if(op == opBiggerThan){
      result = new CDataInitBoolean(lval > rval);
    } else if(op == opBiggerEqual){
      result = new CDataInitBoolean(lval >= rval);
    } else if(op == opAdd){
      result = new CDataInitLongint(lval + rval);
    }else if(op == opSub){
      result = new CDataInitLongint(lval - rval);
    }else if(op == opMul){
      result = new CDataInitLongint(lval * rval);
    }else if(op == opDiv){
      result = new CDataInitLongint(lval / rval);
    }
  } else if(t->IsInteger()){
    int lval = dynamic_cast<const CDataInitInteger*>(l)->GetData();
    int rval = dynamic_cast<const CDataInitInteger*>(r)->GetData();
    if(op == opEqual){
      result = new CDataInitBoolean(lval == rval);
    }else if(op == opNotEqual){
      result = new CDataInitBoolean(lval != rval);
    } else if(op == opLessThan){
      result = new CDataInitBoolean(lval < rval);
    } else if(op == opLessEqual){
      result = new CDataInitBoolean(lval <= rval);
    } else if(op == opBiggerThan){
      result = new CDataInitBoolean(lval > rval);
    } else if(op == opBiggerEqual){
      result = new CDataInitBoolean(lval >= rval);
    } else if(op == opAdd){
      result = new CDataInitInteger(lval + rval);
    }else if(op == opSub){
      result = new CDataInitInteger(lval - rval);
    }else if(op == opMul){
      result = new CDataInitInteger(lval * rval);
    }else if(op == opDiv){
      result = new CDataInitInteger(lval / rval);
    }
  } else if(t->IsChar()){
    char lval = dynamic_cast<const CDataInitChar*>(l)->GetData();
    char rval = dynamic_cast<const CDataInitChar*>(r)->GetData();
    if(op == opEqual){
      result = new CDataInitBoolean(lval == rval);
    }else if(op == opNotEqual){
      result = new CDataInitBoolean(lval != rval);
    } else if(op == opLessThan){
      result = new CDataInitBoolean(lval < rval);
    } else if(op == opLessEqual){
      result = new CDataInitBoolean(lval <= rval);
    } else if(op == opBiggerThan){
      result = new CDataInitBoolean(lval > rval);
    } else if(op == opBiggerEqual){
      result = new CDataInitBoolean(lval >= rval);
    } 
  }


  return result;
}


//--------------------------------------------------------------------------------------------------
// CAstUnaryOp
//
bool CAstUnaryOp::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)
  EOperation op = GetOperation();
  if(op != opNeg) return GetOperand()->TypeCheck(t, msg);
  CAstExpression *e = GetOperand();
  while(1){
    try{
      e = dynamic_cast<CAstBinaryOp*>(e)->GetLeft();
      if(e->GetParenthesized()) break;
    } catch(...){
      break;
    }
  }
  if(dynamic_cast<CAstConstant*>(e) == NULL) return GetOperand()->TypeCheck(t, msg);
  else if(dynamic_cast<CAstConstant*>(e)->GetType()->IsInteger() || dynamic_cast<CAstConstant*>(e)->GetType()->IsLongint()){
    dynamic_cast<CAstConstant*>(e)->FoldNeg();
    return dynamic_cast<CAstConstant*>(e)->TypeCheck(t, msg);
  }
  *t = GetToken();
  *msg = "negated expression must be integer or longint.";
  return false;
}

const CType* CAstUnaryOp::GetType(void) const
{
  // TODO (phase 3)
  return GetOperand()->GetType();
}

const CDataInitializer* CAstUnaryOp::Evaluate(void) const
{
  // TODO (phase 3)
  CAstExpression *e = GetOperand();
  EOperation op = GetOperation();
  if(op == opPos) return e->Evaluate();
  else if(op == opNeg){
    while(1){
      try{
        e = dynamic_cast<CAstBinaryOp*>(e)->GetLeft();
        if(e->GetParenthesized()) break;
      } catch(...){
        break;
      }
    }
    if(e->GetParenthesized()){
      int val = dynamic_cast<const CDataInitInteger*>(GetOperand()->Evaluate())->GetData();
      return new CDataInitInteger(-val);
    } else return GetOperand()->Evaluate();
  } else if(op == opNot){
    while(1){
      try{
        e = dynamic_cast<CAstBinaryOp*>(e)->GetLeft();
        if(e->GetParenthesized()) break;
      } catch(...){
        break;
      }
    }
    if(e->GetParenthesized()){
      bool val = dynamic_cast<const CDataInitBoolean*>(GetOperand()->Evaluate())->GetData();
      return new CDataInitBoolean(!val);
    } else return GetOperand()->Evaluate();
  }
  return NULL;
}

//--------------------------------------------------------------------------------------------------
// CAstSpecialOp
//
bool CAstSpecialOp::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)

  return GetOperand()->TypeCheck(t, msg);
}

const CType* CAstSpecialOp::GetType(void) const
{
  CType *t = NULL;
  // TODO (phase 3)
  if(GetOperation() == opAddress){
    t = const_cast<CPointerType *>(CTypeManager::Get()->GetPointer(GetOperand()->GetType()));
  }else if(GetOperation() == opDeref){
    CPointerType *pt = (CPointerType*)GetOperand()->GetType();
    t = const_cast<CType *>(pt -> GetBaseType());
  }else if(GetOperation() == opCast || GetOperation() == opWiden || GetOperation() == opNarrow){
    t = const_cast<CType *>(_type);
  }
  return t;
}

const CDataInitializer* CAstSpecialOp::Evaluate(void) const
{
  // TODO (phase 3)

  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstFunctionCall
//
bool CAstFunctionCall::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)
  bool result = true;
  vector<CAstExpression*>::iterator iter  =_arg.begin();
  while(iter != _arg.end()){
    result = result && (*iter)->TypeCheck(t, msg);
    iter++;
  }

  return false;
}

const CType* CAstFunctionCall::GetType(void) const
{
  return GetSymbol()->GetDataType();
}


//--------------------------------------------------------------------------------------------------
// CAstOperand
//


//--------------------------------------------------------------------------------------------------
// CAstDesignator
//
bool CAstDesignator::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)

  return true;
}

const CType* CAstDesignator::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

const CDataInitializer* CAstDesignator::Evaluate(void) const
{
  // TODO (phase 3)

  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstArrayDesignator
//
bool CAstArrayDesignator::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)
  bool result = true;
  vector<CAstExpression*>::iterator iter =  _idx.begin();
  while(iter != _idx.end()){
    result = result && (*iter)->TypeCheck(t, msg);
    iter++;
  }
  // What about offset?
  return result;
}

const CType* CAstArrayDesignator::GetType(void) const
{
  // TODO (phase 3)

  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstConstant
//
bool CAstConstant::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)
  if(_type->IsInteger()){
    if(_value > INT_MAX  || _value < INT_MIN){
      if(_value == INT_MIN && _negated == true) return true;
      if(t != NULL) *t = GetToken();
      if(msg != NULL) *msg = "integer constant out of range.";
      return false;
    }
  } else if(_type->IsBoolean()){
    if(_value != 0 && _value != 1){
      if(t != NULL) *t = GetToken();
      if(msg != NULL) *msg = "boolean constant out of range.";
      return false;
    }
  } else if(_type->IsChar()){
    if(_value > CHAR_MAX || _value < CHAR_MIN){
      if(t != NULL) *t = GetToken();
      if(msg != NULL) *msg = "char constant out of range.";
      return false;
    }
  } else if(_type->IsLongint()){
    if(_value > LONG_MAX || _value < LONG_MIN){
      if(_value == LONG_MIN && _negated == true) return true;
      if(t != NULL) *t = GetToken();
      if(msg != NULL) *msg = "longint constant out of range.";
      return false;
    }
  }
  return true;
}

const CType* CAstConstant::GetType(void) const
{
  return _type;
}

const CDataInitializer* CAstConstant::Evaluate(void) const
{
  // TODO (phase 3)
  if (_type->IsLongint())  return new CDataInitLongint((long long)GetValue());
  if (_type->IsInteger())  return new CDataInitInteger((int)GetValue());
  if (_type->IsBoolean())  return new CDataInitBoolean((bool)GetValue());
  if (_type->IsChar())     return new CDataInitChar((char)GetValue());
  // return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstStringConstant
//
bool CAstStringConstant::TypeCheck(CToken *t, string *msg)
{
  return true;
}

const CType* CAstStringConstant::GetType(void) const
{
  // TODO (phase 3)
  // done?
  return _type;
}

const CDataInitializer* CAstStringConstant::Evaluate(void) const
{
  // TODO (phase 3)

  return _value;
}

