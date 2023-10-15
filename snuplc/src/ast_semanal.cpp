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

  // TODO (phase 3)

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

  return true;
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
  // TODO (phase 3)

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
  // TODO (phase 3)

  return true;
}


//--------------------------------------------------------------------------------------------------
// CAstStatWhile
//
bool CAstStatWhile::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)

  return true;
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

  return true;
}

const CType* CAstBinaryOp::GetType(void) const
{
  // TODO (phase 3)

  return CTypeManager::Get()->GetInteger();
}

const CDataInitializer* CAstBinaryOp::Evaluate(void) const
{
  // TODO (phase 3)

  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstUnaryOp
//
bool CAstUnaryOp::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)

  return true;
}

const CType* CAstUnaryOp::GetType(void) const
{
  // TODO (phase 3)

  return CTypeManager::Get()->GetInteger();
}

const CDataInitializer* CAstUnaryOp::Evaluate(void) const
{
  // TODO (phase 3)

  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstSpecialOp
//
bool CAstSpecialOp::TypeCheck(CToken *t, string *msg)
{
  // TODO (phase 3)

  return true;
}

const CType* CAstSpecialOp::GetType(void) const
{
  // TODO (phase 3)

  return NULL;
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

  return true;
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
  assert(_done);

  // TODO (phase 3)

  return true;
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

  return true;
}

const CType* CAstConstant::GetType(void) const
{
  return _type;
}

const CDataInitializer* CAstConstant::Evaluate(void) const
{
  // TODO (phase 3)

  return NULL;
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

  return NULL;
}

const CDataInitializer* CAstStringConstant::Evaluate(void) const
{
  // TODO (phase 3)

  return NULL;
}

