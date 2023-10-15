//--------------------------------------------------------------------------------------------------
/// @brief SnuPL abstract syntax tree (methods related to three-address-code generation)
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2023/09/26 Bernhard Egger created (move TAC generation code from ast.cpp to this file)
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


//------------------------------------------------------------------------------
// CAstScope
//
CTacAddr* CAstScope::ToTac(CCodeBlock *cb)
{
  // TODO (phase 4)

  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstModule
//


//--------------------------------------------------------------------------------------------------
// CAstProcedure
//


//--------------------------------------------------------------------------------------------------
// CAstType
//


//--------------------------------------------------------------------------------------------------
// CAstStatement
//
CTacAddr* CAstStatement::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  assert(false);
  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstStatAssign
//
CTacAddr* CAstStatAssign::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  // TODO (phase 4)

  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstStatCall
//
CTacAddr* CAstStatCall::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  // TODO (phase 4)

  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstStatReturn
//
CTacAddr* CAstStatReturn::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  // TODO (phase 4)

  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstStatIf
//
CTacAddr* CAstStatIf::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  // TODO (phase 4)

  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstStatWhile
//
CTacAddr* CAstStatWhile::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  // TODO (phase 4)

  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstExpression
//
CTacAddr* CAstExpression::ToTac(CCodeBlock *cb)
{
  return NULL;
}

CTacAddr* CAstExpression::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstOperation
//


//--------------------------------------------------------------------------------------------------
// CAstBinaryOp
//
CTacAddr* CAstBinaryOp::ToTac(CCodeBlock *cb)
{
  // TODO (phase 4)

  return NULL;
}

CTacAddr* CAstBinaryOp::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
  // TODO (phase 4)

  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstUnaryOp
//
CTacAddr* CAstUnaryOp::ToTac(CCodeBlock *cb)
{
  // TODO (phase 4)

  return NULL;
}

CTacAddr* CAstUnaryOp::ToTac(CCodeBlock *cb,
                             CTacLabel *ltrue, CTacLabel *lfalse)
{
  // TODO (phase 4)

  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstSpecialOp
//
CTacAddr* CAstSpecialOp::ToTac(CCodeBlock *cb)
{
  // TODO (phase 4)

  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstFunctionCall
//
CTacAddr* CAstFunctionCall::ToTac(CCodeBlock *cb)
{
  // TODO (phase 4)

  return NULL;
}

CTacAddr* CAstFunctionCall::ToTac(CCodeBlock *cb,
                                  CTacLabel *ltrue, CTacLabel *lfalse)
{
  // TODO (phase 4)

  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstOperand
//


//--------------------------------------------------------------------------------------------------
// CAstDesignator
//
CTacAddr* CAstDesignator::ToTac(CCodeBlock *cb)
{
  // TODO (phase 4)

  return NULL;
}

CTacAddr* CAstDesignator::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  // TODO (phase 4)

  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstArrayDesignator
//
CTacAddr* CAstArrayDesignator::ToTac(CCodeBlock *cb)
{
  // TODO (phase 4)

  return NULL;
}

CTacAddr* CAstArrayDesignator::ToTac(CCodeBlock *cb,
                                     CTacLabel *ltrue, CTacLabel *lfalse)
{
  // TODO (phase 4)

  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstConstant
//
CTacAddr* CAstConstant::ToTac(CCodeBlock *cb)
{
  // TODO (phase 4)

  return NULL;
}
CTacAddr* CAstConstant::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  // TODO (phase 4)

  return NULL;
}


//--------------------------------------------------------------------------------------------------
// CAstStringConstant
//
CTacAddr* CAstStringConstant::ToTac(CCodeBlock *cb)
{
  // TODO (phase 4)

  return NULL;
}

CTacAddr* CAstStringConstant::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  // TODO (phase 4)

  return NULL;
}
