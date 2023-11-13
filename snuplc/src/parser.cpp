//--------------------------------------------------------------------------------------------------
/// @brief SnuPL parser
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/11/04 Bernhard Egger maintain unary '+' signs in the AST
/// 2016/04/01 Bernhard Egger adapted to SnuPL/1 (this is not a joke)
/// 2019/09/15 Bernhard Egger added support for constant expressions
/// 2020/07/31 Bernhard Egger adapted to SnuPL/2
/// 2020/09/27 Bernhard Egger assignment 2: parser for SnuPL/-1
///
/// @section license_section License
/// Copyright (c) 2012-2023, Computer Systems and Platforms Laboratory, SNU
/// All rights reserved.
///
/// Redistribution and use in source and binary forms, with or without
/// modification, are permitted provided that the following conditions are met:
///
/// - Redistributions of source code must retain the above copyright notice,
/// this list of condi-
///   tions and the following disclaimer.
/// - Redistributions in binary form must reproduce the above copyright notice,
/// this list of condi-
///   tions and the following disclaimer in the documentation and/or other
///   materials provided with the distribution.
///
/// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
/// AND ANY EXPRESS OR IMPLIED WARRANTIES,  INCLUDING, BUT NOT LIMITED TO,  THE
/// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
/// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
/// LIABLE FOR ANY DIRECT,  INDIRECT,  INCIDENTAL,  SPECIAL,  EXEMPLARY, OR
/// CONSE- QUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,  PROCUREMENT OF
/// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,  OR PROFITS;  OR BUSINESS
/// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
/// CONTRACT, STRICT LIABILITY, OR TORT  (INCLUDING NEGLIGENCE OR OTHERWISE)
/// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
/// POSSIBILITY OF SUCH DAMAGE.
//--------------------------------------------------------------------------------------------------

#include "parser.h"

#include <errno.h>
#include <limits.h>

#include <cassert>
#include <cstdlib>
#include <exception>
#include <iostream>
#include <vector>
using namespace std;

//--------------------------------------------------------------------------------------------------
// EBNF of SnuPL/-2
// module            = "module" ident ";"
//                     { constDeclaration | varDeclaration | subroutineDecl }
//                     [ "begin" statSequence ] "end" ident ".".

// letter            = "A".."Z" | "a".."z" | "_".
// digit             = "0".."9".
// hexdigit          = digit | "A".."F" | "a".."f".
// character         = LATIN1_char | "\n" | "\t" | "\"" | "\'" | "\\" |
// hexencoded. hexedcoded        = "\x" hexdigit hexdigit. char              =
// "'" character  "'" | "'" "\0" "'". string            = '"' { character } '"'.

// ident             = letter { letter | digit }.
// number            = digit { digit } [ "L" ].
// boolean           = "true" | "false".
// type              = basetype | type "[" [ simpleexpr ] "]".
// basetype          = "boolean" | "char" | "integer" | "longint".

// qualident         = ident { "[" simpleexpr "]" }.
// factOp            = "*" | "/" | "&&".
// termOp            = "+" | "-" | "||".
// relOp             = "=" | "#" | "<" | "<=" | ">" | ">=".

// factor            = qualident | number | boolean | char | string |
//                    "(" expression ")" | subroutineCall | "!" factor.
// term              = factor { factOp factor }.
// simpleexpr        = ["+"|"-"] term { termOp term }.
// expression        = simpleexpr [ relOp simplexpr ].

// assignment        = qualident ":=" expression.
// subroutineCall    = ident "(" [ expression {"," expression} ] ")".
// ifStatement       = "if" "(" expression ")" "then" statSequence
//                     [ "else" statSequence ] "end".
// whileStatement    = "while" "(" expression ")" "do" statSequence "end".
// returnStatement   = "return" [ expression ].

// statement         = assignment | subroutineCall | ifStatement
//                     | whileStatement | returnStatement.
// statSequence      = [ statement { ";" statement } ].

// constDeclaration  = [ "const" constDeclSequence ].
// constDeclSequence = constDecl ";" { constDecl ";" }
// constDecl         = varDecl "=" expression.

// varDeclaration    = [ "var" varDeclSequence ";" ].
// varDeclSequence   = varDecl { ";" varDecl }.
// varDecl           = ident { "," ident } ":" type.

// subroutineDecl    = (procedureDecl | functionDecl)
//                     ( "extern" | subroutineBody ident ) ";".
// procedureDecl     = "procedure" ident [ formalParam ] ";".
// functionDecl      = "function" ident [ formalParam ] ":" type ";".
// formalParam       = "(" [ varDeclSequence ] ")".
// subroutineBody    = constDeclaration varDeclaration
//                     "begin" statSequence "end".

// comment           = "//" {[^\n]} \n.
// whitespace        = { " " | \t | \n }.

//--------------------------------------------------------------------------------------------------
// CParser
//
CParser::CParser(CScanner *scanner) {
  _scanner = scanner;
  _module = NULL;
}

CAstNode *CParser::Parse(void) {
  _abort = false;

  if (_module != NULL) {
    delete _module;
    _module = NULL;
  }

  try {
    if (_scanner != NULL) _module = module();
  } catch (...) {
    _module = NULL;
  }
  return _module;
}

const CToken *CParser::GetErrorToken(void) const {
  if (_abort)
    return &_error_token;
  else
    return NULL;
}

string CParser::GetErrorMessage(void) const {
  if (_abort)
    return _message;
  else
    return "";
}

void CParser::SetError(CToken t, const string message) {
  _error_token = t;
  _message = message;
  _abort = true;
  throw message;
}

bool CParser::Consume(EToken tm, CToken *token) {
  if (_abort) return false;

  CToken t = _scanner->Get();

  if (t.GetType() != tm) {
    SetError(t,
             "expected '" + CToken::Name(tm) + "', got '" + t.GetName() + "'");
  }

  if (token != NULL) *token = t;

  return t.GetType() == tm;
}

void CParser::InitSymbolTable(CSymtab *st) {
  CTypeManager *tm = CTypeManager::Get();
  CSymbol *s;
  CSymProc *f;

  // reserved identifiers
  // such identifiers cannot be used as function/procedure/global variable names
  // 'main' is used to denote the module body in the generated assembly file
  s = new CSymbol("main", stReserved, tm->GetNull());
  st->AddSymbol(s);

  // predefined functions for I/O
  f = new CSymProc("ReadInt", tm->GetInteger(), true);
  st->AddSymbol(f);

  // TODO (phase 2)
  // add remaining predefined functions here
  f = new CSymProc("ReadLong", tm->GetLongint(), true);
  st->AddSymbol(f);

  f = new CSymProc("WriteInt", tm->GetNull(), true);
  f->AddParam(new CSymParam(0, "v", tm->GetInteger()));
  st->AddSymbol(f);

  f = new CSymProc("WriteLong", tm->GetNull(), true);
  f->AddParam(new CSymParam(0, "v", tm->GetInteger()));
  st->AddSymbol(f);

  f = new CSymProc("WriteChar", tm->GetNull(), true);
  f->AddParam(new CSymParam(0, "c", tm->GetChar()));
  st->AddSymbol(f);

  f = new CSymProc("WriteStr", tm->GetNull(), true);
  f->AddParam(new CSymParam(
      0, "string",
      tm->GetPointer(tm->GetArray(CArrayType::OPEN, tm->GetChar()))));
  st->AddSymbol(f);

  f = new CSymProc("WriteLn", tm->GetNull(), true);
  st->AddSymbol(f);

  f = new CSymProc("DIM", tm->GetInteger(), true);
  f->AddParam(new CSymParam(0, "array", tm->GetPointer(tm->GetNull())));
  f->AddParam(new CSymParam(1, "dim", tm->GetInteger()));
  st->AddSymbol(f);

  f = new CSymProc("DOFS", tm->GetInteger(), true);
  f->AddParam(new CSymParam(0, "array", tm->GetPointer(tm->GetNull())));
  st->AddSymbol(f);
}

CAstModule *CParser::module(void) {
  //
  // module ::= "module" ident ";"
  //            { constDeclaration | varDeclaration | subroutineDecl }
  //            [ "begin" statSequence ] "end" ident ".".
  //

  CToken mt, t;
  string id;
  CAstStatement *statseq = NULL;

  Consume(tModule, &mt);
  Consume(tIdent, &t);
  id = t.GetValue();
  Consume(tSemicolon);

  CAstModule *scope = new CAstModule(mt, id);
  InitSymbolTable(scope->GetSymbolTable());

  CToken t1 = _scanner->Peek();
  EToken t1_type = t1.GetType();
  while (t1_type == tConstDecl || t1_type == tVarDecl || t1_type == tFunction ||
         t1_type == tProcedure) {
    switch (t1_type) {
      case tConstDecl:
        constDeclaration(scope);
        break;
      case tVarDecl:
        varDeclaration(scope);
        break;
      case tFunction:
        procedureDecl(scope);
        break;
      case tProcedure:
        procedureDecl(scope);
        break;
      default:
        SetError(t1, "invalid declaration");
        break;
    }
    t1 = _scanner->Peek();
    t1_type = t1.GetType();
  }

  if (t1_type == tBegin) {
    Consume(tBegin);
    statseq = statSequence(scope);
    scope->SetStatementSequence(statseq);
  }

  Consume(tEnd);
  Consume(tIdent, &t);
  if (id != t.GetValue()) {
    SetError(
        t, "Module identifier mismatch (" + id + " and " + t.GetValue() + ")");
  }
  Consume(tDot);
  return scope;
}

void CParser::constDeclaration(CAstScope *scope) {
  // constDeclaration  = [ "const" constDeclSequence ].
  // constDeclSequence = constDecl ";" { constDecl ";" }
  // constDecl         = varDecl "=" expression.

  Consume(tConstDecl);
  while (_scanner->Peek().GetType() == tIdent) {
    vector<string> idents;
    CType *ty = varDecl(idents, scope);
    Consume(tRelOp);
    CAstExpression *cExp = expression(scope);
    CDataInitializer *dataInit =
        const_cast<CDataInitializer *>(cExp->Evaluate());
    if(ty->IsArray() && cExp->GetType()->IsArray()){
      CArrayType *laty = (CArrayType*) ty;
      CArrayType *raty = (CArrayType*) (cExp->GetType());
      if (!laty->Match(raty)){
        SetError(_scanner->Peek(), "Type mismatch in constant expression");}
    }
    for (auto &i : idents) {
      CSymbol *s = scope->CreateConst(i, ty, dataInit);
      const CSymbol *cc = scope->GetSymbolTable()->FindSymbol(i, sLocal);
      if (cc != NULL) {
        SetError(_scanner->Peek(), "duplicate identifier " + i);
      }
      scope->GetSymbolTable()->AddSymbol(s);
    }
    Consume(tSemicolon);
  }
}

void CParser::varDeclaration(CAstScope *scope) {
  //  varDeclaration    = [ "var" varDeclSequence ";" ].
  //  varDeclSequence   = varDecl { ";" varDecl }.
  //  varDecl           = ident { "," ident } ":" tm.

  Consume(tVarDecl);
  while (_scanner->Peek().GetType() == tIdent) {
    vector<string> idents;
    CType *ty = varDecl(idents, scope);
    for (auto &i : idents) {
      CSymbol *s = scope->CreateVar(i, ty);
      const CSymbol *cc = scope->GetSymbolTable()->FindSymbol(i, sLocal);
      if (cc != NULL) {
        SetError(_scanner->Peek(), "duplicate identifier " + i);
      }
      scope->GetSymbolTable()->AddSymbol(s);
    }
    Consume(tSemicolon);
  }
  // if (_scanner->Peek().GetType() == tSemicolon) {
  //   Consume(tSemicolon);
  //   if (_scanner->Peek().GetType() ==
  //       tIdent) {  // Since Follow(varDeclaration) = {tBegin}
  //     Consume(tVarDecl);
  //   }
  // }
  // }
}

CType *CParser::varDecl(vector<string> &idents, CAstScope *scope) {
  //  varDecl           = ident { "," ident } ":" tm.
  CToken t;
  while (_scanner->Peek().GetType() == tIdent) {
    Consume(tIdent, &t);
    idents.push_back(t.GetValue());
    while (_scanner->Peek().GetType() == tComma) {
      Consume(tComma);
      Consume(tIdent, &t);
      idents.push_back(t.GetValue());
    }
    Consume(tColon);
  }
  t = (_scanner->Get());
  EToken baseType = t.GetType();
  vector<long long> indexValues;
  while (_scanner->Peek().GetType() == tLBrak) {
    Consume(tLBrak);
    if(_scanner->Peek().GetType() == tRBrak){
      indexValues.push_back(CArrayType::OPEN);
    }else {
      CAstExpression *expr = simpleexpr(scope);
      const CType *exprType = expr->GetType();
      long long exprValue;
      if (exprType->IsLongint()) {
        const CDataInitLongint *d =
            dynamic_cast<const CDataInitLongint *>(expr->Evaluate());
        exprValue = d->GetData();
      } else if (exprType->IsInteger()) {
        const CDataInitInteger *d =
            dynamic_cast<const CDataInitInteger *>(expr->Evaluate());
        exprValue = d->GetData();
      } else {
        SetError(t, "Array index must be integer");
      }
      if(exprValue > CArrayType::MAX_SIZE){
        SetError(t, "Array index must be smaller thatn MAX_SIZE");
      }
      indexValues.push_back(exprValue);
    }
    Consume(tRBrak);
  }
  CTypeManager *tm = CTypeManager::Get();
  CType *ty;
  const CType *cty;
  switch (t.GetType()) {
    case tInteger:
      cty = CTypeManager::Get()->GetInteger();
      ty = const_cast<CType *>(cty);
      break;
    case tChar:
      cty = CTypeManager::Get()->GetChar();
      ty = const_cast<CType *>(cty);
      break;
    case tBoolean:
      cty = CTypeManager::Get()->GetBool();
      ty = const_cast<CType *>(cty);
      break;
    case tLongint:
      cty = CTypeManager::Get()->GetLongint();
      ty = const_cast<CType *>(cty);
      break;
    default:
      SetError(t, "invalid type");
      break;
  }
  while (indexValues.size() > 0) {
    long long index = indexValues.back();
    indexValues.pop_back();
    const CArrayType *cty = CTypeManager::Get()->GetArray(index, ty);
    ty = const_cast<CArrayType *>(cty);
  }
  return ty;
}

void CParser::procedureDecl(CAstScope *scope) {
  // procedureDecl     = "procedure" ident [ formalParam ] ";".
  // functionDecl      = "function" ident [ formalParam ] ":" type ";".
  // formalParam       = "(" [ varDeclSequence ] ")".
  // varDeclSequence   = varDecl { ";" varDecl }.

  CToken t = _scanner->Get();
  EToken subroutineType = t.GetType();
  CToken name = _scanner->Get();

  if (scope->GetSymbolTable()->FindSymbol(name.GetValue(), sGlobal) != NULL) {
    SetError(name,
             "duplicate procedure/function declaration " + name.GetValue());
  }
  vector<pair<vector<string>, CType *>> varDeclSequence;
  EToken eToken;

  // formalParam
  if (_scanner->Peek().GetType() == tLParens) {
    Consume(tLParens);
    while (_scanner->Peek().GetType() == tIdent) {
      vector<string> idents;
      CType *ty = varDecl(idents, scope);
      varDeclSequence.push_back(make_pair(idents, ty));
      if (_scanner->Peek().GetType() == tSemicolon) {
        Consume(tSemicolon);
      }
    }
    Consume(tRParens);
  }
  // Type
  CType *returnType;
  if (subroutineType == tFunction) {
    Consume(tColon);
    CToken t = (_scanner->Get());
    EToken baseType = t.GetType();
    vector<CAstConstant *> indexValues;
    while (_scanner->Peek().GetType() == tLBrak) {
      Consume(tLBrak);
      Consume(tInteger, &t);
      CAstConstant *c = number();
      Consume(tRBrak);
    }
    CTypeManager *tm = CTypeManager::Get();
    const CType *cty;
    switch (t.GetType()) {
      case tInteger:
        cty = CTypeManager::Get()->GetInteger();
        returnType = const_cast<CType *>(cty);
        break;
      case tChar:
        cty = CTypeManager::Get()->GetChar();
        returnType = const_cast<CType *>(cty);
        break;
      case tBoolean:
        cty = CTypeManager::Get()->GetBool();
        returnType = const_cast<CType *>(cty);
        break;
      case tLongint:
        cty = CTypeManager::Get()->GetLongint();
        returnType = const_cast<CType *>(cty);
        break;
      default:
        SetError(t, "invalid type");
        break;
    }

    while (indexValues.size() > 0) {
      CAstConstant *constExpr = indexValues.back();
      const CType *cty =
          CTypeManager::Get()->GetArray(constExpr->GetValue(), returnType);
      returnType = const_cast<CType *>(cty);
    }
  } else {
    const CType *cty = (CTypeManager::Get()->GetNull());
    returnType = const_cast<CType *>(cty);
  }
  Consume(tSemicolon);
  bool isExtern = false;
  if (_scanner->Peek().GetType() == tExtern) {
    Consume(tExtern);
    isExtern = true;
  }
  CSymProc *proc = new CSymProc(name.GetValue(), returnType, isExtern);
  scope->GetSymbolTable()->AddSymbol(proc);

  CAstProcedure *astProc = new CAstProcedure(t, name.GetValue(), scope, proc);
  // add parameters to symbol table
  for (auto &varDecl : varDeclSequence) {
    for (auto &ident : varDecl.first) {
      CSymParam *csymParam =
          new CSymParam(proc->GetNParams(), ident, varDecl.second);
      proc->AddParam(csymParam);
      astProc->GetSymbolTable()->AddSymbol(csymParam);
    }
  }

  if (!isExtern) {
    if (_scanner->Peek().GetType() == tConstDecl) {
      constDeclaration(astProc);
    }

    if (_scanner->Peek().GetType() == tVarDecl) {
      varDeclaration(astProc);
    }

    Consume(tBegin);
    CAstStatement *statseq = statSequence(astProc);
    astProc->SetStatementSequence(statseq);
    Consume(tEnd);
    CToken endid;
    Consume(tIdent, &endid);
    if (endid.GetValue() != name.GetValue()) {
      SetError(endid, "Procedure identifier mismatch (" + name.GetValue() +
                          " and " + endid.GetValue() + ")");
    }
  }
  Consume(tSemicolon);
}

CAstStatement *CParser::statSequence(CAstScope *s) {
  //
  // statSequence ::= [ statement { ";" statement } ].
  // statement ::= assignment | subroutineCall | ifStatement | whileStatement
  // | returnStatement.
  // assignment ::= qualident ":=" expression.
  // subroutineCall ::= ident "(" [ expression {"," expression} ] ")".
  // ifStatement ::= "if" "(" expression ")" "then" statSequence
  //                     [ "else" statSequence ] "end".
  // whileStatement ::= "while" "(" expression ")" "do" statSequence "end".
  // returnStatement ::= "return" [ expression ].
  // qualident ::= ident { "[" simpleexpr "]" }.

  // FIRST(statSequence) = { tIdent, tIf, tWhile, tReturn }
  // FOLLOW(statSequence) = { tEnd, tElse }
  //
  // FIRST(statement) = { tIdent, tIf, tWhile, tReturn }
  // FOLLOW(statement) = { tSemicolon, tEnd, tElse }
  //

  // The linking of statement sequences is a bit akward here because
  // we implement statSequence as a loop and not recursively.
  // We keep a 'head' that points to the first statement and is finally
  // returned at the end of the function. Head can be NULL if no statement
  // is present.
  // In the loop, we track the end of the linked list using 'tail' and
  // attach new statements to that tail.
  CAstStatement *head = NULL;

  if (_scanner->Peek().GetType() != tEnd &&
      _scanner->Peek().GetType() != tElse) {
    CAstStatement *tail = NULL;

    do {
      CAstStatement *st = NULL;

      switch (_scanner->Peek().GetType()) {
        // statement ::= assignment | subroutineCall | ifStatement |
        // whileStatement | returnStatement.
        // case tCharConst:
        //   st = assignment(s);
        //   break;
        case tIf:
          st = ifStatement(s);
          break;
        case tWhile:
          st = whileStatement(s);
          break;
        case tReturn:
          st = returnStatement(s);
          break;
        case tIdent: {
          CToken t1 = _scanner->Peek();
          ESymbolType stype;
          const CSymbol *cc =
              s->GetSymbolTable()->FindSymbol(t1.GetValue(), sGlobal);
          if (cc == NULL) SetError(t1, "undefined symbol");
          stype = cc->GetSymbolType();
          if (stype == stProcedure)
            st = subroutineCall(s);
          else
            st = assignment(s);
          break;
        }
        // case tElse:
        //   break;
        // case tEnd:
        //   break;
        default:
          SetError(_scanner->Peek(), "statement expected.");
          break;
      }
      assert(st != NULL);
      if (head == NULL)
        head = st;
      else
        tail->SetNext(st);
      tail = st;

      // if (_scanner->Peek().GetType() == tEnd) break;
      // if (_scanner->Peek().GetType() == tElse) break;
      if (_scanner->Peek().GetType() == tSemicolon) {
        Consume(tSemicolon);
      } else {
        break;
      }
    } while (!_abort);
  }
  return head;
}

CAstStatWhile *CParser::whileStatement(CAstScope *scope) {
  //
  // whileStatement ::= "while" "(" expression ")" "do" statSequence "end".
  //
  CToken t;

  Consume(tWhile, &t);
  Consume(tLParens);

  CAstExpression *cond = expression(scope);

  Consume(tRParens);
  Consume(tDo);

  CAstStatement *body = statSequence(scope);

  Consume(tEnd);

  return new CAstStatWhile(t, cond, body);
}

CAstStatIf *CParser::ifStatement(CAstScope *scope) {
  //
  // ifStatement ::= "if" "(" expression ")" "then" statSequence
  //                     [ "else" statSequence ] "end".
  //
  CToken t;

  Consume(tIf, &t);
  Consume(tLParens);

  CAstExpression *cond = expression(scope);

  Consume(tRParens);
  Consume(tThen);

  CAstStatement *ifBody = statSequence(scope);
  CAstStatement *elseBody = NULL;

  if (_scanner->Peek().GetType() == tElse) {
    Consume(tElse);
    elseBody = statSequence(scope);
  }

  Consume(tEnd);

  return new CAstStatIf(t, cond, ifBody, elseBody);
}

CAstStatReturn *CParser::returnStatement(CAstScope *scope) {
  //
  // returnStatement ::= "return" [ expression ].
  //
  CToken t;

  Consume(tReturn, &t);
  CAstExpression *expr = NULL;
  if (_scanner->Peek().GetType() != tSemicolon &&
      _scanner->Peek().GetType() != tEnd) {
    expr = expression(scope);
  }

  return new CAstStatReturn(t, scope, expr);
}

CAstStatCall *CParser::subroutineCall(CAstScope *scope) {
  //
  // Statement
  // subroutineCall ::= ident "(" [ expression {"," expression} ] ")".
  //
  CToken id = _scanner->Peek();

  return new CAstStatCall(id, functionCall(scope));
}
CAstFunctionCall *CParser::functionCall(CAstScope *scope) {
  //
  // Expression
  // subroutineCall ::= ident "(" [ expression {"," expression} ] ")".
  //
  CToken id;
  Consume(tIdent, &id);
  Consume(tLParens);

  CAstFunctionCall *fc = new CAstFunctionCall(
      id,
      (CSymProc *)scope->GetSymbolTable()->FindSymbol(id.GetValue(), sGlobal));
  while (_scanner->Peek().GetType() != tRParens) {
    CAstExpression *expr = expression(scope);
    fc->AddArg(expr);
    if (_scanner->Peek().GetType() == tComma) {
      Consume(tComma);
    }
  }
  Consume(tRParens);
  return fc;
}
CAstStatAssign *CParser::assignment(CAstScope *s) {
  //
  // assignment ::= qualident ":=" expression.
  //
  CToken t;

  CAstDesignator *lhs = qualident(s);
  Consume(tAssign, &t);

  CAstExpression *rhs = expression(s);
  return new CAstStatAssign(t, lhs, rhs);
}

CAstExpression *CParser::expression(CAstScope *s) {
  //
  // expression ::= simpleexpr [ relOp simpleexpr ].
  //
  CToken t;
  EOperation relop;
  CAstExpression *left = NULL, *right = NULL;

  left = simpleexpr(s);

  if (_scanner->Peek().GetType() == tRelOp) {
    Consume(tRelOp, &t);
    right = simpleexpr(s);

    if (t.GetValue() == "=")
      relop = opEqual;
    else if (t.GetValue() == "#")
      relop = opNotEqual;
    else if (t.GetValue() == "<")
      relop = opLessThan;
    else if (t.GetValue() == ">")
      relop = opBiggerThan;
    else if (t.GetValue() == "<=")
      relop = opLessEqual;
    else if (t.GetValue() == ">=")
      relop = opBiggerEqual;
    else
      SetError(t, "invalid relation.");

    return new CAstBinaryOp(t, relop, left, right);
  } else {
    return left;
  }
}

CAstExpression *CParser::simpleexpr(CAstScope *scope) {
  //
  // simpleexpr ::= ["+"|"-"] term { termOp term }.
  //
  CAstExpression *n = NULL;
  CToken t1;
  if (_scanner->Peek().GetType() == tPlusMinus) {
    CAstUnaryOp *tmp = unaryOp(scope);
    EOperation op = tmp->GetOperation();
    CAstExpression *e = tmp->GetOperand();
    if(op == opNeg){
      string *msg;
      bool res = tmp->TypeCheck(&t1, msg);
      if(!res) SetError(tmp->GetToken(), *msg);
      while(dynamic_cast<CAstBinaryOp*>(e) != NULL && e->GetParenthesized() == false){
        try{
          e = dynamic_cast<CAstBinaryOp*>(e)->GetLeft();
        } catch(...){
          break;
        }
      }
      if(dynamic_cast<CAstConstant *>(e) != NULL) n = e;
      else n = tmp;
    }
  } else {
    n = term(scope);
  }

  while (_scanner->Peek().GetType() == tPlusMinus ||
         _scanner->Peek().GetType() == tOr) {
    CToken t;
    CAstExpression *l = n, *r;

    if (_scanner->Peek().GetType() == tPlusMinus)
      Consume(tPlusMinus, &t);
    else
      Consume(tOr, &t);

    r = term(scope);

    EOperation op;
    if (t.GetValue() == "+")
      op = opAdd;  // "+""
    else if (t.GetValue() == "-")
      op = opSub;  // "-"
    else if (t.GetValue() == "||")
      op = opOr;  // "||"
    else
      SetError(t, "invalid term operator.");

    n = new CAstBinaryOp(t, op, l, r);
  }

  return n;
}

CAstExpression *CParser::term(CAstScope *s) {
  //
  // term ::= factor { ("*"|"/" | "&&") factor }.
  //
  CAstExpression *n = NULL;

  n = factor(s);

  EToken tt = _scanner->Peek().GetType();

  while (tt == tMulDiv || tt == tAnd) {
    CToken t;
    CAstExpression *l = n, *r;

    if (tt == tMulDiv)
      Consume(tMulDiv, &t);
    else
      Consume(tAnd, &t);
    r = factor(s);
    if (t.GetValue() == "&&") {
      n = new CAstBinaryOp(t, opAnd, l, r);
    } else {
      n = new CAstBinaryOp(t, t.GetValue() == "*" ? opMul : opDiv, l, r);
    }
    tt = _scanner->Peek().GetType();
  }

  return n;
}

CAstExpression *CParser::factor(CAstScope *s) {
  //
  // factor ::= qualident | number | boolean | char | string |
  //            "(" expression ")" | subroutineCall | "!" factor.

  //
  // FIRST(factor) = { tDigit, tLBrak }
  //

  CToken t;
  CAstExpression *n = NULL;

  switch (_scanner->Peek().GetType()) {
    // factor ::= number
    case tNumber:
      n = number();
      break;
    case tBoolConst:
      n = boolConst();
      break;
    case tCharConst:
      n = charConst();
      break;
    case tStringConst:
      n = stringConst(s);
      break;
    // factor ::= "(" expression ")"
    case tLParens:
      Consume(tLParens);
      n = expression(s);
      n->SetParenthesized(true);
      Consume(tRParens);
      break;
    case tNot:
      Consume(tNot, &t);
      n = factor(s);
      n = new CAstUnaryOp(t, opNot, n);
      break;
    case tIdent: {
      // qualident | subroutineCall
      CToken t1 = _scanner->Peek();
      ESymbolType stype;
      
     const CSymbol *cc =
          s->GetSymbolTable()->FindSymbol(t1.GetValue(), sGlobal);
      if (cc == NULL) SetError(t1, "undefined symbol");
      stype = cc->GetSymbolType();
      if (stype == stProcedure) {
        n = functionCall(s);
      } else if (stype == stConstant) {
        n = qualident(s);
        if (n->GetType()->IsInteger()) {
          n = new CAstConstant(t, CTypeManager::Get()->GetInteger(),
                               ((CDataInitInteger *)n->Evaluate())->GetData());
        } else if (n->GetType()->IsLongint()) {
          n = new CAstConstant(t, CTypeManager::Get()->GetLongint(),
                               ((CDataInitLongint *)n->Evaluate())->GetData());
        } else if (n->GetType()->IsChar()) {
          n = new CAstConstant(t, CTypeManager::Get()->GetChar(),
                               ((CDataInitChar *)n->Evaluate())->GetData());
        } else if (n->GetType()->IsBoolean()) {
          n = new CAstConstant(t, CTypeManager::Get()->GetBool(),
                               ((CDataInitBoolean *)n->Evaluate())->GetData());
        }
        // case: string
      } else {
        n = qualident(s);
      }
      break;
    }
    default:
      // Error
      SetError(_scanner->Peek(), "factor expected.");
      break;
  }

  return n;
}
CAstDesignator *CParser::qualident(CAstScope *scope) {
  //
  // qualident ::= ident { "[" expression "]" }.
  //
  CToken t;
  Consume(tIdent, &t);
  const CSymbol *s = scope->GetSymbolTable()->FindSymbol(t.GetValue(), sGlobal);
  if(s == NULL) SetError(t, "undefined symbol");
  if(_scanner->Peek().GetType() == tLParens){
    SetError(t, "invalid procedure/function identifier " + t.GetValue());
  }
  if (_scanner->Peek().GetType() == tLBrak) {
    CAstArrayDesignator *d = new CAstArrayDesignator(t, s);
    while (_scanner->Peek().GetType() == tLBrak) {
      Consume(tLBrak);
      CAstExpression *e = expression(scope);
      if (!e->GetType()->IsInt()) SetError(t, "invalid array index: not int");
      // long long index;
      // if (e->GetType()->IsInteger()) {
      //   index = ((CDataInitInteger *)e->Evaluate())->GetData();
      // } else if (e->GetType()->IsLongint()) {
      //   index = ((CDataInitLongint *)e->Evaluate())->GetData();
      // }
      // if (index < 0) SetError(t, "invalid array index: negative value");
      Consume(tRBrak);
      d->AddIndex(e);
    }
    return d;
  }
  CAstDesignator *d = new CAstDesignator(t, s);
  return d;
}
CAstConstant *CParser::boolConst(void) {
  //
  // boolean ::= "true" | "false".
  //
  CToken t;

  Consume(tBoolConst, &t);

  return new CAstConstant(t, CTypeManager::Get()->GetBool(),
                          t.GetValue() == "true" ? 1 : 0);
}
CAstStringConstant *CParser::stringConst(CAstScope *scope) {
  //
  // string ::= '"' { character } '"'.
  //
  CToken t;

  Consume(tStringConst, &t);

  // while (scope->GetParent() != NULL) {
  //   scope = scope->GetParent();
  // }
  return new CAstStringConstant(t, t.GetValue(), scope);
}
CAstConstant *CParser::charConst(void) {
  //
  // char ::= "'" character "'".
  //
  CToken t;

  Consume(tCharConst, &t);

  if (t.GetValue().size() == 1)
    return new CAstConstant(t, CTypeManager::Get()->GetChar(), t.GetValue()[0]);
  else {
    string s = t.GetValue();
    if (s[1] == 'n')
      return new CAstConstant(t, CTypeManager::Get()->GetChar(), '\n');
    else if (s[1] == 't')
      return new CAstConstant(t, CTypeManager::Get()->GetChar(), '\t');
    else if (s[1] == '0')
      return new CAstConstant(t, CTypeManager::Get()->GetChar(), '\0');
    else if (s[1] == '\\')
      return new CAstConstant(t, CTypeManager::Get()->GetChar(), '\\');
    else if (s[1] == '\'')
      return new CAstConstant(t, CTypeManager::Get()->GetChar(), '\'');
    else
      SetError(t, "invalid character");
  }
}
CAstConstant *CParser::number(void) {
  //
  // number ::= "0".."9".
  //

  CToken t;

  Consume(tNumber, &t);

  errno = 0;
  long long v = strtoll(t.GetValue().c_str(), NULL, 10);
  if (errno != 0) SetError(t, "invalid number.");

  return new CAstConstant(t, CTypeManager::Get()->GetInteger(), v);
}

CAstUnaryOp *CParser::unaryOp(CAstScope *s) {
  //
  // unary ::= "+"|"-" factor.
  //
  CToken t;
  EOperation op;

  Consume(tPlusMinus, &t);

  if (t.GetValue() == "+")
    op = opPos;
  else if (t.GetValue() == "-")
    op = opNeg;
  else if (t.GetValue() == "!")
    op = opNot;
  else
    SetError(t, "invalid unary operator.");
  CAstExpression *e = term(s);
  // cerr << t.GetValue() << endl;
  // cerr << e->GetType() << endl;
  return new CAstUnaryOp(t, op, e);
}

//
// CAstDesignator *CParser::letter(CAstScope *s) {
//   //
//   // letter := "a".."z".
//   //

//   CToken t;
//   CSymtab *st = s->GetSymbolTable();

//   Consume(tLetter, &t);

//   // check if symbol exists in (local) symbol table
//   const CSymbol *sym = st->FindSymbol(t.GetValue(), sLocal);

//   if (sym == NULL) {
//     // if not, create one and add it to the symbol table
//     CSymbol *nsym =
//         s->CreateVar(t.GetValue(), CTypeManager::Get()->GetInteger());
//     st->AddSymbol(nsym);

//     sym = nsym;
//   }

//   return new CAstDesignator(t, sym);
// }
