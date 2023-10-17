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

#include <limits.h>
#include <cassert>
#include <errno.h>
#include <cstdlib>
#include <vector>
#include <iostream>
#include <exception>

#include "parser.h"
using namespace std;

//--------------------------------------------------------------------------------------------------
// EBNF of SnuPL/-1
//   module        =  statSequence "."
//   digit         =  "0".."9".
//   letter        =  "a".."z".
//   factOp        =  "*" | "/".
//   termOp        =  "+" | "-".
//   relOp         =  "=" | "#".
//   factor        =  digit | "(" expression ")".
//   term          =  factor { factOp factor }.
//   simpleexpr    =  term { termOp term }.
//   expression    =  simpleexpr [ relOp simpleexpr ].
//   assignment    =  letter ":=" expression.
//   statement     =  assignment.
//   statSequence  =  [ statement { ";" statement } ].
//   whitespace    =  { " " | \n }+.

//--------------------------------------------------------------------------------------------------
// CParser
//
CParser::CParser(CScanner *scanner)
{
  _scanner = scanner;
  _module = NULL;
}

CAstNode* CParser::Parse(void)
{
  _abort = false;

  if (_module != NULL) { delete _module; _module = NULL; }

  try {
    if (_scanner != NULL) _module = module();
  } catch (...) {
    _module = NULL;
  }

  return _module;
}

const CToken* CParser::GetErrorToken(void) const
{
  if (_abort) return &_error_token;
  else return NULL;
}

string CParser::GetErrorMessage(void) const
{
  if (_abort) return _message;
  else return "";
}

void CParser::SetError(CToken t, const string message)
{
  _error_token = t;
  _message = message;
  _abort = true;
  throw message;
}

bool CParser::Consume(EToken tm, CToken *token)
{
  if (_abort) return false;

  CToken t = _scanner->Get();

  if (t.GetType() != tm) {
    SetError(t, "expected '" + CToken::Name(tm) + "', got '" +
             t.GetName() + "'");
  }

  if (token != NULL) *token = t;

  return t.GetType() == tm;
}

void CParser::InitSymbolTable(CSymtab *st)
{
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
  f = new CSymProc("ReadLong", tm->GetNull(),true);
  st->AddSymbol(f);

  f = new CSymProc("WriteInt", tm->GetNull(),true);
  f->AddParam(new CSymParam(0, "v", tm->GetInteger()));
  st->AddSymbol(f);

  f = new CSymProc("WriteLong", tm->GetNull(),true);
  f->AddParam(new CSymParam(0, "v", tm->GetInteger()));
  st->AddSymbol(f);

  f = new CSymProc("WriteChar", tm->GetNull(),true);
  f->AddParam(new CSymParam(0, "c", tm->GetChar()));
  st->AddSymbol(f);

  f = new CSymProc("WriteString", tm->GetNull(),true);
  f->AddParam(new CSymParam(0, "string", tm->GetPointer(tm->GetChar())));
  st->AddSymbol(f);
  
  f = new CSymProc("WriteLn", tm->GetNull(),true);
  st->AddSymbol(f);

  f = new CSymProc("DIM", tm->GetInteger(), true);
  f->AddParam(new CSymParam(0, "array", tm->GetPointer(tm->GetNull())));
  st->AddSymbol(f);

  f = new CSymProc("DOFS", tm->GetInteger(), true);
  f->AddParam(new CSymParam(0, "array", tm->GetPointer(tm->GetNull())));
  st->AddSymbol(f);
}

CAstModule* CParser::module(void)
{
  //
  // module ::= "module" ident ";"
  //            { constDeclaration | varDeclaration | subroutineDecl }
  //            [ "begin" statSequence ] "end" ident ".".
  //

  CToken mt, t;
	string id;
  CAstStatement *statseq = NULL;
    
  Consume(tModule, &mt);
	Consume(tIdent, &mt);
	id = t.GetValue();
	Consume(tSemicolon);

  CAstModule *scope = new CAstModule(mt, id);
  InitSymbolTable(scope->GetSymbolTable());
  
  CToken t1 = _scanner->Peek();
  EToken t1_type = t1.GetType();
  while(t1_type == tConstDecl || t1_type == tVarDecl || t1_type == tFunction || t1_type == tProcedure){
    switch(t1_type){
      case tConstDecl:
        constDeclaration(scope);
        break;
      case tVarDecl:
        varDeclaration(scope);
        break;
      case tFunction:
        subroutineDecl(scope);
        break;
      case tProcedure:
        subroutineDecl(scope);
        break;
      default:
        SetError(t1, "invalid declaration");
        break;
    }
    t1 = _scanner->Peek();
    t1_type = t1.GetType();
  }
  
  if(t1_type == tBegin){
    Consume(tBegin);
    statseq = statSequence(scope);
    scope->SetStatementSequence(statseq);
    Consume(tEnd);
  }

  Consume(tIdent);
  Consume(tDot);

  scope->SetStatementSequence(statseq);
  

  return scope;
}

void CParser::varDecl(CAstScope *scope, bool isConst) {
  CToken t;
  vector<CToken> idents;
  while (_scanner->Peek().GetType() == tIdent) {
    Consume(tIdent, &t);
    idents.push_back(t);
    while (_scanner->Peek().GetType() == tComma) {
      Consume(tComma);
      Consume(tIdent, &t);
      idents.push_back(t);
    }
    Consume(tColon);
  }
  t = (_scanner->Get());
  EToken baseType = t.GetType();
  vector<CAstConstant *> cAstConsts;
  while (_scanner->Peek().GetType() == tLBrak) {
    Consume(tLBrak);
    Consume(tInteger, &t);
    CAstConstant *c = number();
    Consume(tRBrak);
  }
  CTypeManager *tm = CTypeManager::Get();
  const CType *ty;
  switch (t.GetType()) {
    case tInteger:
      ty = CTypeManager::Get()->GetInteger();
      break;
    case tChar:
      ty = CTypeManager::Get()->GetChar();
      break;
    case tBoolean:
      ty = CTypeManager::Get()->GetBool();
      break;
    case tLongint:
      ty = CTypeManager::Get()->GetLongint();
      break;
    default:
      SetError(t, "invalid type");
      break;
  }

  while (cAstConsts.size() > 0) {
    CAstConstant *constExpr = cAstConsts.back();
    ty = CTypeManager::Get()->GetArray(constExpr->GetValue(), ty);
  }

  for (auto &i : idents) {
    CSymbol *s;
    if(isConst){
       s = scope->CreateConst(i.GetValue(), ty, NULL);
    }
    else{
       s = scope->CreateVar(i.GetValue(), ty);
    }
    scope->GetSymbolTable()->AddSymbol(s);
  }
}


void CParser::constDeclaration(CAstScope *scope){
  // constDeclaration  = [ "const" constDeclSequence ].
  // constDeclSequence = constDecl ";" { constDecl ";" }
  // constDecl         = varDecl "=" expression.

  Consume(tConstDecl);
  while(_scanner->Peek().GetType() == tIdent){
    varDecl(scope, true);
    Consume(tRelOp);
    expression(scope);
    Consume(tSemicolon);
  }
}

void CParser::varDeclaration(CAstScope *scope, bool isConst){
//  varDeclaration    = [ "var" varDeclSequence ";" ].
//  varDeclSequence   = varDecl { ";" varDecl }.
//  varDecl           = ident { "," ident } ":" tm.

  Consume(tVarDecl);
  while(_scanner->Peek().GetType() == tIdent){
    varDecl(scope, isConst);
    if(_scanner->Peek().GetType() == tSemicolon){
      Consume(tSemicolon);
      if(_scanner->Peek().GetType() == tIdent){ // Since Follow(varDeclaration) = {tBegin}
        Consume(tVarDecl);
      }
    }
  }
  
}

void CParser::procDeclaration(CAstScope *scope){
  EToken subroutineType = _scanner->Get().GetType();
  EToken name = _scanner->Get().GetType();
  scope->GetSymbolTable()->AddSymbol(scope->CreateConst());
  // if(_scanner->Get().GetType() == tExtern){
  //   Consume(tExtern);
  //   Consume(tColon);
  //   if(subroutineType == tFunction){

  // }
}

CAstStatement* CParser::statSequence(CAstScope *s)
{
  //
  // statSequence ::= [ statement { ";" statement } ].
  // statement ::= assignment | subroutineCall | ifStatement | whileStatement | returnStatement.
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
      _scanner->Peek().GetType() != tElse
      ) {
    CAstStatement *tail = NULL;

    do {
      CAstStatement *st = NULL;

      switch (_scanner->Peek().GetType()) {
        // statement ::= assignment | subroutineCall | ifStatement | whileStatement | returnStatement.
        case tCharConst:
				  st = assignment(s);
				  break;
			  case tIf:
				  st = ifstatement(s);
				  break;
			  case tWhile:
				  st = whilestatement(s);
				  break;
			  case tReturn:
				  st = returnstatement(s);
				  break;

        default:
          SetError(_scanner->Peek(), "statement expected.");
          break;
      }

      assert(st != NULL);
      if (head == NULL) head = st;
      else tail->SetNext(st);
      tail = st;

      if (_scanner->Peek().GetType() == tEnd) break;

      Consume(tSemicolon);
    } while (!_abort);
  }

  return head;
}

CAstStatAssign* CParser::assignment(CAstScope *s)
{
  //
  // assignment ::= letter ":=" expression.
  //
  CToken t;

  CAstDesignator *lhs = letter(s);
  Consume(tAssign, &t);

  CAstExpression *rhs = expression(s);

  return new CAstStatAssign(t, lhs, rhs);
}

CAstExpression* CParser::expression(CAstScope* s)
{
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

    if (t.GetValue() == "=")       relop = opEqual;
    else if (t.GetValue() == "#")  relop = opNotEqual;
    else SetError(t, "invalid relation.");

    return new CAstBinaryOp(t, relop, left, right);
  } else {
    return left;
  }
}

CAstExpression* CParser::simpleexpr(CAstScope *s)
{
  //
  // simpleexpr ::= term { termOp term }.
  //
  CAstExpression *n = NULL;

  n = term(s);

  while (_scanner->Peek().GetType() == tPlusMinus) {
    CToken t;
    CAstExpression *l = n, *r;

    Consume(tPlusMinus, &t);

    r = term(s);

    n = new CAstBinaryOp(t, t.GetValue() == "+" ? opAdd : opSub, l, r);
  }


  return n;
}

CAstExpression* CParser::term(CAstScope *s)
{
  //
  // term ::= factor { ("*"|"/") factor }.
  //
  CAstExpression *n = NULL;

  n = factor(s);

  EToken tt = _scanner->Peek().GetType();

  while (tt == tMulDiv) {
    CToken t;
    CAstExpression *l = n, *r;

    Consume(tMulDiv, &t);

    r = factor(s);

    n = new CAstBinaryOp(t, t.GetValue() == "*" ? opMul : opDiv, l, r);

    tt = _scanner->Peek().GetType();
  }

  return n;
}

CAstExpression* CParser::factor(CAstScope *s)
{
  //
  // factor ::= number | "(" expression ")"
  //
  // FIRST(factor) = { tDigit, tLBrak }
  //

  CToken t;
  CAstExpression *n = NULL;

  switch (_scanner->Peek().GetType()) {
    // factor ::= number
    case tDigit:
      n = number();
      break;

    // factor ::= "(" expression ")"
    case tLBrak:
      Consume(tLBrak);
      n = expression(s);
      Consume(tRBrak);
      break;

    default:
      SetError(_scanner->Peek(), "factor expected.");
      break;
  }

  return n;
}

CAstDesignator* CParser::letter(CAstScope *s)
{
  //
  // letter := "a".."z".
  //

  CToken t;
  CSymtab *st = s->GetSymbolTable();

  Consume(tLetter, &t);

  // check if symbol exists in (local) symbol table
  const CSymbol *sym = st->FindSymbol(t.GetValue(), sLocal);

  if (sym == NULL) {
    // if not, create one and add it to the symbol table
    CSymbol *nsym = s->CreateVar(t.GetValue(), CTypeManager::Get()->GetInteger());
    st->AddSymbol(nsym);

    sym = nsym;
  }

  return new CAstDesignator(t, sym);
}

CAstConstant* CParser::number(void)
{
  //
  // number ::= "0".."9".
  //

  CToken t;

  Consume(tDigit, &t);

  errno = 0;
  long long v = strtoll(t.GetValue().c_str(), NULL, 10);
  if (errno != 0) SetError(t, "invalid number.");

  return new CAstConstant(t, CTypeManager::Get()->GetInteger(), v);
}

