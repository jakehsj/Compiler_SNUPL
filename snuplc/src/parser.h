//--------------------------------------------------------------------------------------------------
/// @brief SnuPL parser
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2016/03/09 Bernhard Egger adapted to SnuPL/1
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

#ifndef __SnuPL_PARSER_H__
#define __SnuPL_PARSER_H__

#include "ast.h"
#include "scanner.h"
#include "symtab.h"

//--------------------------------------------------------------------------------------------------
/// @brief operational modes for type scanning
///
enum EType {
  mConstant = 0,  ///< constant definition
  mVariable,      ///< variable definition
  mFormalPar,     ///< formal parameter definition
};

//--------------------------------------------------------------------------------------------------
/// @brief parser
///
/// parses a module
///
class CParser {
 public:
  /// @brief constructor
  ///
  /// @param scanner  CScanner from which the input stream is read
  CParser(CScanner *scanner);

  /// @brief parse a module
  /// @retval CAstNode program node
  CAstNode *Parse(void);

  /// @name error handling
  ///@{

  /// @brief indicates whether there was an error while parsing the source
  /// @retval true if the parser detected an error
  /// @retval false otherwise
  bool HasError(void) const { return _abort; };

  /// @brief returns the token that caused the error
  /// @retval CToken containing the error token
  const CToken *GetErrorToken(void) const;

  /// @brief returns a human-readable error message
  /// @retval error message
  string GetErrorMessage(void) const;
  ///@}

 private:
  /// @brief sets the token causing a parse error along with a message
  /// @param t token causing the error
  /// @param message human-readable error message
  void SetError(CToken t, const string message);

  /// @brief consume a token given type and optionally store the token
  /// @param type expected token type
  /// @param token If not null, the consumed token is stored in 'token'
  /// @retval true if a token has been consumed
  /// @retval false otherwise
  bool Consume(EToken type, CToken *token = NULL);

  /// @brief initialize symbol table @a s with predefined procedures and
  ///        global variables
  void InitSymbolTable(CSymtab *s);

  /// @name methods for recursive-descent parsing
  /// @{

  /// @brief parse a module
  /// @retval CAstModule module node
  CAstModule *module(void);

  /// declaration parsing

  /// @brief parse a function declaration
  /// @param scope scope whose symbol table is used to store the function
  /// @retval CAstScope scope node
  void procedureDecl(CAstScope *scope);

  /// @brief parse a const declaration
  /// @param scope scope whose symbol table is used to store the const
  /// @retval CAstScope scope node
  void constDeclaration(CAstScope *scope);

  /// @brief parse var declaration
  /// @param scope scope whose symbol table is used to store the type
  /// @retval CAstScope scope node
  void varDeclaration(CAstScope *scope);

  /// @brief parse a type declaration
  /// @param idents vector which will store the varaible names after parsing
  /// @retval CType type which variables are declared with
  CType *varDecl(vector<string> &idents);

  /// statement parsing(subclass of CAstStatement)

  /// @brief parse a sequence statement
  /// @param s scope in which the sequence statement is parsed
  /// @retval CAstStatement sequence statement node
  CAstStatement *CParser::statSequence(CAstScope *s);

  /// @brief parse a while statement
  /// @param scope scope in which the while statement is parsed
  /// @retval CAstStatWhile while statement node
  CAstStatWhile *whileStatement(CAstScope *scope);

  /// @brief parse a if statement
  /// @param scope scope in which the if statement is parsed
  /// @retval CAstStatIf if statement node
  CAstStatIf *ifStatement(CAstScope *scope);

  /// @brief parse a return statement
  /// @param scope scope in which the statement is parsed
  /// @retval CAstStatReturn return statement node
  CAstStatReturn *returnStatement(CAstScope *scope);

  /// @brief parse a assignment statement
  /// @param s scope in which the assignment statement is parsed
  /// @retval CAstStatAssign assignment statement node
  CAstStatAssign *CParser::assignment(CAstScope *s);

  /// @brief parse a subroutine call statement
  /// @note like "foo(1, 2, 3);" parse whole statement
  /// compare with @ref functionCall
  /// @param scope scope in which the statement is parsed
  /// @retval CAstStatCall subroutine call node
  CAstStatCall *subroutineCall(CAstScope *scope);

  /// expression parsing(subclass of CAstExpression)

  /// @brief parse a statement
  /// @note when "a := foo(1);" parse only rvalue "foo(1)"
  /// compare with @ref subroutineCall
  /// @param scope scope in which the statement is parsed
  /// @retval CAstFunctionCall function call node
  CAstFunctionCall *functionCall(CAstScope *scope);

  /// @brief parse a expression
  /// @param s scope in which the expression is parsed
  /// @retval CAstExpression expression node
  CAstExpression *expression(CAstScope *s);

  /// @brief parse a simple expression
  /// @param s scope in which the simple expression is parsed
  /// @retval CAstExpression simple expression node
  CAstExpression *simpleexpr(CAstScope *s);

  /// @brief parse a term
  /// @param s scope in which the term is parsed
  /// @retval CAstExpression term node
  CAstExpression *term(CAstScope *s);

  /// @brief  parse a factor
  /// @param s scope in which the factor is parsed
  /// @return CAstExpression factor node
  CAstExpression *factor(CAstScope *s);

  /// @brief parse a unary operator
  /// @retval CAstUnaryOp operation node
  CAstUnaryOp *unaryOp(CAstScope *scope);

  // /// @brief parse a letter
  // /// @param s scope in which the letter is parsed
  // /// @retval CAstDesignator designator node
  // CAstDesignator *letter(CAstScope *s);

  /// @brief parse a number constant
  /// @retval CAstConstant constant node
  CAstConstant *number(void);

  /// @brief parse a boolean constant
  /// @retval CAstConstant constant node
  CAstConstant *boolConst(void);

  /// @brief parse a string constant
  /// @param scope scope in which the string is parsed
  /// @retval CAstConstant constant node
  CAstStringConstant *stringConst(CAstScope *scope);

  /// @brief parse a character constant
  /// @retval CAstConstant constant node
  CAstConstant *charConst(void);

  /// @brief parse a ident
  /// @param scope scope in which the ident is parsed
  CAstDesignator *qualident(CAstScope *scope);
  /// @}

  CScanner *_scanner;   ///< CScanner instance
  CAstModule *_module;  ///< root node of the program
  CToken _token;        ///< current token

  /// @name error handling
  CToken _error_token;  ///< error token
  string _message;      ///< error message
  bool _abort;          ///< error flag
};

#endif  // __SnuPL_PARSER_H__
