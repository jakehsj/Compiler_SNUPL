//--------------------------------------------------------------------------------------------------
/// @brief SnuPL scanner
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/09/10 Bernhard Egger assignment 1: scans SnuPL/-1
/// 2016/03/11 Bernhard Egger adapted to SnuPL/1
/// 2016/03/13 Bernhard Egger assignment 1: adapted to modified SnuPL/-1 syntax
/// 2017/09/22 Bernhard Egger fixed implementation of strings and characters
/// 2019/09/13 Bernhard Egger added const token, better string/char handling
/// 2020/07/31 Bernhard Egger adapted to SnuPL/2
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
#include <sstream>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include <cstdio>

#include "scanner.h"
using namespace std;

//--------------------------------------------------------------------------------------------------
// token names
//
#define TOKEN_STRLEN 24

char ETokenName[][TOKEN_STRLEN] = {
  "tIdent",                         ///< identifier
  "tNumber",                        ///< a number
  "tPlusMinus",                     ///< '+' or '-'
  "tMulDiv",                        ///< '*' or '/'
  "tRelOp",                         ///< relational operator
  "tOr",                            ///< '||'
  "tAnd",                           ///< '&&'
  "tNot",                           ///< '!'
  "tAssign",                        ///< assignment operator
  "tSemicolon",                     ///< a semicolon
  "tColon",                         ///< a colon
  "tDot",                           ///< a dot
  "tComma",                         ///< a comma
  "tLBrak",                         ///< a left bracket
  "tRBrak",                         ///< a right bracket
  "tLParens",                       ///< a left parenthesis
  "tRParens",                       ///< a right parenthesis
  "tCharConst",                     ///< character constant
  "tStringConst",                   ///< string constant

  "tEOF",                           ///< end of file
  "tIOError",                       ///< I/O error
  "tInvStringConst",                ///< invalid string constant
  "tUndefined",                     ///< undefined
  "tComment",                       ///< comment
  "tInvCharConst",                  ///< invalid character constant

  "tModule",                        ///< 'module'
  "tProcedure",                     ///< 'procedure'
  "tFunction",                      ///< 'function'
  "tExtern",                        ///< 'extern'
  "tVarDecl",                       ///< 'var'
  "tConstDecl",                     ///< 'const'
  "tLongint",                       ///< 'longint'
  "tInteger",                       ///< 'integer'
  "tBoolean",                       ///< 'boolean'
  "tChar",                          ///< 'char'
  "tBegin",                         ///< 'begin'
  "tEnd",                           ///< 'end'
  "tIf",                            ///< 'if'
  "tThen",                          ///< 'then'
  "tElse",                          ///< 'else'
  "tWhile",                         ///< 'while'
  "tDo",                            ///< 'do'
  "tReturn"                         ///< 'return'
  "tBoolConst",                     ///< boolean constant
};



//------------------------------------------------------------------------------
// format strings used for printing tokens
//

char ETokenStr[][TOKEN_STRLEN] = {
  "tIdent (%s)",                    ///< identifier
  "tNumber (%s)",                   ///< a number
  "tPlusMinus (%s)",                ///< '+' or '-'
  "tMulDiv (%s)",                   ///< '*' or '/'
  "tRelOp (%s)",                    ///< relational operator
  "tOr",                            ///< '||'
  "tAnd",                           ///< '&&'
  "tNot",                           ///< '!'
  "tAssign",                        ///< assignment operator
  "tSemicolon",                     ///< a semicolon
  "tColon",                         ///< a colon
  "tDot",                           ///< a dot
  "tComma",                         ///< a comma
  "tLBrak",                         ///< a left bracket
  "tRBrak",                         ///< a right bracket
  "tLParens",                       ///< a left parenthesis
  "tRParens",                       ///< a right parenthesis
  "tCharConst (%s)",                ///< character constant
  "tStringConst (%s)",              ///< string constant

  "tEOF",                           ///< end of file
  "tIOError",                       ///< I/O error
  "tInvStringConst (%s)",           ///< invalid string constant
  "tUndefined",                     ///< undefined
  "tComment",                       ///< comment
  "tInvCharConst (%s)",             ///< invalid character constant
  
  "tModule",                        ///< 'module'
  "tProcedure",                     ///< 'procedure'
  "tFunction",                      ///< 'function'
  "tExtern",                        ///< 'extern'
  "tVarDecl",                       ///< 'var'
  "tConstDecl",                     ///< 'const'
  "tLongint",                       ///< 'longint'
  "tInteger",                       ///< 'integer'
  "tBoolean",                       ///< 'boolean'
  "tChar",                          ///< 'char'
  "tBegin",                         ///< 'begin'
  "tEnd",                           ///< 'end'
  "tIf",                            ///< 'if'
  "tThen",                          ///< 'then'
  "tElse",                          ///< 'else'
  "tWhile",                         ///< 'while'
  "tDo",                            ///< 'do'
  "tReturn",                        ///< 'return'
  "tBoolConst (%s)",                ///< boolean constant
};



//--------------------------------------------------------------------------------------------------
// reserved keywords
//
pair<const char*, EToken> Keywords[] =
{
  {"module", tModule},
  {"procedure", tProcedure},
  {"function", tFunction},
  {"extern", tExtern},
  {"var", tVarDecl},
  {"const", tConstDecl},
  {"longint", tLongint},
  {"integer", tInteger},
  {"boolean", tBoolean},
  {"char", tChar},
  {"begin", tBegin},
  {"end", tEnd},
  {"if", tIf},
  {"then", tThen},
  {"else", tElse},
  {"while", tWhile},
  {"do", tDo},
  {"return", tReturn},
  {"true", tBoolConst},
  {"false", tBoolConst},
};



//--------------------------------------------------------------------------------------------------
// CToken
//
CToken::CToken()
{
  _type = tUndefined;
  _value = "";
  _line = _char = 0;
}

CToken::CToken(int line, int charpos, EToken type, const string value)
{
  _type = type;
  if ((type==tStringConst) || (type==tCharConst)) _value = escape(type, value);
  else _value = value;
  _line = line;
  _char = charpos;
}

CToken::CToken(const CToken &token)
{
  _type = token.GetType();
  _value = token.GetValue();
  _line = token.GetLineNumber();
  _char = token.GetCharPosition();
}

CToken::CToken(const CToken *token)
{
  _type = token->GetType();
  _value = token->GetValue();
  _line = token->GetLineNumber();
  _char = token->GetCharPosition();
}

const string CToken::Name(EToken type)
{
  return string(ETokenName[type]);
}

const string CToken::GetName(void) const
{
  return CToken::Name(GetType());
}

ostream& CToken::print(ostream &out) const
{
  #define MAX_STRLEN 128
  int str_len = _value.length();
  str_len = TOKEN_STRLEN + (str_len < MAX_STRLEN ? str_len : MAX_STRLEN);
  char *str = (char*)malloc(str_len);
  snprintf(str, str_len, ETokenStr[GetType()], _value.c_str());
  out << dec << _line << ":" << _char << ": " << str;
  free(str);
  return out;
}

string CToken::escape(EToken type, const string text)
{
  // inverse of CToken::unescape()

  const char *t = text.c_str();
  string s;

  while ((type == tCharConst) || (*t != '\0')) {
    char c = *t;

    switch (c) {
      case '\n': s += "\\n";  break;
      case '\t': s += "\\t";  break;
      case '\0': s += "\\0";  break;
      case '\'': if (type == tCharConst) s += "\\'";  
                 else s += c;
                 break;
      case '\"': if (type == tStringConst) s += "\\\""; 
                 else s += c;
                 break;
      case '\\': s += "\\\\"; break;
      default :  if ((c < ' ') || (c == '\x7f')) {
                   // ASCII characters 0x80~0xff satisfy (signed char c < ' ')
                   char str[5];
                   snprintf(str, sizeof(str), "\\x%02x", (unsigned char)c);
                   s += str;
                 } else {
                   s += c;
                 }
    }
    if (type == tCharConst) break;
    t++;
  }
  // if(type == tStringConst){
  //   s = '\"' + s;
  //   s += '\"';
  // }
  return s;
}

string CToken::unescape(const string text)
{
  // inverse of CToken::escape()

  const char *t = text.c_str(); 
  char c;
  string s;

  while (*t != '\0') {
    if (*t == '\\') {
      switch (*++t) {
        case 'n':  s += "\n";  break;
        case 't':  s += "\t";  break;
        case '0':  s += "\0";  break;
        case '\'': s += "'";  break;
        case '"':  s += "\""; break;
        case '\\': s += "\\"; break;
        case 'x':  c  = digitValue(*++t)<<4;
                   s += (c + digitValue(*++t)); break;
        default :  s += '?'; // error
      }
    } else {
      s += *t;
    }
    t++;
  }

  return s;
}

int CToken::digitValue(char c)
{
  c = tolower(c);
  if (('0' <= c) && (c <= '9')) return c - '0';
  if (('a' <= c) && (c <= 'f')) return c - 'a' + 10;
  return -1;
}

ostream& operator<<(ostream &out, const CToken &t)
{
  return t.print(out);
}

ostream& operator<<(ostream &out, const CToken *t)
{
  return t->print(out);
}


//--------------------------------------------------------------------------------------------------
// CScanner
//
map<string, EToken> CScanner::keywords;

CScanner::CScanner(istream *in)
{
  InitKeywords();
  _in = in;
  _delete_in = false;
  _line = _char = 1;
  _token = NULL;
  _good = in->good();
  NextToken();
}

CScanner::CScanner(string in)
{
  InitKeywords();
  _in = new istringstream(in);
  _delete_in = true;
  _line = _char = 1;
  _token = NULL;
  _good = true;
  NextToken();
}

CScanner::~CScanner()
{
  if (_token != NULL) delete _token;
  if (_delete_in) delete _in;
}

void CScanner::InitKeywords(void)
{
  if (keywords.size() == 0) {
    int size = sizeof(Keywords) / sizeof(Keywords[0]);
    for (int i=0; i<size; i++) {
      keywords[Keywords[i].first] = Keywords[i].second;
    }
  }
}

CToken CScanner::Get()
{
  CToken result(_token);

  EToken type = _token->GetType();
  _good = !(type == tIOError);

  NextToken();
  return result;
}

CToken CScanner::Peek() const
{
  return CToken(_token);
}

void CScanner::NextToken()
{
  if (_token != NULL) delete _token;

  _token = Scan();
}

void CScanner::RecordStreamPosition(void)
{
  _saved_line = _line;
  _saved_char = _char;
}

void CScanner::GetRecordedStreamPosition(int *lineno, int *charpos)
{
  *lineno = _saved_line;
  *charpos = _saved_char;
}

CToken* CScanner::NewToken(EToken type, const string token)
{
  return new CToken(_saved_line, _saved_char, type, token);
}

CToken* CScanner::Scan()
{
  EToken token;
  string tokval;
  unsigned char c;

  // while (_in->good() && IsWhite(PeekChar())) GetChar();
  while (_in->good()) { 
    if(IsWhite(PeekChar())) GetChar();
    else if(PeekChar() == '/') {
      GetChar();
      if(PeekChar() == '/'){
        GetChar();
        while (PeekChar() != '\n'){
          if(PeekChar() == 255) { // EOF
            RecordStreamPosition();
            return NewToken(tEOF);
          }
          GetChar();
        }
      } else{
        _saved_line = _line;
        _saved_char = _char - 1;
        return NewToken(tMulDiv, "/");
      }
    } else break;
  }
  RecordStreamPosition();

  if (_in->eof()) return NewToken(tEOF);
  if (!_in->good()) return NewToken(tIOError);

  c = GetChar();
  // printf("new %c\n", c);
  tokval = c;
  token = tUndefined;

  switch (c) {
    case ':':
      if (PeekChar() == '=') {
        tokval += GetChar();
        token = tAssign;
      } else{
        token = tColon;
      }
      break;

    case '+':
    case '-':
      token = tPlusMinus;
      break;

    case '*':
      token = tMulDiv;
      break;

    case '=':
    case '#':
      token = tRelOp;
      break;
    case '<':
    case '>':
      if (PeekChar() == '=') {
        tokval += GetChar();
        token = tRelOp;
      } else token = tRelOp;
      break;

    case '|':
      tokval += GetChar();
      token = tOr;
      break;
    
    case '&':
      tokval += GetChar();
      token = tAnd;
      break;
    
    case '!':
      token = tNot;
      break;

    case ',':
      token = tComma;
      break;

    case ';':
      token = tSemicolon;
      break;

    case '.':
      token = tDot;
      break;

    case '(':
      token = tLParens;
      break;

    case ')':
      token = tRParens;
      break;
    
    case '[':
      token = tLBrak;
      break;
    
    case ']':
      token = tRBrak;
      break;

    default:
      if (IsAlpha(c)) {
        token = tIdent;
        while(IsIDChar(PeekChar())) tokval += GetChar();
        map<string, EToken>::iterator it = keywords.find(tokval);
        if (it != keywords.end()) { 
            token = (*it).second;
          }
        else token = tIdent;
      } else if (IsNum(c)) {
        token = tNumber;
        while(IsNum(PeekChar())) tokval += GetChar();
        if(PeekChar() == 'L') tokval += GetChar();
      } else if (c == '\''){
        ECharacter res = GetCharacter(c, tCharConst);
        if(PeekChar() != '\'') {
          res = cInvChar;
          while(PeekChar() != '\'' && PeekChar() != '\n') {
            if(PeekChar() == 255) break;
            GetChar();
          }
        }
        GetChar();
        if(res != cOkay) token = tInvCharConst;
        else token = tCharConst;
        tokval = c;
      } else if (c == '\"'){
        tokval = "";
        ECharacter res = GetCharacter(c, tStringConst);
        while (res == cOkay) {
          if (c == '\"') break;
          tokval += c;
          res = GetCharacter(c, tStringConst);
        }
        if(res != cOkay) token = tInvStringConst;
        else token = tStringConst;
      }
      else {
        tokval = "invalid character '";
        tokval += c;
        tokval += "'";
      }
      break;
  }
  return NewToken(token, tokval);
}

CScanner::ECharacter CScanner::GetCharacter(unsigned char &c, EToken mode)
{
  int i, t, v;
  ECharacter res = cOkay;

  // To pin-point the exact location of the illegal character, we only peek at
  // the next character here.
  // GetChar() advances the line/character pos (used by RecordStreamPosition())
  // and we cannot use RecordStreamPosition() before reading the character
  // because that would overwrite the position needed for valid strings
  // (recorded at the initial '"').
  //
  // As a consequence, GetChar() calls need to be inserted at the right
  // positions in this function to make sure we consume the character(s).
  //
  // Also note that we do not return cEOF/cIOError because this would hide
  // the invalid string/character constant error. EOF and I/O errors are
  // caught the next time CScanner::Scan() is invoked.
  c = PeekChar();

  if (c == '\\') {
    // escaped character
    if (_in->eof() || !_in->good()) return cUnexpEnd;
    c = GetChar();

    switch (PeekChar()) {
      case 'n':  c = '\n'; break;
      case 't':  c = '\t'; break;
      case '0':  if (mode == tCharConst) c = '\0'; 
                 else res = cInvEnc;
                 break;
      case '\'': c = '\''; break;
      case '\"': c = '\"'; break;
      case '\\': c = '\\'; break;

      case 'x':  // \xHH encoding: read exactly two hexadecimal digits
                 for (i=v=0; i<2; i++) {
                   if (_in->eof() || !_in->good()) return cUnexpEnd;
                   GetChar();
                   if ((t = CToken::digitValue(PeekChar())) == -1) break;
                   v = (v << 4) + t;
                 }
                 c = v;

                 if (t == -1) res = cInvChar;
                 else if ((mode != tCharConst) && (v == 0)) res = cInvEnc;
                 break;

      default:   res = cInvEnc;
    }
  } else if ((c < ' ') || (c == 0x7f)) {
    // non-printable characters must be escaped
    res = c == '\n' ? cUnexpEnd : cInvChar;
  }

  // record exact error position
  if (res != cOkay) RecordStreamPosition();

  // consume character (we only peeked at it so far)
  if (_in->eof() || !_in->good()) return cUnexpEnd;
  GetChar();
  
  return res;
}

unsigned char CScanner::PeekChar()
{
  return (unsigned char)_in->peek();
}

unsigned char CScanner::GetChar()
{
  unsigned char c = _in->get();
  if (c == '\n') { _line++; _char = 1; } else _char++;
  return c;
}

string CScanner::GetChar(int n)
{
  string str;
  for (int i=0; i<n; i++) str += GetChar();
  return str;
}

bool CScanner::IsWhite(unsigned char c)
{
  return ((c == ' ') || (c == '\t') || (c == '\n'));
}

bool CScanner::IsAlpha(unsigned char c)
{
  return ((('a' <= c) && (c <= 'z')) || (('A' <= c) && (c <= 'Z')) || (c == '_'));
}

bool CScanner::IsNum(unsigned char c)
{
  return (('0' <= c) && (c <= '9'));
}

bool CScanner::IsHexDigit(unsigned char c)
{
  return (('0' <= c) && (c <= '9')) ||
         (('a' <= c) && (c <= 'f')) ||
         (('A' <= c) && (c <= 'F'));
}

bool CScanner::IsIDChar(unsigned char c)
{
  return (IsAlpha(c) || IsNum(c));
}

