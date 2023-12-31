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

#ifndef __SnuPL_SCANNER_H__
#define __SnuPL_SCANNER_H__

#include <istream>
#include <ostream>
#include <iomanip>
#include <map>

using namespace std;

//--------------------------------------------------------------------------------------------------
/// @brief SnuPL token type
///
/// Each member of this enumeration represents a token in SnuPL
///
enum EToken {
  tIdent=0,                         ///< identifier
  tNumber,                          ///< a number
  tPlusMinus,                       ///< '+' or '-'
  tMulDiv,                          ///< '*' or '/'
  tRelOp,                           ///< relational operator
  tOr,                              ///< '||'
  tAnd,                             ///< '&&'
  tNot,                             ///< '!'
  tAssign,                          ///< assignment operator
  tSemicolon,                       ///< a semicolon
  tColon,                           ///< a colon
  tDot,                             ///< a dot
  tComma,                           ///< a comma
  tLBrak,                           ///< a left bracket
  tRBrak,                           ///< a right bracket
  tLParens,                         ///< a left parenthesis
  tRParens,                         ///< a right parenthesis
  tCharConst,                       ///< character constant
  tStringConst,                     ///< string constant

  tEOF,                             ///< end of file
  tIOError,                         ///< I/O error
  tInvStringConst,                  ///< invalid string constant
  tUndefined,                       ///< undefined
  tComment,                         ///< comment
  tInvCharConst,                    ///< invalid character constant

  tModule,                          ///< 'module'
  tProcedure,                       ///< 'procedure'
  tFunction,                        ///< 'function'
  tExtern,                          ///< 'extern'
  tVarDecl,                         ///< 'var'
  tConstDecl,                       ///< 'const'
  tLongint,                         ///< 'longint'
  tInteger,                         ///< 'integer'
  tBoolean,                         ///< 'boolean'
  tChar,                            ///< 'char'
  tBegin,                           ///< 'begin'
  tEnd,                             ///< 'end'
  tIf,                              ///< 'if'
  tThen,                            ///< 'then'
  tElse,                            ///< 'else'
  tWhile,                           ///< 'while'
  tDo,                              ///< 'do'
  tReturn,                          ///< 'return'
  tBoolConst,                       ///< boolean constant
};


//--------------------------------------------------------------------------------------------------
/// @brief token class
///
/// Represents a token. Each token has a type (EToken) and a value (the lexeme).
/// Additional fields specify the exact position of the lexeme in the input
/// stream (line/column); this is used for error reporting.
///
/// Note that the token value is stored in escaped form for strings/characters.
///
class CToken {
  friend class CScanner;
  public:
    /// @name constructors
    /// @{

    /// @brief default constructor
    CToken();

    /// @brief constructor taking initialization values
    ///
    /// @param line line number in the input stream
    /// @param charpos character position in the input stream
    /// @param type token type
    /// @param value token value
    CToken(int line, int charpos, EToken type, const string value="");

    /// @brief copy contructor
    ///
    /// @param token token to copy
    CToken(const CToken &token);

    /// @brief copy contructor
    ///
    /// @param token token to copy
    CToken(const CToken *token);
    /// @}

    /// @name token attributes
    /// @{

    /// @brief return the name for a given token
    ///
    /// @retval token name
    static const string Name(EToken type);

    /// @brief return the token name of this instance
    ///
    /// @retval token name
    const string GetName(void) const;

    /// @brief return the token type of this instance
    ///
    /// @retval token type
    EToken GetType(void) const { return _type; };

    /// @brief return the token value of this instance
    ///
    /// @retval token value
    string GetValue(void) const { return _value; };

    /// @}

    /// @name stream attributes
    /// @{

    /// @brief return the line number
    ///
    /// @retval line number of the token in the input stream
    int GetLineNumber(void) const { return _line; };

    /// @brief return the character position
    ///
    /// @retval character position of the token in the input stream
    int GetCharPosition(void) const { return _char; };

    /// @}

    /// @name string escaping/unescaping (static methods)
    /// @{

    /// @brief escape special characters in a string
    ///
    /// @param type token type
    /// @param text string
    /// @retval escaped string
    static string escape(EToken type, const string text);

    /// @brief unescape special characters in a string
    ///
    /// @param text escapted string
    /// @retval unescaped string
    static string unescape(const string text);

    /// @brief return the value of a single hexadecimal digit
    ///
    /// @param c hexadecimal digit
    /// @retval int numerical value of digit (>=0)
    /// @retval -1 if @c is not a (hexa)decimal digit
    static int digitValue(char c);

    /// @}

    /// @brief print the token to an output stream
    ///
    /// @param out output stream
    ostream&  print(ostream &out) const;

  private:
    EToken _type;                   ///< token type
    string _value;                  ///< token value
    int    _line;                   ///< input stream position (line)
    int    _char;                   ///< input stream position (character pos)
};

/// @name CToken output operators
/// @{

/// @brief CToken output operator
///
/// @param out output stream
/// @param t reference to CToken
/// @retval output stream
ostream& operator<<(ostream &out, const CToken &t);

/// @brief CToken output operator
///
/// @param out output stream
/// @param t reference to CToken
/// @retval output stream
ostream& operator<<(ostream &out, const CToken *t);

/// @}


//--------------------------------------------------------------------------------------------------
/// @brief scanner class
///
/// Instantiated by CParser and called repeatedly to tokenize SnuPL code.
///
class CScanner {
  public:
    /// @name construction/destruction
    /// @{

    /// @brief constructor
    ///
    /// @param in input stream containing the source code
    CScanner(istream *in);

    /// @brief constructor
    ///
    /// @param in input stream containing the source code
    CScanner(string in);

    /// @brief destructor
    ~CScanner();

    /// @}

    /// @brief return and remove the next token from the input stream
    ///
    /// @retval token token
    CToken Get(void);

    /// @brief peek at the next token in the input stream (without removing it)
    ///
    /// @retval token token
    CToken Peek(void) const;

    /// @brief check the status of the scanner
    ///
    /// @retval true if the scanner is in an operating (i.e., normal) state
    /// @retval false if an error has occurred
    bool Good(void) const { return _good; };

    /// @brief get the line number of the next token in the input stream
    ///
    /// @retval line number
    int GetLineNumber(void) const { return _line; };

    /// @brief get the character position of the next token in the input stream
    ///
    /// @retval character position
    int GetCharPosition() const { return _char; };

  private:
    /// @brief result type for the GetCharacter() method
    enum ECharacter {
      cOkay =0,                     ///< character parsed
      cInvChar,                     ///< invalid character
      cInvEnc,                      ///< invalid escape sequence
      cUnexpEnd,                    ///< unexpected end of string/character
    };

    /// @brief initialize list of reserved keywords
    void InitKeywords(void);

    /// @brief scan the next token
    void NextToken(void);

    /// @brief store the current position of the input stream internally
    void RecordStreamPosition(void);

    /// @brief return the previously recorded input stream position
    ///
    /// @param lineno line number
    /// @param charpos character position
    void GetRecordedStreamPosition(int *lineno, int *charpos);

    /// @brief create and return a new token
    ///
    /// @param type token type
    /// @param token  token value
    /// @retval CToken instance
    CToken* NewToken(EToken type, const string token="");


    /// @name low-level scanner routines
    /// @{

    /// @brief scan the input stream and return the next token
    ///
    /// @retval CToken instance
    CToken* Scan(void);

    /// @brief parse a (possibly escaped) character
    ///
    /// @param &c parsed (i.e., unescaped) character. Valid iff retval = cOkay
    /// @param mode mode. Must be tStringConst or tCharConst
    /// @retval ECharacter status of character parse
    ECharacter GetCharacter(unsigned char &c, EToken mode);

    /// @brief peek at the next character in the input stream (w/o removing it)
    ///
    /// @retval next character in the input stream
    unsigned char PeekChar(void);

    /// @brief return the next character from the input stream
    ///
    /// @retval next character in the input stream
    unsigned char GetChar(void);

    /// @brief return the next 'n' characters from the input stream
    ///
    /// @param n number of characters to read
    /// @retval string containing the characters read
    string GetChar(int n);

    /// @brief check if a character is a white character
    ///
    /// @param c character
    /// @retval true character is white space
    /// @retval false character is not white space
    static bool IsWhite(unsigned char c);

    /// @brief check if a character is an alphabetic character (a-z, A-Z)
    ///
    /// @param c character
    /// @retval true character is alphabetic
    /// @retval false character is not alphabetic
    static bool IsAlpha(unsigned char c);

    /// @brief check if a character is an numeric character (0-9)
    ///
    /// @param c character
    /// @retval true character is numeric
    /// @retval false character is not numeric
    static bool IsNum(unsigned char c);

    /// @brief check if a character is an hexadecimal digit (0-9,a-f,A-F)
    ///
    /// @param c character
    /// @retval true character is a hexadecimal digit
    /// @retval false character is not a hexadecimal digit
    static bool IsHexDigit(unsigned char c);

    /// @brief check if a character is a valid ID character
    ///
    /// @param c character
    /// @retval true character is valid as an ID character
    /// @retval false character is not valid in an ID
    static bool IsIDChar(unsigned char c);

    /// @}


  private:
    static map<string, EToken> keywords;///< reserved keywords with corr. tokens
    istream *_in;                   ///< input stream
    bool    _delete_in;             ///< delete input stream upon destruction
    bool    _good;                  ///< scanner status flag
    int     _line;                  ///< current stream position (line)
    int     _char;                  ///< current stream position (character pos)
    int     _saved_line;            ///< saved stream position (line)
    int     _saved_char;            ///< saved stream position (character pos)
    CToken *_token;                 ///< next token in input stream
};


#endif // __SnuPL_SCANNER_H__
