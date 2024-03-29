/*
 * Copyright (c) 2011-2018 Igor Maznitsa. All rights reserved.
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package com.igormaznitsa.prologparser.tokenizer;

import static com.igormaznitsa.prologparser.ParserContext.FLAG_BLOCK_COMMENTS;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_ZERO_QUOTATION_ALLOWS_WHITESPACE_CHAR;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_ZERO_QUOTATION_CHARCODE;
import static com.igormaznitsa.prologparser.tokenizer.TokenizerState.ATOM;
import static com.igormaznitsa.prologparser.tokenizer.TokenizerState.LOOK_FOR;
import static com.igormaznitsa.prologparser.tokenizer.TokenizerState.OPERATOR;
import static com.igormaznitsa.prologparser.tokenizer.TokenizerState.STRING;
import static com.igormaznitsa.prologparser.utils.StringUtils.isCharAllowedForUnquotedAtom;

import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.PrologParser;
import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.terms.OpContainer;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologFloat;
import com.igormaznitsa.prologparser.terms.PrologInt;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.PrologVar;
import com.igormaznitsa.prologparser.terms.Quotation;
import com.igormaznitsa.prologparser.utils.Koi7CharOpMap;
import com.igormaznitsa.prologparser.utils.StringBuilderEx;
import com.igormaznitsa.prologparser.utils.StringUtils;
import java.io.IOException;
import java.io.PushbackReader;
import java.io.Reader;
import java.math.BigInteger;

/**
 * Internal tokenizer to extract next token from reader.
 */
public final class Tokenizer {

  private final StringBuilderEx strBuf;
  private final StringBuilderEx specCharBuf;
  private final StringBuilderEx insideCharBuffer;
  private final boolean blockCommentsAllowed;
  private final boolean zeroSingleQuotationAllowed;
  private final boolean zeroQuotationAllowsWhitespaceChar;
  private final Reader reader;
  private final PrologParser parser;
  private final Koi7CharOpMap metaOperators;
  private TokenizerResult lastPushedTerm;
  private int prevTokenLine;
  private int prevTokenPos;
  private int lastTokenLine;
  private int lastTokenPos;
  private int prevPos;
  private int prevLine;
  private int pos;
  private int line;

  public Tokenizer(final PrologParser parser, final Koi7CharOpMap metaOperators,
                   final Reader reader) {
    super();
    this.metaOperators = metaOperators;
    this.reader = reader;
    this.parser = parser;

    final int maxAllowedCharBufferSize = parser.getContext() == null ? Integer.MAX_VALUE :
        parser.getContext().getMaxTokenizerBufferLength();
    this.blockCommentsAllowed = parser.getContext() != null
        && ((parser.getContext().getFlags() & FLAG_BLOCK_COMMENTS) != 0);
    this.zeroSingleQuotationAllowed = parser.getContext() != null
        && ((parser.getContext().getFlags() & FLAG_ZERO_QUOTATION_CHARCODE) != 0);
    this.zeroQuotationAllowsWhitespaceChar = parser.getContext() != null
        && ((parser.getContext().getFlags()
        & FLAG_ZERO_QUOTATION_ALLOWS_WHITESPACE_CHAR) != 0);

    this.strBuf = new StringBuilderEx(32, maxAllowedCharBufferSize);
    this.specCharBuf = new StringBuilderEx(8, maxAllowedCharBufferSize);
    this.insideCharBuffer = new StringBuilderEx(8, maxAllowedCharBufferSize);

    this.pos = 1;
    this.line = 1;
    this.prevPos = 1;
    this.prevLine = 1;
  }

  public static boolean isCharAllowedForRadix(final char chr, final int radix) {
    if (radix == 10) {
      return Character.isDigit(chr);
    } else if (radix < 10) {
      return chr >= '0' && chr < ('0' + radix);
    } else {
      if (chr >= '0' && chr <= '9') {
        return true;
      }
      final int diff = radix - 10;
      if (chr >= 'A' && chr < ('A' + diff)) {
        return true;
      }
      return chr >= 'a' && chr < ('a' + diff);
    }
  }

  TokenizerResult getLastPushed() {
    return this.lastPushedTerm;
  }

  private synchronized int doReadChar() throws IOException {
    int ch;
    if (this.insideCharBuffer.isEmpty()) {
      ch = this.reader.read();
    } else {
      ch = this.insideCharBuffer.pop();
    }

    this.prevPos = this.pos;
    this.prevLine = this.line;
    if (ch == '\n') {
      this.pos = 1;
      this.line++;
    } else {
      if (ch >= 0) {
        this.pos++;
      }
    }
    return ch;
  }

  public void calcDiffAndPushResultBack(final String etalon, final StringBuilderEx buffer) {
    int chars = buffer.length() - etalon.length();
    int bufferPosition = buffer.length() - 1;

    int locPos = this.pos;
    int locPosPrev = this.prevPos;
    int locLine = this.line;
    int locLinePrev = this.prevLine;

    while (chars > 0) {
      final char ch = buffer.charAt(bufferPosition--);
      this.insideCharBuffer.push(ch);
      chars--;
      locPos = Math.max(1, locPos - 1);

      locPosPrev = locPos;
      if (ch == '\n') {
        locLine = Math.max(1, locLine - 1);
        locLinePrev = locLine;
      }
    }

    this.pos = locPos;
    this.prevPos = locPosPrev;
    this.line = locLine;
    this.prevLine = locLinePrev;
  }

  public void push(final char ch) {
    this.insideCharBuffer.push(ch);
    if (ch == '\n') {
      this.pos = 1;
      this.line = Math.max(1, this.line - 1);
    } else {
      this.pos = Math.max(1, this.pos - 1);
    }
  }

  public void close(final boolean closeReader) throws IOException {
    this.lastPushedTerm = null;
    try {
      if (this.reader instanceof PushbackReader) {
        final PushbackReader pbReader = (PushbackReader) this.reader;
        while (!this.strBuf.isEmpty()) {
          pbReader.unread(this.strBuf.pop());
        }
      }
    } finally {
      this.strBuf.clear();
      if (closeReader) {
        this.reader.close();
      }
    }
  }

  public boolean hasOperatorStartsWith(final String operatorNameStartSubstring) {
    boolean result = false;
    if (this.metaOperators.contains(operatorNameStartSubstring)) {
      result = true;
    } else if (parser != null) {
      final ParserContext ctx = parser.getContext();
      if (ctx != null) {
        result = ctx.hasOpStartsWith(parser, operatorNameStartSubstring);
      }
    }
    return result;
  }

  public OpContainer findOperatorForName(final String operatorName) {
    OpContainer result = null;

    // check meta-operators as the first ones
    if (operatorName.length() == 1) {
      result = this.metaOperators.get(operatorName);
    }
    if (result == null) {
      // check user defined operators because a user can replace a system operator
      final ParserContext ctx = parser.getContext();
      if (ctx != null) {
        result = ctx.findOpForName(parser, operatorName);
      }
    }

    return result;
  }

  public OpContainer findOperatorForSingleChar(final char c) {
    OpContainer result = this.metaOperators.get(c);
    if (result == null) {
      return findOperatorForName(String.valueOf(c));
    }
    return result;
  }

  public void push(final TokenizerResult object) {
    if (this.lastPushedTerm != null) {
      throw new IllegalStateException("There is already pushed term");
    }
    this.lastPushedTerm = object;
  }

  public TokenizerResult peek() {
    TokenizerResult result;
    if (this.lastPushedTerm == null) {
      result = readNextToken();
      push(result);
    } else {
      result = this.lastPushedTerm;
    }
    return result;
  }

  public int getLastTokenPos() {
    return this.lastPushedTerm == null ? this.lastTokenPos : this.prevTokenPos;
  }

  public int getLastTokenLine() {
    return this.lastPushedTerm == null ? this.lastTokenLine : this.prevTokenLine;
  }

  public void fixPosition() {
    this.prevTokenLine = this.lastTokenLine;
    this.prevTokenPos = this.lastTokenPos;
    this.lastTokenLine = this.line;
    this.lastTokenPos = this.pos - 1;
  }

  private void skipUntilBlockCommentEnd() throws IOException {
    boolean starCharDetected = false;
    while (true) {
      final int readChar = this.doReadChar();
      if (readChar < 0 || (readChar == '/' && starCharDetected)) {
        break;
      } else {
        starCharDetected = readChar == '*';
      }
    }
  }

  private void skipUntilNextString() throws IOException {
    while (true) {
      final int readChar = this.doReadChar();
      if (readChar < 0 || readChar == '\n') {
        break;
      }
    }
  }

  public TokenizerResult pop() {
    try {
      return this.lastPushedTerm;
    } finally {
      this.lastPushedTerm = null;
    }
  }

  /**
   * Read next token
   *
   * @return next token or null if not found or stream ended
   */
  public TokenizerResult readNextToken() {

    if (this.lastPushedTerm != null) {
      return pop();
    }

    Quotation quoting = Quotation.NONE;

    int radix = 10;
    char detectedRadixChar = ' ';

    TokenizerState state = LOOK_FOR;
    boolean specCharDetected = false;
    boolean charCodeAsInt = false;

    this.strBuf.clear();
    this.specCharBuf.clear();

    final StringBuilderEx strBuffer = this.strBuf;
    final StringBuilderEx specCharBuffer = this.specCharBuf;

    OpContainer lastFoundFullOperator = null;

    boolean letterOrDigitOnly = false;

    boolean foundUnderscoreInNumber = false;

    try {
      while (true) {
        final int readChar = this.doReadChar();

        if (readChar < 0) {
          switch (state) {
            case LOOK_FOR:
              return null;
            case FLOAT:
            case INTEGER:
            case ATOM: {
              if (foundUnderscoreInNumber) {
                throw new PrologParserException("Contains unexpected underscore: " + strBuffer,
                    this.prevLine, this.prevPos);
              }

              if (state == TokenizerState.FLOAT && strBuffer.isLastChar('.')) {
                // non-ended float then it is an integer number ended by the '.' operator
                push('.');
                // it is Integer
                return new TokenizerResult(
                    makeTermFromString(strBuffer.toStringExcludeLastChar(), radix, quoting,
                        TokenizerState.INTEGER),
                    TokenizerState.INTEGER,
                    getLastTokenLine(),
                    getLastTokenPos()
                );
              } else {
                // it is just integer number or an atom
                final String text = strBuffer.toString();
                return new TokenizerResult(
                    makeTermFromString(text, radix, PrologTerm.findQuotation(text), state),
                    state,
                    getLastTokenLine(),
                    getLastTokenPos()
                );
              }
            }
            case VAR: {
              if (strBuffer.isSingleChar('_')) {
                return new TokenizerResult(
                    new PrologVar(),
                    state,
                    getLastTokenLine(),
                    getLastTokenPos()
                );
              } else {
                return new TokenizerResult(
                    new PrologVar(strBuffer.toString()),
                    state,
                    getLastTokenLine(),
                    getLastTokenPos()
                );
              }
            }
            case STRING: {
              throw new PrologParserException("Non-completed string: " + strBuffer,
                  this.lastTokenLine, this.lastTokenPos);
            }
            case OPERATOR: {
              if (lastFoundFullOperator == null) {
                return new TokenizerResult(
                    makeTermFromString(strBuffer.toString(), radix, quoting, state),
                    state,
                    getLastTokenLine(),
                    getLastTokenPos()
                );
              } else {
                calcDiffAndPushResultBack(lastFoundFullOperator.getText(), strBuffer);
                return new TokenizerResult(
                    lastFoundFullOperator,
                    state,
                    getLastTokenLine(),
                    getLastTokenPos()
                );
              }
            }
            default: {
              throw new PrologParserException("Non-completed term (" + state + "): " + strBuffer,
                  this.prevLine, this.prevPos);
            }
          }
        }

        final char chr = (char) readChar;

        if (state != STRING && this.blockCommentsAllowed && chr == '*'
            && this.strBuf.isLastChar('/')) {
          if (this.strBuf.isSingleChar('/')) {
            this.strBuf.pop();
            state = this.strBuf.isEmpty() ? LOOK_FOR : state;
          } else if (state == OPERATOR) {
            throw new PrologParserException("Operator can be mixed with comment block: "
                + this.strBuf + chr, getLastTokenLine(), getLastTokenPos());
          }

          skipUntilBlockCommentEnd();

        } else {
          switch (state) {
            case LOOK_FOR: {
              if (Character.isWhitespace(chr) || Character.isISOControl(chr)) {
                continue;
              }

              switch (chr) {
                case '%': {
                  skipUntilNextString();
                }
                break;
                case '_': {
                  fixPosition();
                  strBuffer.append(chr);
                  state = TokenizerState.VAR;
                }
                break;
                case '\'': {
                  fixPosition();
                  quoting = Quotation.SINGLE;
                  state = TokenizerState.STRING;
                }
                break;
                case '\"': {
                  fixPosition();
                  quoting = Quotation.DOUBLE;
                  state = TokenizerState.STRING;
                }
                break;
                case '`': {
                  fixPosition();
                  quoting = Quotation.BACK_TICK;
                  state = TokenizerState.STRING;
                }
                break;

                default: {
                  fixPosition();

                  strBuffer.append(chr);

                  if (Character.isUpperCase(chr)) {
                    state = TokenizerState.VAR;
                  } else {
                    letterOrDigitOnly = Character.isLetterOrDigit(chr);
                    final String operator = String.valueOf(chr);

                    if (hasOperatorStartsWith(operator)) {
                      lastFoundFullOperator = findOperatorForName(operator);
                      state = TokenizerState.OPERATOR;
                    } else {
                      if (Character.isDigit(chr)) {
                        state = TokenizerState.INTEGER;
                      } else {
                        state = TokenizerState.ATOM;
                      }
                    }
                  }
                }
                break;
              }
            }
            break;
            case ATOM: {
              if (chr == '_') {
                strBuffer.append(chr);
              } else if (Character.isISOControl(chr) || Character.isWhitespace(chr)) {
                final String text = strBuffer.toString();

                if (quoting == Quotation.NONE) {
                  final OpContainer operator = findOperatorForName(text);
                  if (operator != null) {
                    return new TokenizerResult(
                        operator,
                        TokenizerState.OPERATOR,
                        getLastTokenLine(),
                        getLastTokenPos());
                  }
                }

                return new TokenizerResult(
                    makeTermFromString(text, radix, PrologTerm.findQuotation(text), state),
                    state,
                    getLastTokenLine(),
                    getLastTokenPos());
              } else if ((chr == '\'' || chr == '\"' || chr == '`')
                  || (letterOrDigitOnly != Character.isLetterOrDigit(chr))
                  || (!Character.isLetter(chr) && findOperatorForSingleChar(chr) != null)) {
                push(chr);
                final String text = strBuffer.toString();

                if (quoting == Quotation.NONE) {
                  final OpContainer operator = findOperatorForName(text);
                  if (operator != null) {
                    return new TokenizerResult(
                        operator,
                        TokenizerState.OPERATOR,
                        getLastTokenLine(),
                        getLastTokenPos());
                  }
                }
                return new TokenizerResult(
                    makeTermFromString(text, radix, PrologTerm.findQuotation(text), state),
                    state,
                    getLastTokenLine(),
                    getLastTokenPos());

              } else {
                strBuffer.append(chr);
              }
            }
            break;
            case INTEGER: {
              if (isCharAllowedForRadix(chr, radix)) {
                foundUnderscoreInNumber = false;
                strBuffer.append(chr);
              } else if (chr == '_') {
                if (foundUnderscoreInNumber) {
                  throw new PrologParserException("Duplicated underscore char in integer",
                      this.prevLine, this.prevPos);
                } else {
                  foundUnderscoreInNumber = true;
                }
              } else {
                if (chr == '.' || chr == 'e' || chr == 'E') {
                  if (foundUnderscoreInNumber) {
                    throw new PrologParserException("Underscore is not allowed before E or dot",
                        this.prevLine, this.prevPos);
                  }

                  strBuffer.append(chr);
                  state = TokenizerState.FLOAT;
                } else {
                  if (foundUnderscoreInNumber) {
                    throw new PrologParserException("Unexpected underscore", this.prevLine,
                        this.prevPos);
                  }

                  if (chr == '\'') {
                    if (strBuffer.isSingleChar('0')) {
                      if (this.zeroSingleQuotationAllowed) {
                        state = STRING;
                        charCodeAsInt = true;
                        strBuffer.clear();
                      } else {
                        push(chr);
                        return new TokenizerResult(
                            makeTermFromString(strBuffer.toString(), radix, quoting, state),
                            TokenizerState.INTEGER,
                            getLastTokenLine(),
                            getLastTokenPos());
                      }
                    } else {
                      radix = Integer.parseInt(strBuffer.toString());
                      if (radix < 2 || radix > 36) {
                        throw new PrologParserException("Radix must be 2..36: " + radix,
                            getLastTokenLine(),
                            getLastTokenPos());
                      }
                      strBuffer.clear();
                    }
                  } else {
                    boolean radixCharFound = false;
                    if (radix == 10 && strBuffer.isSingleChar('0')) {
                      switch (chr) {
                        case 'x':
                          radixCharFound = true;
                          radix = 16;
                          detectedRadixChar = chr;
                          strBuffer.clear();
                          break;
                        case 'o':
                          radixCharFound = true;
                          radix = 8;
                          detectedRadixChar = chr;
                          strBuffer.clear();
                          break;
                        case 'b':
                          radixCharFound = true;
                          radix = 2;
                          detectedRadixChar = chr;
                          strBuffer.clear();
                          break;
                        default:
                          break;
                      }
                    }
                    if (!radixCharFound) {
                      push(chr);
                      if (strBuffer.isEmpty() && detectedRadixChar != ' ') {
                        push(detectedRadixChar);
                        strBuffer.append('0');
                      }
                      return new TokenizerResult(
                          makeTermFromString(strBuffer.toString(), radix, quoting, state),
                          TokenizerState.INTEGER,
                          getLastTokenLine(),
                          getLastTokenPos());
                    }
                  }
                }
              }
            }
            break;
            case FLOAT: {
              if (Character.isDigit(chr)) {
                foundUnderscoreInNumber = false;
                strBuffer.append(chr);
              } else if (chr == '_') {
                if (foundUnderscoreInNumber || strBuffer.isLastChar('.')) {
                  throw new PrologParserException("Underscore after dot in number: " + strBuffer,
                      this.prevLine, this.prevPos);
                } else {
                  foundUnderscoreInNumber = true;
                }
              } else {
                switch (chr) {
                  case '-':
                  case '+':
                    if (strBuffer.isLastChar('e')) {
                      strBuffer.append(chr);
                    } else {
                      push(chr);
                      return new TokenizerResult(
                          makeTermFromString(strBuffer.toString(), radix, quoting,
                              TokenizerState.FLOAT),
                          TokenizerState.FLOAT,
                          getLastTokenLine(),
                          getLastTokenPos());
                    }
                    break;
                  case 'e':
                  case 'E':
                    if (foundUnderscoreInNumber) {
                      throw new PrologParserException("Underscore is not allowed before E",
                          this.prevLine, this.prevPos);
                    }
                    if (strBuffer.lastIndexOf('e') < 0) {
                      strBuffer.append('e');
                    } else {
                      push(chr);
                      return new TokenizerResult(
                          makeTermFromString(strBuffer.toStringExcludeLastChar(), radix, quoting,
                              TokenizerState.FLOAT),
                          TokenizerState.FLOAT,
                          getLastTokenLine(),
                          getLastTokenPos());
                    }
                    break;
                  default:
                    push(chr);

                    if (foundUnderscoreInNumber) {
                      throw new PrologParserException("Unexpected underscore", this.prevLine,
                          this.prevPos);
                    }

                    if (strBuffer.isLastChar('.')) {
                      // it was an integer
                      push('.');
                      return new TokenizerResult(
                          makeTermFromString(strBuffer.toStringExcludeLastChar(), radix, quoting,
                              TokenizerState.INTEGER),
                          TokenizerState.INTEGER,
                          getLastTokenLine(),
                          getLastTokenPos());
                    } else {
                      // it is a float
                      if (!Character.isDigit(strBuffer.getLastChar())) {
                        throw new PrologParserException("Unexpected end of float: "
                            + strBuffer, this.prevLine, this.prevPos);
                      }
                      return new TokenizerResult(
                          makeTermFromString(strBuffer.toString(), radix, quoting, state),
                          state,
                          getLastTokenLine(),
                          getLastTokenPos()
                      );
                    }
                }
              }
            }
            break;
            case OPERATOR: {
              if (chr != '_' && letterOrDigitOnly != Character.isLetterOrDigit(chr)) {
                push(chr);

                if (lastFoundFullOperator == null || letterOrDigitOnly) {
                  final String textInBuffer = strBuffer.toString();

                  if (lastFoundFullOperator != null
                      && lastFoundFullOperator.getText().equals(textInBuffer)) {
                    return new TokenizerResult(
                        lastFoundFullOperator,
                        state,
                        getLastTokenLine(),
                        getLastTokenPos()
                    );
                  } else {
                    return new TokenizerResult(
                        makeTermFromString(textInBuffer, radix, quoting, ATOM),
                        ATOM,
                        getLastTokenLine(),
                        getLastTokenPos()
                    );
                  }
                } else {
                  calcDiffAndPushResultBack(
                      lastFoundFullOperator.getText(), strBuffer);
                  return new TokenizerResult(
                      lastFoundFullOperator,
                      state,
                      getLastTokenLine(),
                      getLastTokenPos()
                  );
                }
              } else {
                final OpContainer previouslyDetectedOperator = lastFoundFullOperator;

                strBuffer.append(chr);
                final String operator = strBuffer.toString();
                lastFoundFullOperator = findOperatorForName(operator);

                if (previouslyDetectedOperator == null) {
                  if (!hasOperatorStartsWith(operator)) {
                    if (hasOperatorStartsWith(String.valueOf(chr))) {
                      // next char can be the start char of an
                      // operator, so we need get back it into the
                      // buffer
                      strBuffer.pop();
                      push(chr);
                    }
                    state = TokenizerState.ATOM;
                  }
                } else {
                  if (lastFoundFullOperator == null) {
                    if (hasOperatorStartsWith(operator)) {
                      lastFoundFullOperator = previouslyDetectedOperator;
                    } else {
                      if (letterOrDigitOnly) {
                        state = TokenizerState.ATOM;
                      } else {
                        calcDiffAndPushResultBack(
                            previouslyDetectedOperator.getText(), strBuffer);
                        return new TokenizerResult(
                            previouslyDetectedOperator,
                            state, getLastTokenLine(),
                            getLastTokenPos()
                        );
                      }
                    }
                  } else {
                    if (!hasOperatorStartsWith(operator)) {
                      calcDiffAndPushResultBack(
                          previouslyDetectedOperator.getText(), strBuffer);
                      return new TokenizerResult(
                          previouslyDetectedOperator,
                          state,
                          getLastTokenLine(),
                          getLastTokenPos());
                    }
                  }
                }
              }
            }
            break;
            case STRING: {
              if (specCharDetected) {
                if (specCharBuffer.isEmpty() && chr == '\n') {
                  specCharDetected = false;

                  strBuffer.append('\n');
                  if (charCodeAsInt) {
                    return new TokenizerResult(
                        makeTermFromString(strBuffer.toString(), radix, quoting, state),
                        state,
                        getLastTokenLine(),
                        getLastTokenPos()
                    );
                  }
                } else if (!Character.isISOControl(chr)) {
                  specCharBuffer.append(chr);
                  final StringUtils.UnescapeResult result =
                      StringUtils.tryUnescapeCharacter(specCharBuffer);
                  if (result.isError()) {
                    throw new PrologParserException("Detected wrong escape char: \\"
                        + specCharBuffer, this.prevLine, this.prevPos);
                  } else if (!result.doesNeedMore()) {
                    specCharDetected = false;
                    if (charCodeAsInt) {
                      return new TokenizerResult(
                          new PrologInt(result.getDecoded()),
                          state,
                          getLastTokenLine(),
                          getLastTokenPos()
                      );
                    } else {
                      strBuffer.append(result.getDecoded());
                    }
                  }
                } else {
                  if (!(chr == '\r' && specCharBuffer.isEmpty())) {
                    throw new PrologParserException(
                        "Unexpected char: 0x" + Integer.toHexString(chr), this.prevLine,
                        this.prevPos);
                  }
                }
              } else {
                switch (chr) {
                  case '\'':
                    if (quoting == Quotation.SINGLE) {
                      return new TokenizerResult(
                          makeTermFromString(strBuffer.toString(), radix, quoting, state),
                          state,
                          getLastTokenLine(),
                          getLastTokenPos()
                      );
                    } else {
                      if (charCodeAsInt) {
                        throw new PrologParserException("Char ''' must be escaped in such case",
                            this.prevLine, this.prevPos);
                      } else {
                        strBuffer.append(chr);
                      }
                    }
                    break;
                  case '`':
                    if (quoting == Quotation.BACK_TICK) {
                      return new TokenizerResult(
                          makeTermFromString(strBuffer.toString(), radix, quoting, state),
                          state,
                          getLastTokenLine(),
                          getLastTokenPos()
                      );
                    } else {
                      if (charCodeAsInt) {
                        return new TokenizerResult(
                            new PrologInt(chr),
                            state,
                            getLastTokenLine(),
                            getLastTokenPos()
                        );
                      } else {
                        strBuffer.append(chr);
                      }
                    }
                    break;
                  case '\"':
                    if (quoting == Quotation.DOUBLE) {
                      return new TokenizerResult(
                          makeTermFromString(strBuffer.toString(), radix, quoting, state),
                          state,
                          getLastTokenLine(),
                          getLastTokenPos()
                      );
                    } else {
                      if (charCodeAsInt) {
                        return new TokenizerResult(
                            new PrologInt(chr),
                            state,
                            getLastTokenLine(),
                            getLastTokenPos()
                        );
                      } else {
                        strBuffer.append(chr);
                      }
                    }
                    break;
                  case '\\':
                    specCharDetected = true;
                    specCharBuffer.clear();
                    break;
                  default:
                    final char theChar;

                    if (Character.isISOControl(chr)) {
                      throw new PrologParserException(
                          "Unexpected control char: 0x" + Integer.toHexString(chr), this.prevLine,
                          this.prevPos);
                    } else {
                      theChar = chr;
                    }
                    if (charCodeAsInt) {
                      if (Character.isWhitespace(chr) && !this.zeroQuotationAllowsWhitespaceChar) {
                        throw new PrologParserException(
                            "Unexpected whitespace char: 0x" + Integer.toHexString(chr),
                            this.prevLine, this.prevPos);
                      } else {
                        return new TokenizerResult(
                            new PrologInt(theChar),
                            state,
                            getLastTokenLine(),
                            getLastTokenPos()
                        );
                      }
                    } else {
                      strBuffer.append(theChar);
                    }
                    break;
                }
              }
            }
            break;
            case VAR: {
              if (!isCharAllowedForUnquotedAtom(chr)) {
                push(chr);
                if (strBuffer.isSingleChar('_')) {
                  return new TokenizerResult(new PrologVar(), state, getLastTokenLine(),
                      getLastTokenPos());
                }
                return new TokenizerResult(new PrologVar(strBuffer.toString()), state,
                    getLastTokenLine(), getLastTokenPos());
              } else {
                strBuffer.append(chr);
              }
            }
            break;
            default:
              throw new CriticalUnexpectedError();
          }
        }
      }
    } catch (IOException ex) {
      throw new PrologParserException("IO exception during read char", this.prevLine, this.prevPos,
          ex);
    }
  }

  public PrologTerm makeTermFromString(final String str, final int radix,
                                       final Quotation quotingType, final TokenizerState state) {
    PrologTerm result;

    switch (state) {
      case INTEGER: {
        try {
          result = radix == 10 ? new PrologInt(str) : new PrologInt(new BigInteger(str, radix));
        } catch (NumberFormatException ex) {
          result = null;
        }
      }
      break;
      case FLOAT: {
        try {
          result = new PrologFloat(str);
        } catch (NumberFormatException ex) {
          result = null;
        }
      }
      break;
      default: {
        result = null;
      }
      break;
    }

    if (result == null) {
      result = new PrologAtom(str, quotingType);
    }

    return result;
  }

  public int getLine() {
    return this.line;
  }

  public int getPos() {
    return this.pos;
  }
}
