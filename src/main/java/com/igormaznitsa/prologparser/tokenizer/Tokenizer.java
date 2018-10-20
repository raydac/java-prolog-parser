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

import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.terms.OpContainer;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologFloat;
import com.igormaznitsa.prologparser.terms.PrologInteger;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.PrologVariable;
import com.igormaznitsa.prologparser.utils.SoftObjectPool;
import com.igormaznitsa.prologparser.utils.StringBuilderEx;
import com.igormaznitsa.prologparser.utils.StringUtils;

import java.io.IOException;
import java.io.Reader;

import static com.igormaznitsa.prologparser.tokenizer.TokenizerState.LOOKFOR;
import static com.igormaznitsa.prologparser.tokenizer.TokenizerState.STRING;

final class Tokenizer {

  private final StringBuilderEx strBuf = new StringBuilderEx(32);
  private final StringBuilderEx specCharBuf = new StringBuilderEx(8);
  private final StringBuilderEx insideCharBuffer = new StringBuilderEx(8);
  private final boolean blockCommentsAllowed;
  private final Reader reader;
  private final PrologParser parser;
  private final SoftObjectPool<TokenizerResult> tokenizerResultPool;
  private TokenizerResult lastPushedTerm;
  private int prevTokenLine;
  private int prevTokenPos;
  private int lastTokenLine;
  private int lastTokenPos;
  private int prevPos;
  private int prevLine;
  private int pos;
  private int line;

  Tokenizer(final PrologParser parser, final Reader reader) {
    super();
    this.reader = reader;
    this.parser = parser;
    this.blockCommentsAllowed = parser.context != null && ((parser.context.getFlags() & ParserContext.FLAG_BLOCK_COMMENTS) != 0);

    this.pos = 1;
    this.line = 1;
    this.prevPos = 1;
    this.prevLine = 1;

    this.tokenizerResultPool = new SoftObjectPool<TokenizerResult>(8) {
      @Override
      public final TokenizerResult get() {
        return new TokenizerResult(this);
      }
    };
    this.tokenizerResultPool.fill();
  }

  TokenizerResult getLastPushed() {
    return this.lastPushedTerm;
  }

  int readChar() throws IOException {
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

  private void calcDiffAndPushResultBack(final String etalon, final StringBuilderEx buffer) {
    int chars = buffer.length() - etalon.length();
    int bufferPosition = buffer.length() - 1;

    int lpos = this.pos;
    int lposPrev = this.prevPos;
    int lline = this.line;
    int llinePrev = this.prevLine;

    while (chars > 0) {
      final char ch = buffer.charAt(bufferPosition--);
      this.insideCharBuffer.push(ch);
      chars--;
      lpos = Math.max(1, lpos - 1);

      lposPrev = lpos;
      if (ch == '\n') {
        lline = Math.max(1, lline - 1);
        llinePrev = lline;
      }
    }

    this.pos = lpos;
    this.prevPos = lposPrev;
    this.line = lline;
    this.prevLine = llinePrev;
  }

  private void push(final char ch) {
    this.insideCharBuffer.push(ch);
    if (ch == '\n') {
      this.pos = 1;
      this.line = Math.max(1, this.line - 1);
    } else {
      this.pos = Math.max(1, this.pos - 1);
    }
  }

  void close() throws IOException {
    this.lastPushedTerm = null;
    this.strBuf.clear();
    this.reader.close();
  }

  boolean hasOperatorStartsWith(final String operatorNameStartSubstring) {
    boolean result = false;
    if (PrologParser.SYSTEM_OPERATORS_PREFIXES.contains(operatorNameStartSubstring)) {
      result = true;
    } else if (parser != null) {
      final ParserContext ctx = parser.getContext();
      if (ctx != null) {
        result = ctx.hasOperatorStartsWith(parser, operatorNameStartSubstring);
      }
    }
    return result;
  }

  OpContainer findOperatorForName(final String operatorName) {
    OpContainer result = null;

    // check metaoperators as the first ones
    if (operatorName.length() == 1) {
      result = PrologParser.META_SYSTEM_OPERATORS.get(operatorName);
    }
    if (result == null) {
      // check user defined operators because a user can replace a system operator
      final ParserContext ctx = parser.getContext();
      if (ctx != null) {
        result = ctx.findOperatorForName(parser, operatorName);
      }

      // check system operators
      if (result == null) {
        result = PrologParser.SYSTEM_OPERATORS.get(operatorName);
      }
    }

    return result;
  }

  private OpContainer findOperatorForSingleChar(final char c) {
    OpContainer result = PrologParser.META_SYSTEM_OPERATORS.get(c);
    if (result == null) {
      return findOperatorForName(String.valueOf(c));
    }
    return result;
  }

  void push(final TokenizerResult object) {
    if (this.lastPushedTerm != null) {
      throw new IllegalStateException("There is already pushed term");
    }
    this.lastPushedTerm = object;
  }

  TokenizerResult peek() {
    TokenizerResult result;
    if (lastPushedTerm == null) {
      result = readNextToken();
      push(result);
    } else {
      result = this.lastPushedTerm;
    }
    return result;
  }

  int getLastTokenPos() {
    return this.lastPushedTerm == null ? this.lastTokenPos : this.prevTokenPos;
  }

  int getLastTokenLine() {
    return this.lastPushedTerm == null ? this.lastTokenLine : this.prevTokenLine;
  }

  private void fixPosition() {
    this.prevTokenLine = this.lastTokenLine;
    this.prevTokenPos = this.lastTokenPos;
    this.lastTokenLine = this.line;
    this.lastTokenPos = this.pos - 1;
  }

  private void skipUntilBlockCommentEnd() throws IOException {
    boolean star = false;
    while (!Thread.currentThread().isInterrupted()) {
      final int readchar = this.readChar();
      if (readchar < 0) {
        break;
      } else if (readchar == '/' && star) {
        break;
      } else if (readchar == '*') {
        star = true;
      } else {
        star = false;
      }
    }
  }

  private void skipUntilNextString() throws IOException {
    while (!Thread.currentThread().isInterrupted()) {
      final int readchar = this.readChar();
      if (readchar < 0 || readchar == '\n') {
        break;
      }
    }
  }

  /**
   * Read next token
   *
   * @return next token or null if not found or thread interruption detected
   */
  TokenizerResult readNextToken() {

    if (this.lastPushedTerm != null) {
      try {
        return lastPushedTerm;
      } finally {
        lastPushedTerm = null;
      }
    }

    PrologTerm.QuotingType quoting = PrologTerm.QuotingType.NO_QUOTED;

    TokenizerState state = LOOKFOR;
    boolean specCharDetected = false;

    strBuf.clear();
    specCharBuf.clear();

    final StringBuilderEx strBuffer = this.strBuf;
    final StringBuilderEx specCharBuffer = this.specCharBuf;

    OpContainer lastFoundFullOperator = null;

    boolean letterOrDigitOnly = false;

    boolean foundUnderlineInNumeric = false;

    try {
      while (!Thread.currentThread().isInterrupted()) {
        final int readChar = this.readChar();

        if (readChar < 0) {
          switch (state) {
            case LOOKFOR:
              return null;
            case FLOAT:
            case INTEGER:
            case ATOM: {
              if (foundUnderlineInNumeric) {
                throw new PrologParserException("Unexpected low line", this.prevLine, this.prevPos);
              }

              if (state == TokenizerState.FLOAT && strBuffer.isLastChar('.')) {
                // non-ended float then it is an integer number ened by the '.' operator
                push('.');
                // it is Integer
                return this.tokenizerResultPool.find().setData(
                    makeTermFromString(strBuffer.toStringExcludeLastChar(), quoting, TokenizerState.INTEGER),
                    TokenizerState.ATOM,
                    getLastTokenLine(),
                    getLastTokenPos()
                );
              } else {
                // it is just integer number or an atom
                final String text = strBuffer.toString();
                return this.tokenizerResultPool.find().setData(
                    makeTermFromString(text, PrologTerm.findAppropriateQuoting(text), state),
                    state,
                    getLastTokenLine(),
                    getLastTokenPos()
                );
              }
            }
            case VAR: {
              if (strBuffer.isSingleChar('_')) {
                return this.tokenizerResultPool.find().setData(
                    new PrologVariable(),
                    state,
                    getLastTokenLine(),
                    getLastTokenPos()
                );
              } else {
                return this.tokenizerResultPool.find().setData(
                    new PrologVariable(strBuffer.toString()),
                    state,
                    getLastTokenLine(),
                    getLastTokenPos()
                );
              }
            }
            case STRING: {
              throw new PrologParserException("Non-closed string", this.lastTokenLine, this.lastTokenPos);
            }
            case OPERATOR: {
              if (lastFoundFullOperator == null) {
                return this.tokenizerResultPool.find().setData(
                    makeTermFromString(strBuffer.toString(), quoting, state),
                    state,
                    getLastTokenLine(),
                    getLastTokenPos()
                );
              } else {
                calcDiffAndPushResultBack(lastFoundFullOperator.getTermText(), strBuffer);
                return this.tokenizerResultPool.find().setData(
                    lastFoundFullOperator,
                    state,
                    getLastTokenLine(),
                    getLastTokenPos()
                );
              }
            }
            default: {
              throw new CriticalUnexpectedError();
            }
          }
        }

        final char chr = (char) readChar;

        if (state != STRING && this.blockCommentsAllowed && chr == '*' && this.strBuf.isLastChar('/')) {
          if (this.strBuf.isSingleChar('/')) {
            this.strBuf.pop();
            state = this.strBuf.isEmpty() ? LOOKFOR : state;
          }
          skipUntilBlockCommentEnd();
        } else {
          switch (state) {
            case LOOKFOR: {
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
                  quoting = PrologTerm.QuotingType.SINGLE_QUOTED;
                  state = TokenizerState.STRING;
                }
                break;
                case '\"': {
                  fixPosition();
                  quoting = PrologTerm.QuotingType.DOUBLE_QUOTED;
                  state = TokenizerState.STRING;
                }
                break;
                case '`': {
                  fixPosition();
                  quoting = PrologTerm.QuotingType.BACK_QUOTED;
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
                return this.tokenizerResultPool.find().setData(makeTermFromString(text, PrologTerm.findAppropriateQuoting(text), state), state, getLastTokenLine(), getLastTokenPos());
              } else if (chr == '\''
                  || (letterOrDigitOnly != Character.isLetterOrDigit(chr))
                  || findOperatorForSingleChar(chr) != null) {
                push(chr);
                final String text = strBuffer.toString();
                return this.tokenizerResultPool.find().setData(
                    makeTermFromString(text, PrologTerm.findAppropriateQuoting(text), state),
                    state,
                    getLastTokenLine(),
                    getLastTokenPos());
              } else {
                strBuffer.append(chr);
              }
            }
            break;
            case INTEGER: {
              if (Character.isDigit(chr)) {
                foundUnderlineInNumeric = false;
                strBuffer.append(chr);
              } else if (chr == '_') {
                if (foundUnderlineInNumeric) {
                  throw new PrologParserException("Duplicated low line in integer", this.prevLine, this.prevPos);
                } else {
                  foundUnderlineInNumeric = true;
                }
              } else {
                if (chr == '.' || chr == 'e' || chr == 'E') {
                  if (foundUnderlineInNumeric) {
                    throw new PrologParserException("Low line is not allowed before E or dot", this.prevLine, this.prevPos);
                  }

                  strBuffer.append(chr);
                  state = TokenizerState.FLOAT;
                } else {
                  if (foundUnderlineInNumeric) {
                    throw new PrologParserException("Unexpected low line", this.prevLine, this.prevPos);
                  }

                  push(chr);

                  return this.tokenizerResultPool.find().setData(
                      makeTermFromString(strBuffer.toString(), quoting, state),
                      TokenizerState.INTEGER,
                      getLastTokenLine(),
                      getLastTokenPos());
                }
              }
            }
            break;
            case FLOAT: {
              if (Character.isDigit(chr)) {
                foundUnderlineInNumeric = false;
                strBuffer.append(chr);
              } else if (chr == '_') {
                if (foundUnderlineInNumeric || strBuffer.isLastChar('.')) {
                  throw new PrologParserException("Low line is not allowed before dot", this.prevLine, this.prevPos);
                } else {
                  foundUnderlineInNumeric = true;
                }
              } else {
                if (chr == '-' || chr == '+') {
                  if (strBuffer.isLastChar('e')) {
                    strBuffer.append(chr);
                  } else {
                    push(chr);
                    return this.tokenizerResultPool.find().setData(
                        makeTermFromString(strBuffer.toString(), quoting, TokenizerState.FLOAT),
                        TokenizerState.FLOAT,
                        getLastTokenLine(),
                        getLastTokenPos());
                  }
                } else if (chr == 'e' || chr == 'E') {
                  if (foundUnderlineInNumeric) {
                    throw new PrologParserException("Low line is not allowed before E", this.prevLine, this.prevPos);
                  }

                  if (strBuffer.lastIndexOf('e') < 0) {
                    strBuffer.append('e');
                  } else {
                    push(chr);
                    return this.tokenizerResultPool.find().setData(
                        makeTermFromString(strBuffer.toStringExcludeLastChar(), quoting, TokenizerState.FLOAT),
                        TokenizerState.FLOAT,
                        getLastTokenLine(),
                        getLastTokenPos());
                  }
                } else {

                  push(chr);

                  if (foundUnderlineInNumeric) {
                    throw new PrologParserException("Unexpected low line char", this.prevLine, this.prevPos);
                  }

                  if (strBuffer.isLastChar('.')) {
                    // it was an integer
                    push('.');
                    return this.tokenizerResultPool.find().setData(
                        makeTermFromString(strBuffer.toStringExcludeLastChar(), quoting, TokenizerState.INTEGER),
                        TokenizerState.INTEGER,
                        getLastTokenLine(),
                        getLastTokenPos());
                  } else {
                    // it is float
                    return this.tokenizerResultPool.find().setData(
                        makeTermFromString(strBuffer.toString(), quoting, state),
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

                if (lastFoundFullOperator == null) {
                  return this.tokenizerResultPool.find().setData(
                      makeTermFromString(strBuffer.toString(), quoting, state),
                      state,
                      getLastTokenLine(),
                      getLastTokenPos()
                  );
                } else {
                  return this.tokenizerResultPool.find().setData(
                      lastFoundFullOperator,
                      state,
                      getLastTokenLine(),
                      getLastTokenPos()
                  );
                }
              } else {
                final OpContainer previousleDetectedOperator = lastFoundFullOperator;
                strBuffer.append(chr);
                final String operator = strBuffer.toString();
                lastFoundFullOperator = findOperatorForName(operator);
                if (previousleDetectedOperator == null) {
                  if (!hasOperatorStartsWith(operator)) {
                    if (hasOperatorStartsWith(String.valueOf(chr))) {
                      // next char can be the start char of an
                      // operator so we need get back it into the
                      // buffer
                      strBuffer.pop();
                      push(chr);
                    }
                    state = TokenizerState.ATOM;
                  }
                } else {
                  if (lastFoundFullOperator == null) {
                    if (hasOperatorStartsWith(operator)) {
                      lastFoundFullOperator = previousleDetectedOperator;
                    } else {
                      if (letterOrDigitOnly) {
                        state = TokenizerState.ATOM;
                      } else {
                        calcDiffAndPushResultBack(
                            previousleDetectedOperator.getTermText(), strBuffer);
                        return this.tokenizerResultPool.find().setData(
                            previousleDetectedOperator,
                            state, getLastTokenLine(),
                            getLastTokenPos()
                        );
                      }
                    }
                  } else {
                    if (!hasOperatorStartsWith(operator)) {
                      calcDiffAndPushResultBack(
                          previousleDetectedOperator.getTermText(), strBuffer);
                      return this.tokenizerResultPool.find().setData(
                          previousleDetectedOperator,
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
                  strBuffer.append('\n');
                  specCharDetected = false;
                } else if (!Character.isISOControl(chr)) {
                  specCharBuffer.append(chr);
                  final StringUtils.UnescapeResult result = StringUtils.tryUnescapeCharacter(specCharBuffer);
                  if (result.isError()) {
                    throw new PrologParserException("Detected wrong escape char: \\" + specCharBuffer.toString(), this.prevLine, this.prevPos);
                  } else if (!result.doesNeedMore()) {
                    strBuffer.append(result.getDecoded());
                    specCharDetected = false;
                  }
                }
              } else {
                switch (chr) {
                  case '\'':
                    if (quoting == PrologTerm.QuotingType.SINGLE_QUOTED) {
                      return this.tokenizerResultPool.find().setData(
                          makeTermFromString(strBuffer.toString(), quoting, state),
                          state,
                          getLastTokenLine(),
                          getLastTokenPos()
                      );
                    } else {
                      strBuffer.append(chr);
                    }
                    break;
                  case '`':
                    if (quoting == PrologTerm.QuotingType.BACK_QUOTED) {
                      return this.tokenizerResultPool.find().setData(
                          makeTermFromString(strBuffer.toString(), quoting, state),
                          state,
                          getLastTokenLine(),
                          getLastTokenPos()
                      );
                    } else {
                      strBuffer.append(chr);
                    }
                    break;
                  case '\"':
                    if (quoting == PrologTerm.QuotingType.DOUBLE_QUOTED) {
                      return this.tokenizerResultPool.find().setData(
                          makeTermFromString(strBuffer.toString(), quoting, state),
                          state,
                          getLastTokenLine(),
                          getLastTokenPos()
                      );
                    } else {
                      strBuffer.append(chr);
                    }
                    break;
                  case '\\':
                    specCharDetected = true;
                    specCharBuffer.clear();
                    break;
                  default:
                    if (Character.isISOControl(chr)) {
                      strBuffer.append(StringUtils.isAllowedEscapeChar(chr) ? chr : '→');
                    } else {
                      strBuffer.append(chr);
                    }
                    break;
                }
              }
            }
            break;
            case VAR: {
              if (Character.isWhitespace(chr) || Character.isISOControl(chr)) {
                if (strBuffer.isSingleChar('_')) {
                  return this.tokenizerResultPool.find().setData(new PrologVariable(), state, getLastTokenLine(), getLastTokenPos());
                }
                return this.tokenizerResultPool.find().setData(new PrologVariable(strBuffer.toString()), state, getLastTokenLine(), getLastTokenPos());
              } else if (!StringUtils.isCharAllowedForUnquotedAtom(chr)) {
                push(chr);
                if (strBuffer.isSingleChar('_')) {
                  return this.tokenizerResultPool.find().setData(new PrologVariable(), state, getLastTokenLine(), getLastTokenPos());
                }
                return this.tokenizerResultPool.find().setData(new PrologVariable(strBuffer.toString()), state, getLastTokenLine(), getLastTokenPos());
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
      return null;
    } catch (IOException ex) {
      throw new PrologParserException("IO exception during read char", this.prevLine, this.prevPos, ex);
    }
  }


  PrologTerm makeTermFromString(final String str, final PrologTerm.QuotingType quotingType, final TokenizerState state) {
    PrologTerm result;

    switch (state) {
      case INTEGER: {
        try {
          result = new PrologInteger(str);
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

  int getLine() {
    return this.line;
  }

  int getPos() {
    return this.pos;
  }
}
