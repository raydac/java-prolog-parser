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
import static com.igormaznitsa.prologparser.ParserContext.FLAG_COMMENTS_AS_ATOMS;
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
public final class Tokenizer implements AutoCloseable {

  private final StringBuilderEx internalStringBuffer;
  private final StringBuilderEx specialCharBuffer;
  private final StringBuilderEx insideCharBuffer;
  private final boolean blockCommentsAllowed;
  private final boolean returnCommentsAsToken;
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

  public Tokenizer(
      final PrologParser parser,
      final Koi7CharOpMap metaOperators,
      final Reader reader
  ) {
    super();
    this.metaOperators = metaOperators;
    this.reader = reader;
    this.parser = parser;

    final int maxAllowedCharBufferSize = parser.getContext() == null ? Integer.MAX_VALUE :
        parser.getContext().getMaxTokenizerBufferLength();
    this.returnCommentsAsToken = parser.getContext() != null
        && ((parser.getContext().getFlags() & FLAG_COMMENTS_AS_ATOMS) != 0);
    this.blockCommentsAllowed = parser.getContext() != null
        && ((parser.getContext().getFlags() & FLAG_BLOCK_COMMENTS) != 0);
    this.zeroSingleQuotationAllowed = parser.getContext() != null
        && ((parser.getContext().getFlags() & FLAG_ZERO_QUOTATION_CHARCODE) != 0);
    this.zeroQuotationAllowsWhitespaceChar = parser.getContext() != null
        && ((parser.getContext().getFlags()
        & FLAG_ZERO_QUOTATION_ALLOWS_WHITESPACE_CHAR) != 0);

    this.internalStringBuffer = new StringBuilderEx(32, maxAllowedCharBufferSize);
    this.specialCharBuffer = new StringBuilderEx(8, maxAllowedCharBufferSize);
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

  @Override
  public void close() throws IOException {
    this.close(true);
  }

  /**
   * Close the tokenizer and clear internal buffers,
   *
   * @param closeReader true if underlying reader also should be closed, false otherwise
   * @throws IOException if any transport error during close
   */
  public void close(final boolean closeReader) throws IOException {
    try {
      if (this.reader instanceof PushbackReader) {
        final PushbackReader pushBackReader = (PushbackReader) this.reader;
        while (!this.internalStringBuffer.isEmpty()) {
          pushBackReader.unread(this.internalStringBuffer.pop());
        }
      }
    } finally {
      this.lastPushedTerm = null;
      this.internalStringBuffer.clear();

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
      result = this.readNextToken();
      this.push(result);
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

  private String skipTillBlockCommentEnd(final boolean accumulateText) throws IOException {
    final StringBuilder result = accumulateText ? new StringBuilder() : null;
    boolean starCharDetected = false;
    while (true) {
      final int readChar = this.doReadChar();
      if (readChar < 0) {
        break;
      } else if (readChar == '/') {
        if (starCharDetected) {
          if (accumulateText) {
            result.setLength(result.length() - 1);
          }
          break;
        } else {
          if (accumulateText) {
            result.append((char) readChar);
          }
        }
      } else {
        starCharDetected = readChar == '*';
        if (accumulateText) {
          result.append((char) readChar);
        }
      }
    }
    return accumulateText ? result.toString() : null;
  }

  private String skipTillNextLine(final boolean accumulateText) throws IOException {
    final StringBuilder result = accumulateText ? new StringBuilder() : null;

    while (true) {
      final int readChar = this.doReadChar();
      if (readChar < 0 || readChar == '\n') {
        break;
      }
      if (accumulateText) {
        result.append((char) readChar);
      }
    }
    return accumulateText ? result.toString() : null;
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
      return this.pop();
    }

    Quotation currentQuoting = Quotation.NONE;

    int currentRadix = 10;
    char detectedRadixChar = ' ';

    TokenizerState state = LOOK_FOR;
    boolean specCharDetected = false;
    boolean charCodeAsInt = false;

    this.internalStringBuffer.clear();
    this.specialCharBuffer.clear();

    final StringBuilderEx locInternalStringBuffer = this.internalStringBuffer;
    final StringBuilderEx locSpecialCharBuffer = this.specialCharBuffer;

    final boolean commentsAsAtoms = this.returnCommentsAsToken;

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
                throw new PrologParserException(
                    "Contains unexpected underscore: " + locInternalStringBuffer,
                    this.prevLine, this.prevPos);
              }

              if (state == TokenizerState.FLOAT && locInternalStringBuffer.isLastChar('.')) {
                // non-ended float then it is an integer number ended by the '.' operator
                push('.');
                // it is Integer
                return new TokenizerResult(
                    makeTermFromString(locInternalStringBuffer.toStringExcludeLastChar(),
                        currentRadix, currentQuoting,
                        TokenizerState.INTEGER),
                    TokenizerState.INTEGER,
                    getLastTokenLine(),
                    getLastTokenPos()
                );
              } else {
                // it is just integer number or an atom
                final String text = locInternalStringBuffer.toString();
                return new TokenizerResult(
                    makeTermFromString(text, currentRadix, PrologTerm.findQuotation(text), state),
                    state,
                    getLastTokenLine(),
                    getLastTokenPos()
                );
              }
            }
            case VAR: {
              if (locInternalStringBuffer.isSingleChar('_')) {
                return new TokenizerResult(
                    new PrologVar(),
                    state,
                    getLastTokenLine(),
                    getLastTokenPos()
                );
              } else {
                return new TokenizerResult(
                    new PrologVar(locInternalStringBuffer.toString()),
                    state,
                    getLastTokenLine(),
                    getLastTokenPos()
                );
              }
            }
            case STRING: {
              throw new PrologParserException("Non-completed string: " + locInternalStringBuffer,
                  this.lastTokenLine, this.lastTokenPos);
            }
            case OPERATOR: {
              if (lastFoundFullOperator == null) {
                return new TokenizerResult(
                    makeTermFromString(locInternalStringBuffer.toString(), currentRadix,
                        currentQuoting, state),
                    state,
                    getLastTokenLine(),
                    getLastTokenPos()
                );
              } else {
                calcDiffAndPushResultBack(lastFoundFullOperator.getText(), locInternalStringBuffer);
                return new TokenizerResult(
                    lastFoundFullOperator,
                    state,
                    getLastTokenLine(),
                    getLastTokenPos()
                );
              }
            }
            default: {
              throw new PrologParserException(
                  "Non-completed term (" + state + "): " + locInternalStringBuffer,
                  this.prevLine, this.prevPos);
            }
          }
        }

        final char chr = (char) readChar;

        if (state != STRING
            && this.blockCommentsAllowed
            && chr == '*'
            && this.internalStringBuffer.isLastChar('/')
        ) {
          if (this.internalStringBuffer.isSingleChar('/')) {
            this.internalStringBuffer.pop();
            state = this.internalStringBuffer.isEmpty() ? LOOK_FOR : state;
          } else if (state == OPERATOR) {
            throw new PrologParserException("Operator can be mixed with comment block: "
                + this.internalStringBuffer + chr, this.getLastTokenLine(), this.getLastTokenPos());
          }

          if (commentsAsAtoms) {
            final String commentText = this.skipTillBlockCommentEnd(true);
            return new TokenizerResult(
                new PrologAtom(commentText, Quotation.COMMENT_BLOCK),
                state,
                this.getLastTokenLine(),
                this.getLastTokenPos()
            );
          } else {
            this.skipTillBlockCommentEnd(false);
          }
        } else {
          switch (state) {
            case LOOK_FOR: {
              if (Character.isWhitespace(chr) || Character.isISOControl(chr)) {
                continue;
              }

              switch (chr) {
                case '%': {
                  this.fixPosition();
                  final String text = skipTillNextLine(commentsAsAtoms);
                  if (commentsAsAtoms) {
                    return new TokenizerResult(
                        new PrologAtom(text, Quotation.COMMENT_LINE),
                        state,
                        this.getLastTokenLine(),
                        this.getLastTokenPos()
                    );
                  }
                }
                break;
                case '_': {
                  fixPosition();
                  locInternalStringBuffer.append(chr);
                  state = TokenizerState.VAR;
                }
                break;
                case '\'': {
                  fixPosition();
                  currentQuoting = Quotation.SINGLE;
                  state = TokenizerState.STRING;
                }
                break;
                case '\"': {
                  fixPosition();
                  currentQuoting = Quotation.DOUBLE;
                  state = TokenizerState.STRING;
                }
                break;
                case '`': {
                  fixPosition();
                  currentQuoting = Quotation.BACK_TICK;
                  state = TokenizerState.STRING;
                }
                break;

                default: {
                  fixPosition();

                  locInternalStringBuffer.append(chr);

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
                locInternalStringBuffer.append(chr);
              } else if (Character.isISOControl(chr) || Character.isWhitespace(chr)) {
                final String text = locInternalStringBuffer.toString();

                if (currentQuoting == Quotation.NONE) {
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
                    makeTermFromString(text, currentRadix, PrologTerm.findQuotation(text), state),
                    state,
                    getLastTokenLine(),
                    getLastTokenPos());
              } else if ((chr == '\'' || chr == '\"' || chr == '`')
                  || (letterOrDigitOnly != Character.isLetterOrDigit(chr))
                  || (!Character.isLetter(chr) && findOperatorForSingleChar(chr) != null)) {
                push(chr);
                final String text = locInternalStringBuffer.toString();

                if (currentQuoting == Quotation.NONE) {
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
                    makeTermFromString(text, currentRadix, PrologTerm.findQuotation(text), state),
                    state,
                    getLastTokenLine(),
                    getLastTokenPos());

              } else {
                locInternalStringBuffer.append(chr);
              }
            }
            break;
            case INTEGER: {
              if (isCharAllowedForRadix(chr, currentRadix)) {
                foundUnderscoreInNumber = false;
                locInternalStringBuffer.append(chr);
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

                  locInternalStringBuffer.append(chr);
                  state = TokenizerState.FLOAT;
                } else {
                  if (foundUnderscoreInNumber) {
                    throw new PrologParserException("Unexpected underscore", this.prevLine,
                        this.prevPos);
                  }

                  if (chr == '\'') {
                    if (locInternalStringBuffer.isSingleChar('0')) {
                      if (this.zeroSingleQuotationAllowed) {
                        state = STRING;
                        charCodeAsInt = true;
                        locInternalStringBuffer.clear();
                      } else {
                        push(chr);
                        return new TokenizerResult(
                            makeTermFromString(locInternalStringBuffer.toString(), currentRadix,
                                currentQuoting, state),
                            TokenizerState.INTEGER,
                            getLastTokenLine(),
                            getLastTokenPos());
                      }
                    } else {
                      currentRadix = Integer.parseInt(locInternalStringBuffer.toString());
                      if (currentRadix < 2 || currentRadix > 36) {
                        throw new PrologParserException("Radix must be 2..36: " + currentRadix,
                            getLastTokenLine(),
                            getLastTokenPos());
                      }
                      locInternalStringBuffer.clear();
                    }
                  } else {
                    boolean radixCharFound = false;
                    if (currentRadix == 10 && locInternalStringBuffer.isSingleChar('0')) {
                      switch (chr) {
                        case 'x':
                          radixCharFound = true;
                          currentRadix = 16;
                          detectedRadixChar = chr;
                          locInternalStringBuffer.clear();
                          break;
                        case 'o':
                          radixCharFound = true;
                          currentRadix = 8;
                          detectedRadixChar = chr;
                          locInternalStringBuffer.clear();
                          break;
                        case 'b':
                          radixCharFound = true;
                          currentRadix = 2;
                          detectedRadixChar = chr;
                          locInternalStringBuffer.clear();
                          break;
                        default:
                          break;
                      }
                    }
                    if (!radixCharFound) {
                      push(chr);
                      if (locInternalStringBuffer.isEmpty() && detectedRadixChar != ' ') {
                        push(detectedRadixChar);
                        locInternalStringBuffer.append('0');
                      }
                      return new TokenizerResult(
                          makeTermFromString(locInternalStringBuffer.toString(), currentRadix,
                              currentQuoting, state),
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
                locInternalStringBuffer.append(chr);
              } else if (chr == '_') {
                if (foundUnderscoreInNumber || locInternalStringBuffer.isLastChar('.')) {
                  throw new PrologParserException(
                      "Underscore after dot in number: " + locInternalStringBuffer,
                      this.prevLine, this.prevPos);
                } else {
                  foundUnderscoreInNumber = true;
                }
              } else {
                switch (chr) {
                  case '-':
                  case '+':
                    if (locInternalStringBuffer.isLastChar('e')) {
                      locInternalStringBuffer.append(chr);
                    } else {
                      push(chr);
                      return new TokenizerResult(
                          makeTermFromString(locInternalStringBuffer.toString(), currentRadix,
                              currentQuoting,
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
                    if (locInternalStringBuffer.lastIndexOf('e') < 0) {
                      locInternalStringBuffer.append('e');
                    } else {
                      push(chr);
                      return new TokenizerResult(
                          makeTermFromString(locInternalStringBuffer.toStringExcludeLastChar(),
                              currentRadix, currentQuoting,
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

                    if (locInternalStringBuffer.isLastChar('.')) {
                      // it was an integer
                      push('.');
                      return new TokenizerResult(
                          makeTermFromString(locInternalStringBuffer.toStringExcludeLastChar(),
                              currentRadix, currentQuoting,
                              TokenizerState.INTEGER),
                          TokenizerState.INTEGER,
                          getLastTokenLine(),
                          getLastTokenPos());
                    } else {
                      // it is a float
                      if (!Character.isDigit(locInternalStringBuffer.getLastChar())) {
                        throw new PrologParserException("Unexpected end of float: "
                            + locInternalStringBuffer, this.prevLine, this.prevPos);
                      }
                      return new TokenizerResult(
                          makeTermFromString(locInternalStringBuffer.toString(), currentRadix,
                              currentQuoting, state),
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
                  final String textInBuffer = locInternalStringBuffer.toString();

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
                        makeTermFromString(textInBuffer, currentRadix, currentQuoting, ATOM),
                        ATOM,
                        getLastTokenLine(),
                        getLastTokenPos()
                    );
                  }
                } else {
                  calcDiffAndPushResultBack(
                      lastFoundFullOperator.getText(), locInternalStringBuffer);
                  return new TokenizerResult(
                      lastFoundFullOperator,
                      state,
                      getLastTokenLine(),
                      getLastTokenPos()
                  );
                }
              } else {
                final OpContainer previouslyDetectedOperator = lastFoundFullOperator;

                locInternalStringBuffer.append(chr);
                final String operator = locInternalStringBuffer.toString();
                lastFoundFullOperator = findOperatorForName(operator);

                if (previouslyDetectedOperator == null) {
                  if (!hasOperatorStartsWith(operator)) {
                    if (hasOperatorStartsWith(String.valueOf(chr))) {
                      // next char can be the start char of an
                      // operator, so we need get back it into the
                      // buffer
                      locInternalStringBuffer.pop();
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
                            previouslyDetectedOperator.getText(), locInternalStringBuffer);
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
                          previouslyDetectedOperator.getText(), locInternalStringBuffer);
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
                if (locSpecialCharBuffer.isEmpty() && chr == '\n') {
                  specCharDetected = false;

                  locInternalStringBuffer.append('\n');
                  if (charCodeAsInt) {
                    return new TokenizerResult(
                        makeTermFromString(locInternalStringBuffer.toString(), currentRadix,
                            currentQuoting, state),
                        state,
                        getLastTokenLine(),
                        getLastTokenPos()
                    );
                  }
                } else if (!Character.isISOControl(chr)) {
                  locSpecialCharBuffer.append(chr);
                  final StringUtils.UnescapeResult result =
                      StringUtils.tryUnescapeCharacter(locSpecialCharBuffer);
                  if (result.isError()) {
                    throw new PrologParserException("Detected wrong escape char: \\"
                        + locSpecialCharBuffer, this.prevLine, this.prevPos);
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
                      locInternalStringBuffer.append(result.getDecoded());
                    }
                  }
                } else {
                  if (!(chr == '\r' && locSpecialCharBuffer.isEmpty())) {
                    throw new PrologParserException(
                        "Unexpected char: 0x" + Integer.toHexString(chr), this.prevLine,
                        this.prevPos);
                  }
                }
              } else {
                switch (chr) {
                  case '\'':
                    if (currentQuoting == Quotation.SINGLE) {
                      return new TokenizerResult(
                          makeTermFromString(locInternalStringBuffer.toString(), currentRadix,
                              currentQuoting, state),
                          state,
                          getLastTokenLine(),
                          getLastTokenPos()
                      );
                    } else {
                      if (charCodeAsInt) {
                        throw new PrologParserException("Char ''' must be escaped in such case",
                            this.prevLine, this.prevPos);
                      } else {
                        locInternalStringBuffer.append(chr);
                      }
                    }
                    break;
                  case '`':
                    if (currentQuoting == Quotation.BACK_TICK) {
                      return new TokenizerResult(
                          makeTermFromString(locInternalStringBuffer.toString(), currentRadix,
                              currentQuoting, state),
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
                        locInternalStringBuffer.append(chr);
                      }
                    }
                    break;
                  case '\"':
                    if (currentQuoting == Quotation.DOUBLE) {
                      return new TokenizerResult(
                          makeTermFromString(locInternalStringBuffer.toString(), currentRadix,
                              currentQuoting, state),
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
                        locInternalStringBuffer.append(chr);
                      }
                    }
                    break;
                  case '\\':
                    specCharDetected = true;
                    locSpecialCharBuffer.clear();
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
                      locInternalStringBuffer.append(theChar);
                    }
                    break;
                }
              }
            }
            break;
            case VAR: {
              if (!isCharAllowedForUnquotedAtom(chr)) {
                push(chr);
                if (locInternalStringBuffer.isSingleChar('_')) {
                  return new TokenizerResult(new PrologVar(), state, getLastTokenLine(),
                      getLastTokenPos());
                }
                return new TokenizerResult(new PrologVar(locInternalStringBuffer.toString()), state,
                    getLastTokenLine(), getLastTokenPos());
              } else {
                locInternalStringBuffer.append(chr);
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
