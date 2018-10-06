package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.OpContainer;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologFloat;
import com.igormaznitsa.prologparser.terms.PrologInteger;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.PrologVariable;
import com.igormaznitsa.prologparser.utils.StringBuilderEx;
import com.igormaznitsa.prologparser.utils.StringUtils;

import java.io.IOException;
import java.io.Reader;

final class Tokenizer {

  private final StringBuilderEx strBuf = new StringBuilderEx(128);
  private final StringBuilderEx specCharBuf = new StringBuilderEx(128);
  private final StringBuilderEx insideCharBuffer = new StringBuilderEx(32);
  private final Reader reader;
  private final PrologParser parser;
  TokenizerResult lastPushedTerm;
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

    this.pos = 1;
    this.line = 1;
    this.prevPos = 1;
    this.prevLine = 1;
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

  void calcDiffAndPushResultBack(final String etalon, final StringBuilderEx buffer) {
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
      lpos--;
      if (lpos < 1) {
        lpos = 1;
      }
      lposPrev = lpos;
      if (ch == '\n') {
        lline--;
        if (lline < 1) {
          lline = 1;
        }
        llinePrev = lline;
      }
    }

    this.pos = lpos;
    this.prevPos = lposPrev;
    this.line = lline;
    this.prevLine = llinePrev;
  }

  void push(final char ch) {
    this.insideCharBuffer.push(ch);
    if (ch == '\n') {
      this.pos = 1;
      this.line--;
      if (this.line <= 0) {
        this.line = 1;
      }
    } else {
      this.pos--;
      if (this.pos <= 0) {
        this.pos = 1;
      }
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

  OpContainer findOperatorForSingleChar(final char c) {
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
      result = lastPushedTerm;
    }
    return result;
  }

  int getLastTokenPos() {
    return this.lastPushedTerm == null ? this.lastTokenPos : this.prevTokenPos;
  }

  int getLastTokenLine() {
    return this.lastPushedTerm == null ? this.lastTokenLine : this.prevTokenLine;
  }

  void fixPosition() {
    this.prevTokenLine = this.lastTokenLine;
    this.prevTokenPos = this.lastTokenPos;
    this.lastTokenLine = this.line;
    this.lastTokenPos = this.pos - 1;
  }

  void skipUntilNextString() throws IOException {
    while (true) {
      final int readchar = this.readChar();
      if (readchar < 0 || readchar == '\n') {
        break;
      }
    }
  }

  TokenizerResult readNextToken() {

    if (this.lastPushedTerm != null) {
      try {
        return lastPushedTerm;
      } finally {
        lastPushedTerm = null;
      }
    }

    TokenizerState state = TokenizerState.LOOKFOR;
    boolean specCharDetected = false;

    strBuf.clear();
    specCharBuf.clear();

    final StringBuilderEx strBuffer = this.strBuf;
    final StringBuilderEx specCharBuffer = this.specCharBuf;

    OpContainer lastFoundFullOperator = null;

    boolean letterOrDigitOnly = false;

    try {
      while (true) {
        final int readChar = this.readChar();

        if (readChar < 0) {
          switch (state) {
            case LOOKFOR:
              return null;
            case FLOAT:
            case INTEGER:
            case ATOM: {
              if (state == TokenizerState.FLOAT && strBuffer.isLastChar('.')) {
                // non-ended float then it is an integer number ened by the '.' operator
                push('.');
                // it is Integer
                return new TokenizerResult(makeTermFromString(
                    strBuffer.toStringExcludeLastChar(),
                    TokenizerState.INTEGER), TokenizerState.ATOM, getLastTokenLine(), getLastTokenPos());
              } else {
                // it is just integer number or an atom
                return new TokenizerResult(makeTermFromString(strBuffer.toString(), state), state, getLastTokenLine(), getLastTokenPos());
              }
            }
            case VAR: {
              if (strBuffer.isSingleChar('_')) {
                return new TokenizerResult(new PrologVariable(), state, getLastTokenLine(), getLastTokenPos());
              } else {
                return new TokenizerResult(new PrologVariable(strBuffer.toString()), state, getLastTokenLine(), getLastTokenPos());
              }
            }
            case STRING: {
              throw new PrologParserException("Unclosed string found",
                  lastTokenLine, lastTokenPos);
            }
            case OPERATOR: {
              if (lastFoundFullOperator == null) {
                return new TokenizerResult(makeTermFromString(strBuffer.toString(),
                    state), state, getLastTokenLine(), getLastTokenPos());
              } else {
                calcDiffAndPushResultBack(lastFoundFullOperator.getText(), strBuffer);
                return new TokenizerResult(lastFoundFullOperator, state, getLastTokenLine(), getLastTokenPos());
              }
            }
            default: {
              throw new CriticalUnexpectedError();
            }
          }
        }

        final char chr = (char) readChar;

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
              return new TokenizerResult(makeTermFromString(strBuffer.toString(), state), state, getLastTokenLine(), getLastTokenPos());
            } else if (chr == '\''
                || (letterOrDigitOnly != Character.isLetterOrDigit(chr))
                || findOperatorForSingleChar(chr) != null) {
              push(chr);

              return new TokenizerResult(makeTermFromString(
                  strBuffer.toString(), state), state, getLastTokenLine(), getLastTokenPos());
            } else {
              strBuffer.append(chr);
            }
          }
          break;
          case INTEGER: {
            if (Character.isDigit(chr)) {
              strBuffer.append(chr);
            } else {
              if (chr == '.' || chr == 'e' || chr == 'E') {
                strBuffer.append(chr);
                state = TokenizerState.FLOAT;
              } else {
                push(chr);

                return new TokenizerResult(makeTermFromString(
                    strBuffer.toString(), state),
                    TokenizerState.INTEGER, getLastTokenLine(), getLastTokenPos());
              }
            }
          }
          break;
          case FLOAT: {
            if (Character.isDigit(chr)) {
              strBuffer.append(chr);
            } else {
              if (chr == '-' || chr == '+') {
                if (strBuffer.isLastChar('e')) {
                  strBuffer.append(chr);
                } else {
                  push(chr);
                  return new TokenizerResult(makeTermFromString(strBuffer.toString(),
                      TokenizerState.FLOAT),
                      TokenizerState.FLOAT, getLastTokenLine(), getLastTokenPos());
                }
              } else if (chr == 'e' || chr == 'E') {
                if (strBuffer.lastIndexOf('e') < 0) {
                  strBuffer.append('e');
                } else {
                  push(chr);
                  return new TokenizerResult(makeTermFromString(strBuffer.toStringExcludeLastChar(), TokenizerState.FLOAT), TokenizerState.FLOAT, getLastTokenLine(), getLastTokenPos());
                }
              } else {

                push(chr);

                if (strBuffer.isLastChar('.')) {
                  // it was an integer
                  push('.');
                  return new TokenizerResult(makeTermFromString(
                      strBuffer.toStringExcludeLastChar(),
                      TokenizerState.INTEGER),
                      TokenizerState.INTEGER, getLastTokenLine(), getLastTokenPos());
                } else {
                  // it is float
                  return new TokenizerResult(makeTermFromString(
                      strBuffer.toString(), state), state, getLastTokenLine(), getLastTokenPos());
                }
              }
            }
          }
          break;
          case OPERATOR: {
            if (chr != '_' && letterOrDigitOnly != Character.isLetterOrDigit(chr)) {
              push(chr);

              if (lastFoundFullOperator == null) {
                return new TokenizerResult(makeTermFromString(
                    strBuffer.toString(), state), state, getLastTokenLine(), getLastTokenPos());
              } else {
                return new TokenizerResult(lastFoundFullOperator, state, getLastTokenLine(), getLastTokenPos());
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
                          previousleDetectedOperator.getText(), strBuffer);
                      return new TokenizerResult(previousleDetectedOperator,
                          state, getLastTokenLine(), getLastTokenPos());
                    }
                  }
                } else {
                  if (!hasOperatorStartsWith(operator)) {
                    calcDiffAndPushResultBack(
                        previousleDetectedOperator.getText(), strBuffer);
                    return new TokenizerResult(previousleDetectedOperator, state, getLastTokenLine(), getLastTokenPos());
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
                  return new TokenizerResult(makeTermFromString(
                      strBuffer.toString(), state), state, getLastTokenLine(), getLastTokenPos());
                case '\\':
                  specCharDetected = true;
                  specCharBuffer.clear();
                  break;
                default:
                  strBuffer.append(chr);
                  break;
              }
            }
          }
          break;
          case VAR: {
            if (Character.isWhitespace(chr) || Character.isISOControl(chr)) {
              if (strBuffer.isSingleChar('_')) {
                return new TokenizerResult(new PrologVariable(), state, getLastTokenLine(), getLastTokenPos());
              }
              return new TokenizerResult(new PrologVariable(strBuffer.toString()), state, getLastTokenLine(), getLastTokenPos());
            } else if (chr != '_' && !Character.isLetterOrDigit(chr)) {
              push(chr);
              if (strBuffer.isSingleChar('_')) {
                return new TokenizerResult(new PrologVariable(), state, getLastTokenLine(), getLastTokenPos());
              }
              return new TokenizerResult(new PrologVariable(strBuffer.toString()), state, getLastTokenLine(), getLastTokenPos());
            } else {
              strBuffer.append(chr);
            }
          }
          break;
          default:
            throw new CriticalUnexpectedError();
        }
      }
    } catch (IOException ex) {
      throw new PrologParserException("IO exception during read char", this.prevLine, this.prevPos, ex);
    }
  }


  PrologTerm makeTermFromString(final String str, final TokenizerState state) {
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
      result = new PrologAtom(str);
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
