package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologFloatNumber;
import com.igormaznitsa.prologparser.terms.PrologIntegerNumber;
import com.igormaznitsa.prologparser.terms.PrologVariable;
import com.igormaznitsa.prologparser.utils.StrBuffer;
import com.igormaznitsa.prologparser.utils.StringUtils;

import java.io.IOException;
import java.io.Reader;

final class Tokenizer {

  private final StrBuffer strBuf = new StrBuffer(128);
  private final StrBuffer specCharBuf = new StrBuffer(128);
  private final StrBuffer insideCharBuffer = new StrBuffer(32);
  private final Reader reader;
  private final PrologParser parser;
  TokenizerResult lastPushedTerm;
  private int prevTokenLine;
  private int prevTokenPos;
  private int lastTokenLine;
  private int lastTokenPos;
  private int prevStrPos;
  private int prevLineNum;
  private int strPos;
  private int lineNum;

  Tokenizer(final PrologParser parser, final Reader reader) {
    super();
    this.reader = reader;
    this.parser = parser;

    this.strPos = 1;
    this.lineNum = 1;
    this.prevStrPos = 1;
    this.prevLineNum = 1;
  }

  public int readChar() throws IOException {
    int ch;
    if (this.insideCharBuffer.isEmpty()) {
      ch = this.reader.read();
    } else {
      ch = this.insideCharBuffer.popChar();
    }

    this.prevStrPos = this.strPos;
    this.prevLineNum = this.lineNum;
    if (ch == '\n') {
      this.strPos = 1;
      this.lineNum++;
    } else {
      if (ch >= 0) {
        this.strPos++;
      }
    }
    return ch;
  }

  public void calcDiffAndPushResultBack(final String etalon, final StrBuffer buffer) {
    int chars = buffer.length() - etalon.length();
    int pos = buffer.length() - 1;

    int lstrpos = this.strPos;
    int lstrposprev = this.prevStrPos;
    int llinenum = this.lineNum;
    int llinenumprev = this.prevLineNum;

    while (chars > 0) {
      final char ch = buffer.charAt(pos--);
      this.insideCharBuffer.pushChar(ch);
      chars--;
      lstrpos--;
      if (lstrpos < 1) {
        lstrpos = 1;
      }
      lstrposprev = lstrpos;
      if (ch == '\n') {
        llinenum--;
        if (llinenum < 1) {
          llinenum = 1;
        }
        llinenumprev = llinenum;
      }
    }

    this.strPos = lstrpos;
    this.prevStrPos = lstrposprev;
    this.lineNum = llinenum;
    this.prevLineNum = llinenumprev;
  }

  void push(final char ch) {
    this.insideCharBuffer.pushChar(ch);
    if (ch == '\n') {
      this.strPos = 1;
      this.lineNum--;
      if (this.lineNum <= 0) {
        this.lineNum = 1;
      }
    } else {
      this.strPos--;
      if (this.strPos <= 0) {
        this.strPos = 1;
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

  OperatorContainer findOperatorForName(final String operatorName) {
    OperatorContainer result = null;

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

  OperatorContainer findOperatorForSingleChar(final char c) {
    OperatorContainer result = PrologParser.META_SYSTEM_OPERATORS.get(c);
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

  int getLastTokenStrPos() {
    return this.lastPushedTerm == null ? this.lastTokenPos : this.prevTokenPos;
  }

  int getLastTokenLineNum() {
    return lastPushedTerm == null ? lastTokenLine
        : prevTokenLine;
  }

  void fixPosition() {
    prevTokenLine = lastTokenLine;
    prevTokenPos = lastTokenPos;
    lastTokenLine = this.lineNum;
    lastTokenPos = this.strPos - 1;
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

    final StrBuffer strBuffer = this.strBuf;
    final StrBuffer specCharBuffer = this.specCharBuf;

    OperatorContainer lastFoundFullOperator = null;

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
                    TokenizerState.INTEGER), TokenizerState.ATOM, getLastTokenStrPos(), getLastTokenLineNum());
              } else {
                // it is just integer number or an atom
                return new TokenizerResult(makeTermFromString(strBuffer.toString(), state), state, getLastTokenStrPos(), getLastTokenLineNum());
              }
            }
            case VAR: {
              if (strBuffer.hasSingleChar('_')) {
                return new TokenizerResult(new PrologVariable(), state, getLastTokenStrPos(), getLastTokenLineNum());
              } else {
                return new TokenizerResult(new PrologVariable(strBuffer.toString()), state, getLastTokenStrPos(), getLastTokenLineNum());
              }
            }
            case STRING: {
              throw new PrologParserException("Unclosed string found",
                  lastTokenLine, lastTokenPos);
            }
            case OPERATOR: {
              if (lastFoundFullOperator == null) {
                return new TokenizerResult(makeTermFromString(strBuffer.toString(),
                    state), state, getLastTokenStrPos(), getLastTokenLineNum());
              } else {
                calcDiffAndPushResultBack(lastFoundFullOperator.getText(), strBuffer);
                return new TokenizerResult(lastFoundFullOperator, state, getLastTokenStrPos(), getLastTokenLineNum());
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
              return new TokenizerResult(makeTermFromString(strBuffer.toString(), state), state, getLastTokenStrPos(), getLastTokenLineNum());
            } else if (chr == '\''
                || (letterOrDigitOnly != Character.isLetterOrDigit(chr))
                || findOperatorForSingleChar(chr) != null) {
              push(chr);

              return new TokenizerResult(makeTermFromString(
                  strBuffer.toString(), state), state, getLastTokenStrPos(), getLastTokenLineNum());
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
                    TokenizerState.INTEGER, getLastTokenStrPos(), getLastTokenLineNum());
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
                      TokenizerState.FLOAT, getLastTokenStrPos(), getLastTokenLineNum());
                }
              } else if (chr == 'e' || chr == 'E') {
                if (strBuffer.lastIndexOf("e") < 0) {
                  strBuffer.append('e');
                } else {
                  push(chr);
                  return new TokenizerResult(makeTermFromString(strBuffer.toStringExcludeLastChar(), TokenizerState.FLOAT), TokenizerState.FLOAT, getLastTokenStrPos(), getLastTokenLineNum());
                }
              } else {

                push(chr);

                if (strBuffer.isLastChar('.')) {
                  // it was an integer
                  push('.');
                  return new TokenizerResult(makeTermFromString(
                      strBuffer.toStringExcludeLastChar(),
                      TokenizerState.INTEGER),
                      TokenizerState.INTEGER, getLastTokenStrPos(), getLastTokenLineNum());
                } else {
                  // it is float
                  return new TokenizerResult(makeTermFromString(
                      strBuffer.toString(), state), state, getLastTokenStrPos(), getLastTokenLineNum());
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
                    strBuffer.toString(), state), state, getLastTokenStrPos(), getLastTokenLineNum());
              } else {
                return new TokenizerResult(lastFoundFullOperator, state, getLastTokenStrPos(), getLastTokenLineNum());
              }
            } else {
              final OperatorContainer previousleDetectedOperator = lastFoundFullOperator;
              strBuffer.append(chr);
              final String operator = strBuffer.toString();
              lastFoundFullOperator = findOperatorForName(operator);
              if (previousleDetectedOperator == null) {
                if (!hasOperatorStartsWith(operator)) {
                  if (hasOperatorStartsWith(String.valueOf(chr))) {
                    // next char can be the start char of an
                    // operator so we need get back it into the
                    // buffer
                    strBuffer.popChar();
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
                          state, getLastTokenStrPos(), getLastTokenLineNum());
                    }
                  }
                } else {
                  if (!hasOperatorStartsWith(operator)) {
                    calcDiffAndPushResultBack(
                        previousleDetectedOperator.getText(), strBuffer);
                    return new TokenizerResult(previousleDetectedOperator, state, getLastTokenStrPos(), getLastTokenLineNum());
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
                  throw new PrologParserException("Detected wrong escape char: \\" + specCharBuffer.toString(), this.prevLineNum, this.prevStrPos);
                } else if (!result.doesNeedMore()) {
                  strBuffer.append(result.getDecoded());
                  specCharDetected = false;
                }
              }
            } else {
              switch (chr) {
                case '\'':
                  return new TokenizerResult(makeTermFromString(
                      strBuffer.toString(), state), state, getLastTokenStrPos(), getLastTokenLineNum());
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
              if (strBuffer.hasSingleChar('_')) {
                return new TokenizerResult(new PrologVariable(), state, getLastTokenStrPos(), getLastTokenLineNum());
              }
              return new TokenizerResult(new PrologVariable(strBuffer.toString()), state, getLastTokenStrPos(), getLastTokenLineNum());
            } else if (chr != '_' && !Character.isLetterOrDigit(chr)) {
              push(chr);
              if (strBuffer.hasSingleChar('_')) {
                return new TokenizerResult(new PrologVariable(), state, getLastTokenStrPos(), getLastTokenLineNum());
              }
              return new TokenizerResult(new PrologVariable(strBuffer.toString()), state, getLastTokenStrPos(), getLastTokenLineNum());
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
      throw new PrologParserException("IO exception during read char", this.prevLineNum, this.prevStrPos, ex);
    }
  }


  AbstractPrologTerm makeTermFromString(final String str, final TokenizerState state) {
    AbstractPrologTerm result;

    switch (state) {
      case INTEGER: {
        try {
          result = new PrologIntegerNumber(str);
        } catch (NumberFormatException ex) {
          result = null;
        }
      }
      break;
      case FLOAT: {
        try {
          result = new PrologFloatNumber(str);
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

  int getLineNum() {
    return this.lineNum;
  }

  int getStrPos() {
    return this.strPos;
  }
}
