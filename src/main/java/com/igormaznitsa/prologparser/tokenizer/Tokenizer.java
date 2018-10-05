package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.CharSource;
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
import com.igormaznitsa.prologparser.utils.ringbuffer.SoftCache;

import java.io.IOException;
import java.util.function.Supplier;

final class Tokenizer implements Supplier<TokenizerResult> {

  private final SoftCache<TokenizerResult> resultCache = new SoftCache<>(this, 32);
  private final StrBuffer strBuf = new StrBuffer(128);
  private final StrBuffer specCharBuf = new StrBuffer(128);
  TokenizerResult lastPushedTerm;
  int prevTokenLine;
  int prevTokenPos;
  int lastTokenLine;
  int lastTokenPos;

  Tokenizer() {
    super();
  }

  static boolean hasOperatorStartsWith(
      final String operatorNameStartSubstring,
      final AbstractPrologParser parser) {

    boolean result = false;
    if (AbstractPrologParser.SYSTEM_OPERATORS_PREFIXES.contains(operatorNameStartSubstring)) {
      result = true;
    } else if (parser != null) {
      final ParserContext ctx = parser.getContext();
      if (ctx != null) {
        result = ctx.hasOperatorStartsWith(parser, operatorNameStartSubstring);
      }
    }
    return result;
  }

  static OperatorContainer findOperatorForName(final String operatorName, final AbstractPrologParser parser) {
    OperatorContainer result = null;

    // check metaoperators as the first ones
    if (operatorName.length() == 1) {
      result = AbstractPrologParser.META_SYSTEM_OPERATORS.get(operatorName);
    }
    if (result == null) {
      // check user defined operators because a user can replace a system operator
      if (parser != null) {
        final ParserContext ctx = parser.getContext();
        if (ctx != null) {
          result = ctx.findOperatorForName(parser, operatorName);
        }
      }

      // check system operators
      if (result == null) {
        result = AbstractPrologParser.SYSTEM_OPERATORS.get(operatorName);
      }
    }

    return result;
  }

  static OperatorContainer findOperatorForSingleChar(final char c, final AbstractPrologParser parser) {
    OperatorContainer result = AbstractPrologParser.META_SYSTEM_OPERATORS.get(c);
    if (result == null) {
      return findOperatorForName(String.valueOf(c), parser);
    }
    return result;
  }

  void push(final TokenizerResult object) {
    if (this.lastPushedTerm != null) {
      throw new IllegalStateException("There is already pushed term");
    }
    this.lastPushedTerm = object;
  }

  TokenizerResult peek(final CharSource reader, final AbstractPrologParser parser) throws IOException {
    TokenizerResult result;
    if (lastPushedTerm == null) {
      result = readNextToken(reader, parser);
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

  void fixPosition(final CharSource reader) {
    prevTokenLine = lastTokenLine;
    prevTokenPos = lastTokenPos;
    lastTokenLine = reader.getLineNum();
    lastTokenPos = reader.getStrPos() - 1;
  }

  void skipUntilNextString(final CharSource reader)
      throws IOException {
    while (true) {
      final int readchar = reader.read();
      if (readchar < 0 || readchar == '\n') {
        break;
      }
    }
  }

  TokenizerResult readNextToken(final CharSource reader, final AbstractPrologParser parser) {

    if (lastPushedTerm != null) {
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
        final int readChar;
        readChar = reader.read();

        if (readChar < 0) {
          switch (state) {
            case LOOKFOR:
              return null;
            case FLOAT:
            case INTEGER:
            case ATOM: {
              if (state == TokenizerState.FLOAT && strBuffer.isLastChar('.')) {
                // non-ended float then it is an integer number ened by the '.' operator
                reader.push('.');
                // it is Integer
                return makeResult(makeTermFromString(
                    strBuffer.toStringExcludeLastChar(),
                    TokenizerState.INTEGER), TokenizerState.ATOM, getLastTokenStrPos(), getLastTokenLineNum());
              } else {
                // it is just integer number or an atom
                return makeResult(makeTermFromString(strBuffer.toString(), state),
                    state, getLastTokenStrPos(), getLastTokenLineNum());
              }
            }
            case VAR: {
              if (strBuffer.hasSingleChar('_')) {
                return makeResult(new PrologVariable(), state, getLastTokenStrPos(), getLastTokenLineNum());
              } else {
                return makeResult(new PrologVariable(strBuffer.toString()),
                    state, getLastTokenStrPos(), getLastTokenLineNum());
              }
            }
            case STRING: {
              throw new PrologParserException("Unclosed string found",
                  lastTokenLine, lastTokenPos);
            }
            case OPERATOR: {
              if (lastFoundFullOperator == null) {
                return makeResult(makeTermFromString(strBuffer.toString(),
                    state), state, getLastTokenStrPos(), getLastTokenLineNum());
              } else {
                reader.calcDiffAndPushResultBack(
                    lastFoundFullOperator.getText(), strBuffer);
                return makeResult(lastFoundFullOperator, state, getLastTokenStrPos(), getLastTokenLineNum());
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
                skipUntilNextString(reader);
              }
              break;
              case '_': {
                fixPosition(reader);
                strBuffer.append(chr);
                state = TokenizerState.VAR;
              }
              break;
              case '\'': {
                fixPosition(reader);
                state = TokenizerState.STRING;
              }
              break;

              default: {
                fixPosition(reader);

                strBuffer.append(chr);

                if (Character.isUpperCase(chr)) {
                  state = TokenizerState.VAR;
                } else {
                  letterOrDigitOnly = Character.isLetterOrDigit(chr);
                  final String operator = String.valueOf(chr);
                  if (hasOperatorStartsWith(operator, parser)) {
                    lastFoundFullOperator = findOperatorForName(
                        operator, parser);
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
              return makeResult(makeTermFromString(
                  strBuffer.toString(), state), state, getLastTokenStrPos(), getLastTokenLineNum());
            } else if (chr == '\''
                || (letterOrDigitOnly != Character.isLetterOrDigit(chr))
                || findOperatorForSingleChar(chr, parser) != null) {
              reader.push(chr);

              return makeResult(makeTermFromString(
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
                reader.push(chr);

                return makeResult(makeTermFromString(
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
                  reader.push(chr);
                  return makeResult(makeTermFromString(strBuffer.toString(),
                      TokenizerState.FLOAT),
                      TokenizerState.FLOAT, getLastTokenStrPos(), getLastTokenLineNum());
                }
              } else if (chr == 'e' || chr == 'E') {
                if (strBuffer.lastIndexOf("e") < 0) {
                  strBuffer.append('e');
                } else {
                  reader.push(chr);
                  return makeResult(makeTermFromString(strBuffer.toStringExcludeLastChar(), TokenizerState.FLOAT), TokenizerState.FLOAT, getLastTokenStrPos(), getLastTokenLineNum());
                }
              } else {

                reader.push(chr);

                if (strBuffer.isLastChar('.')) {
                  // it was an integer
                  reader.push('.');
                  return makeResult(makeTermFromString(
                      strBuffer.toStringExcludeLastChar(),
                      TokenizerState.INTEGER),
                      TokenizerState.INTEGER, getLastTokenStrPos(), getLastTokenLineNum());
                } else {
                  // it is float
                  return makeResult(makeTermFromString(
                      strBuffer.toString(), state), state, getLastTokenStrPos(), getLastTokenLineNum());
                }
              }
            }
          }
          break;
          case OPERATOR: {
            if (chr != '_' && letterOrDigitOnly != Character.isLetterOrDigit(chr)) {
              reader.push(chr);

              if (lastFoundFullOperator == null) {
                return makeResult(makeTermFromString(
                    strBuffer.toString(), state), state, getLastTokenStrPos(), getLastTokenLineNum());
              } else {
                return makeResult(lastFoundFullOperator, state, getLastTokenStrPos(), getLastTokenLineNum());
              }
            } else {
              final OperatorContainer previousleDetectedOperator = lastFoundFullOperator;
              strBuffer.append(chr);
              final String operator = strBuffer.toString();
              lastFoundFullOperator = findOperatorForName(operator, parser);
              if (previousleDetectedOperator == null) {
                if (!hasOperatorStartsWith(operator, parser)) {
                  if (hasOperatorStartsWith(String.valueOf(chr),
                      parser)) {
                    // next char can be the start char of an
                    // operator so we need get back it into the
                    // buffer
                    strBuffer.popChar();
                    reader.push(chr);
                  }
                  state = TokenizerState.ATOM;
                }
              } else {
                if (lastFoundFullOperator == null) {
                  if (hasOperatorStartsWith(operator, parser)) {
                    lastFoundFullOperator = previousleDetectedOperator;
                  } else {
                    if (letterOrDigitOnly) {
                      state = TokenizerState.ATOM;
                    } else {
                      reader.calcDiffAndPushResultBack(
                          previousleDetectedOperator.getText(), strBuffer);
                      return makeResult(previousleDetectedOperator,
                          state, getLastTokenStrPos(), getLastTokenLineNum());
                    }
                  }
                } else {
                  if (!hasOperatorStartsWith(operator, parser)) {
                    reader.calcDiffAndPushResultBack(
                        previousleDetectedOperator.getText(), strBuffer);
                    return makeResult(previousleDetectedOperator, state, getLastTokenStrPos(), getLastTokenLineNum());
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
                  throw new PrologParserException("Detected wrong escape char: \\" + specCharBuffer.toString(), reader.getPrevLineNum(), reader.getPrevStrPos());
                } else if (!result.doesNeedMore()) {
                  strBuffer.append(result.getDecoded());
                  specCharDetected = false;
                }
              }
            } else {
              switch (chr) {
                case '\'':
                  return makeResult(makeTermFromString(
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
                return makeResult(new PrologVariable(), state, getLastTokenStrPos(), getLastTokenLineNum());
              }
              return makeResult(new PrologVariable(strBuffer.toString()), state, getLastTokenStrPos(), getLastTokenLineNum());
            } else if (chr != '_' && !Character.isLetterOrDigit(chr)) {
              reader.push(chr);
              if (strBuffer.hasSingleChar('_')) {
                return makeResult(new PrologVariable(), state, getLastTokenStrPos(), getLastTokenLineNum());
              }
              return makeResult(new PrologVariable(strBuffer.toString()), state, getLastTokenStrPos(), getLastTokenLineNum());
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
      throw new PrologParserException("IO exception during read char", reader.getPrevLineNum(), reader.getPrevStrPos(), ex);
    }
  }

  private TokenizerResult makeResult(final AbstractPrologTerm term, final TokenizerState state, final int strPos, final int lineNum) {
    final TokenizerResult result = resultCache.get();
    result.setData(term, state, strPos, lineNum);
    return result;
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

  @Override
  public TokenizerResult get() {
    return new TokenizerResult();
  }
}
