/*
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.igormaznitsa.prologparser;

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

/**
 * The class implements an intermediate tokenizer between a data stream and a
 * prolog parser.
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 */
final class PrologTokenizer implements Supplier<TokenizerResult> {

  final StringUtils.Mutable<Character> specialCharResult = new StringUtils.Mutable<>();
  /**
   * Inside caching ring buffer to cache tokenizer result items.
   */
  private final SoftCache<TokenizerResult> resultCache = new SoftCache<>(this, 32);
  /**
   * Inside string buffer.
   */
  private final StrBuffer strbuffer = new StrBuffer(128);
  /**
   * Inside string buffer for special chars processing
   */
  private final StrBuffer specialCharBuffer = new StrBuffer(128);
  /**
   * The variable contains the last pushed term. The term has been read
   * already but the reader pushed it back to reread it lately.
   */
  TokenizerResult lastPushedTerm;
  /**
   * The variable contains the previous value of the read token line number.
   */
  int prevReadTokenLineNum;
  /**
   * The variable contains the previous value of the read token string
   * position.
   */
  int prevReadTokenStrPos;
  /**
   * The variable contains the last value of the read token line number.
   */
  int lastReadTokenLineNum;
  /**
   * The variable contains the last value of the read token string position.
   */
  int lastReadTokenStrPos;

  /**
   * The constructor.
   */
  PrologTokenizer() {
    super();
  }

  /**
   * Function allows to check that there is an operator starts with a string,
   * as the first it checks the system operators then call the prolog context.
   *
   * @param operatorNameStartSubstring the start substring to be checked as
   *                                   the operator start name, must not be null
   * @param parser                     a prolog parser which context will be used, it can be null
   * @return true if there is any operator starts with the string, else false
   */
  static boolean hasOperatorStartsWith(
      final String operatorNameStartSubstring,
      final AbstractPrologParser parser) {

    // check for system
    if (AbstractPrologParser.SYSTEM_OPERATORS_PREFIXES.contains(operatorNameStartSubstring)) {
      return true;
    }

    // check only context
    boolean result = false;
    if (parser != null) {
      final ParserContext ctx = parser.getContext();
      if (ctx != null) {
        result = ctx.hasOperatorStartsWith(parser, operatorNameStartSubstring);
      }
    }
    return result;
  }

  /**
   * Function to find an operator for its name, as the first it will search
   * among system operators then in the prolog context.
   *
   * @param operatorName an operator name to be used for search, must not be
   *                     null
   * @param parser       a prolog parser which context will be used, it can be null
   * @return an OperatorContainer if the operator is presented, else null
   */
  static OperatorContainer findOperatorForName(final String operatorName,
                                               final AbstractPrologParser parser) {
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

  /**
   * Method allows to find an operator for its char representation.
   *
   * @param c      the char name of an operator.
   * @param parser a prolog parser which context will be used, it can be null
   * @return an OperatorContainer if the operator is presented, else null
   */
  static OperatorContainer findOperatorForSingleChar(final char c, final AbstractPrologParser parser) {
    OperatorContainer result = AbstractPrologParser.META_SYSTEM_OPERATORS.get(c);
    if (result == null) {
      return findOperatorForName(String.valueOf(c), parser);
    }
    return result;
  }

  /**
   * Push a read object back into buffer to read it lately
   *
   * @param object the object to be pushed back into buffer, null will clear
   *               the buffer.
   */
  void pushTermBack(final TokenizerResult object) {
    if (lastPushedTerm != null) {
      throw new IllegalStateException("An object has been pushed already");
    }
    lastPushedTerm = object;
  }

  /**
   * Peek the next token from the incoming stream. The token will be read but
   * after it will be saved into the inside variable to be read in next step.
   *
   * @param reader the reader to read char data, must not be null
   * @param parser the parser reading the stream, it can be null
   * @return a read token as a ProlTokenizerResult, or null if there is not
   * any more token in the stream
   * @throws IOException it will be throws if there is any transport problem
   */
  TokenizerResult peekToken(final PrologCharDataSource reader,
                            final AbstractPrologParser parser) throws PrologParserException,
      IOException {
    TokenizerResult result;
    if (lastPushedTerm == null) {
      result = nextToken(reader, parser);
      pushTermBack(result);
    } else {
      result = lastPushedTerm;
    }
    return result;
  }

  /**
   * Get the string position of the last read token
   *
   * @return the string position for the last read token as integer
   */
  int getLastTokenStrPos() {
    return lastPushedTerm == null ? lastReadTokenStrPos
        : prevReadTokenStrPos;
  }

  /**
   * Get the line number for the last read token
   *
   * @return the line number for the last read token as integer
   */
  int getLastTokenLineNum() {
    return lastPushedTerm == null ? lastReadTokenLineNum
        : prevReadTokenLineNum;
  }

  /**
   * Inside function to fix current read string and line positions.
   *
   * @param reader the reader which position must be fixed within inside
   *               variables, must not be null
   */
  void fixPosition(final PrologCharDataSource reader) {
    prevReadTokenLineNum = lastReadTokenLineNum;
    prevReadTokenStrPos = lastReadTokenStrPos;
    lastReadTokenLineNum = reader.getLineNumber();
    lastReadTokenStrPos = reader.getNextCharStringPosition() - 1;
  }

  /**
   * Skip all characters until the next line detected
   *
   * @param reader the source for char data, must not be null
   * @throws IOException it will be thrown if there is any transport problem
   *                     during the operation
   */
  void skipUntilNextString(final PrologCharDataSource reader)
      throws IOException {
    while (true) {
      final int readchar = reader.read();
      if (readchar < 0 || readchar == '\n') {
        break;
      }
    }
  }

  /**
   * Read the next token from a reader
   *
   * @param reader the reader to be used as the char data source, must not be
   *               null
   * @param parser the prolog parser calling reading the stream, it can be
   *               null
   * @return the next token found at the stream as a ProlTokenizerResult
   * object or null if the end of the stream has been reached
   * @throws IOException it will be thrown if there is any transport error
   *                     during the operation
   */
  TokenizerResult nextToken(final PrologCharDataSource reader,
                            final AbstractPrologParser parser) throws PrologParserException,
      IOException {

    if (lastPushedTerm != null) {
      try {
        return lastPushedTerm;
      } finally {
        lastPushedTerm = null;
      }
    }

    TokenizerState state = TokenizerState.LOOKFOR;
    boolean specialchar = false;

    strbuffer.clear();
    specialCharBuffer.clear();

    final StrBuffer localstrbuffer = this.strbuffer;
    final StrBuffer localspecialCharBuffer = this.specialCharBuffer;

    specialCharResult.reset();

    OperatorContainer lastFoundFullOperator = null;

    boolean letterOrDigitOnly = false;

    while (true) {
      final int readchar = reader.read();

      if (readchar < 0) {
        switch (state) {
          case LOOKFOR:
            return null;
          case FLOAT:
          case INTEGER:
          case ATOM: {
            if (state == TokenizerState.FLOAT && localstrbuffer.isLastChar('.')) {
              // non-ended float then it is an integer number ened by the '.' operator
              reader.pushCharBack('.');
              // it is Integer
              return makeResult(makeTermFromString(
                  localstrbuffer.toStringExcludeLastChar(),
                  TokenizerState.INTEGER), TokenizerState.ATOM, getLastTokenStrPos(), getLastTokenLineNum());
            } else {
              // it is just integer number or an atom
              return makeResult(makeTermFromString(localstrbuffer.toString(), state),
                  state, getLastTokenStrPos(), getLastTokenLineNum());
            }
          }
          case VARIABLE: {
            if (localstrbuffer.hasSingleChar('_')) {
              return makeResult(new PrologVariable(), state, getLastTokenStrPos(), getLastTokenLineNum());
            } else {
              return makeResult(new PrologVariable(localstrbuffer.toString()),
                  state, getLastTokenStrPos(), getLastTokenLineNum());
            }
          }
          case STRING: {
            throw new PrologParserException("Unclosed string found",
                lastReadTokenLineNum, lastReadTokenStrPos);
          }
          case OPERATOR: {
            if (lastFoundFullOperator == null) {
              return makeResult(makeTermFromString(localstrbuffer.toString(),
                  state), state, getLastTokenStrPos(), getLastTokenLineNum());
            } else {
              reader.calculateDifferenceAndPushTheResultBack(
                  lastFoundFullOperator.getText(), localstrbuffer);
              return makeResult(lastFoundFullOperator, state, getLastTokenStrPos(), getLastTokenLineNum());
            }
          }
          default: {
            throw new CriticalUnexpectedError();
          }
        }
      }

      final char chr = (char) readchar;

      switch (state) {
        case LOOKFOR: {
          if (Character.isWhitespace(chr) || Character.isISOControl(chr)) {
            continue;
          }

          switch (chr) {
            case '%':
              // comments
              skipUntilNextString(reader);
              break;
            case '_':
              fixPosition(reader);
              localstrbuffer.append(chr);
              state = TokenizerState.VARIABLE;
              break;
            case '\'':
              fixPosition(reader);
              state = TokenizerState.STRING;
              break;

            default:
              fixPosition(reader);

              localstrbuffer.append(chr);

              if (Character.isUpperCase(chr)) {
                state = TokenizerState.VARIABLE;
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
        }
        break;
        case ATOM: {
          if (chr == '_') {
            localstrbuffer.append(chr);
          } else if (Character.isISOControl(chr) || Character.isWhitespace(chr)) {
            return makeResult(makeTermFromString(
                localstrbuffer.toString(), state), state, getLastTokenStrPos(), getLastTokenLineNum());
          } else if (chr == '\''
              || (letterOrDigitOnly != Character.isLetterOrDigit(chr))
              || findOperatorForSingleChar(chr, parser) != null) {
            reader.pushCharBack(chr);

            return makeResult(makeTermFromString(
                localstrbuffer.toString(), state), state, getLastTokenStrPos(), getLastTokenLineNum());
          } else {
            localstrbuffer.append(chr);
          }
        }
        break;
        case INTEGER: {
          if (Character.isDigit(chr)) {
            localstrbuffer.append(chr);
          } else {
            if (chr == '.' || chr == 'e' || chr == 'E') {
              localstrbuffer.append(chr);
              state = TokenizerState.FLOAT;
            } else {
              reader.pushCharBack(chr);

              return makeResult(makeTermFromString(
                  localstrbuffer.toString(), state),
                  TokenizerState.INTEGER, getLastTokenStrPos(), getLastTokenLineNum());
            }
          }
        }
        break;
        case FLOAT: {
          if (Character.isDigit(chr)) {
            localstrbuffer.append(chr);
          } else {
            if (chr == '-' || chr == '+') {
              if (localstrbuffer.isLastChar('e')) {
                localstrbuffer.append(chr);
              } else {
                reader.pushCharBack(chr);
                return makeResult(makeTermFromString(localstrbuffer.toString(),
                    TokenizerState.FLOAT),
                    TokenizerState.FLOAT, getLastTokenStrPos(), getLastTokenLineNum());
              }
            } else if (chr == 'e' || chr == 'E') {
              if (localstrbuffer.lastIndexOf("e") < 0) {
                localstrbuffer.append('e');
              } else {
                reader.pushCharBack(chr);
                return makeResult(makeTermFromString(localstrbuffer.toStringExcludeLastChar(), TokenizerState.FLOAT), TokenizerState.FLOAT, getLastTokenStrPos(), getLastTokenLineNum());
              }
            } else {

              reader.pushCharBack(chr);

              if (localstrbuffer.isLastChar('.')) {
                // it was an integer
                reader.pushCharBack('.');
                return makeResult(makeTermFromString(
                    localstrbuffer.toStringExcludeLastChar(),
                    TokenizerState.INTEGER),
                    TokenizerState.INTEGER, getLastTokenStrPos(), getLastTokenLineNum());
              } else {
                // it is float
                return makeResult(makeTermFromString(
                    localstrbuffer.toString(), state), state, getLastTokenStrPos(), getLastTokenLineNum());
              }
            }
          }
        }
        break;
        case OPERATOR: {
          if (chr != '_' && letterOrDigitOnly != Character.isLetterOrDigit(chr)) {
            reader.pushCharBack(chr);

            if (lastFoundFullOperator == null) {
              return makeResult(makeTermFromString(
                  localstrbuffer.toString(), state), state, getLastTokenStrPos(), getLastTokenLineNum());
            } else {
              return makeResult(lastFoundFullOperator, state, getLastTokenStrPos(), getLastTokenLineNum());
            }
          } else {
            final OperatorContainer previousleDetectedOperator = lastFoundFullOperator;
            localstrbuffer.append(chr);
            final String operator = localstrbuffer.toString();
            lastFoundFullOperator = findOperatorForName(operator, parser);
            if (previousleDetectedOperator == null) {
              if (!hasOperatorStartsWith(operator, parser)) {
                if (hasOperatorStartsWith(String.valueOf(chr),
                    parser)) {
                  // next char can be the start char of an
                  // operator so we need get back it into the
                  // buffer
                  localstrbuffer.popChar();
                  reader.pushCharBack(chr);
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
                    reader.calculateDifferenceAndPushTheResultBack(
                        previousleDetectedOperator.getText(), localstrbuffer);
                    return makeResult(previousleDetectedOperator,
                        state, getLastTokenStrPos(), getLastTokenLineNum());
                  }
                }
              } else {
                if (!hasOperatorStartsWith(operator, parser)) {
                  reader.calculateDifferenceAndPushTheResultBack(
                      previousleDetectedOperator.getText(), localstrbuffer);
                  return makeResult(previousleDetectedOperator, state, getLastTokenStrPos(), getLastTokenLineNum());
                }
              }
            }
          }
        }
        break;
        case STRING: {
          if (specialchar) {
            if (localspecialCharBuffer.isEmpty() && chr == '\n') {
              // just add the next line code
              localstrbuffer.append('\n');
              specialchar = false;
            } else {
              // try to parse special char
              localspecialCharBuffer.append(chr);
              if (StringUtils.unescapeCharacter(
                  localspecialCharBuffer.toString(),
                  specialCharResult)) {
                // special character detected and it doesn't have
                // errors
                if (specialCharResult.get() != null) {
                  // the special char fully parsed
                  localstrbuffer.append(specialCharResult.get());
                  specialchar = false;
                }
              } else {
                if (specialCharResult.get() == null) {
                  // error special character detected, so throw an
                  // exception
                  throw new PrologParserException(
                      "Unsupported special char [\\"
                          + localspecialCharBuffer.toString()
                          + "]",
                      reader.getPrevLineNumber(),
                      reader.getPreviousNextCharStringPosition());
                }
              }
            }
          } else {
            switch (chr) {
              case '\'':
                return makeResult(makeTermFromString(
                    localstrbuffer.toString(), state), state, getLastTokenStrPos(), getLastTokenLineNum());
              case '\\':
                specialchar = true;
                localspecialCharBuffer.clear();
                break;
              default:
                localstrbuffer.append(chr);
                break;
            }
          }
        }
        break;
        case VARIABLE: {
          if (Character.isWhitespace(chr) || Character.isISOControl(chr)) {
            if (localstrbuffer.hasSingleChar('_')) {
              return makeResult(new PrologVariable(), state, getLastTokenStrPos(), getLastTokenLineNum());
            }
            return makeResult(new PrologVariable(localstrbuffer.toString()), state, getLastTokenStrPos(), getLastTokenLineNum());
          } else if (chr != '_' && !Character.isLetterOrDigit(chr)) {
            reader.pushCharBack(chr);
            if (localstrbuffer.hasSingleChar('_')) {
              return makeResult(new PrologVariable(), state, getLastTokenStrPos(), getLastTokenLineNum());
            }
            return makeResult(new PrologVariable(localstrbuffer.toString()), state, getLastTokenStrPos(), getLastTokenLineNum());
          } else {
            localstrbuffer.append(chr);
          }
        }
        break;
        default:
          throw new CriticalUnexpectedError();
      }
    }
  }

  /**
   * The auxiliary method to centralize of result creation.
   *
   * @param term    a term
   * @param state   a tokenizer state
   * @param strPos  a string position
   * @param lineNum a line number
   * @return a generated tokenizer result
   */
  private TokenizerResult makeResult(final AbstractPrologTerm term, final TokenizerState state, final int strPos, final int lineNum) {
    final TokenizerResult result = resultCache.get();
    result.setData(term, state, strPos, lineNum);
    return result;
  }

  /**
   * Inside auxiliary function to make a term from a String
   *
   * @param str   the source string object, must not be null
   * @param state the state of inside state machine which was set during the
   *              term reading
   * @return a Term object as the result, not-null value will be returned
   * anyway
   */
  AbstractPrologTerm makeTermFromString(final String str,
                                        final TokenizerState state) {
    AbstractPrologTerm result;

    switch (state) {
      case INTEGER:
        try {
          result = new PrologIntegerNumber(str);
        } catch (NumberFormatException ex) {
          result = null;
        }
        break;
      case FLOAT:
        try {
          result = new PrologFloatNumber(str);
        } catch (NumberFormatException ex) {
          result = null;
        }
        break;
      default:
        result = null;
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
