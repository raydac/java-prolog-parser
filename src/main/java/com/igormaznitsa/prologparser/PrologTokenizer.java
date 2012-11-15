/*
 * Copyright 2011-2012 Igor Maznitsa (http://www.igormaznitsa.com)
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of version 3 of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307  USA
 */
package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.exceptions.CriticalSoftwareDefectError;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologFloatNumber;
import com.igormaznitsa.prologparser.terms.PrologIntegerNumber;
import com.igormaznitsa.prologparser.terms.PrologVariable;
import static com.igormaznitsa.prologparser.utils.AssertionUtils.*;
import com.igormaznitsa.prologparser.utils.StringUtils;
import java.io.IOException;

/**
 * The class implements an intermediate tokenizer between a data stream and a
 * prolog parser.
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 */
final class PrologTokenizer {

    /**
     * Inside string buffer.
     */
    private final FastStringBuilder strbuffer = new FastStringBuilder(128);
    /**
     * Inside string buffer for special chars processing
     */
    private final FastStringBuilder specialCharBuffer = new FastStringBuilder(128);
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
     * Push a read object back into buffer to read it lately
     *
     * @param object the object to be pushed back into buffer, null will clear
     * the buffer.
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
            final PrologParser parser) throws PrologParserException,
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
     * variables, must not be null
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
     * during the operation
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
     * null
     * @param parser the prolog parser calling reading the stream, it can be
     * null
     * @return the next token found at the stream as a ProlTokenizerResult
     * object or null if the end of the stream has been reached
     * @throws IOException it will be thrown if there is any transport error
     * during the operation
     */
    TokenizerResult nextToken(final PrologCharDataSource reader,
            final PrologParser parser) throws PrologParserException,
            IOException {

        checkNotNull("Reader is null", reader);

        if (lastPushedTerm != null) {
            try {
                return lastPushedTerm;
            } finally {
                lastPushedTerm = null;
            }
        }

        TokenizerState state = TokenizerState.LOOKFOR;
        boolean specialchar = false;

        strbuffer.setLength(0);
        specialCharBuffer.setLength(0);

        final FastStringBuilder localstrbuffer = this.strbuffer;
        final FastStringBuilder localspecialCharBuffer = this.specialCharBuffer;

        final StringUtils.Mutable<Character> specialCharResult = new StringUtils.Mutable<Character>();

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
                    case ATOM:
                        if (state == TokenizerState.FLOAT && localstrbuffer.hasLastChar('.')) {
                            // non-ended float then it is an integer number ened by the '.' operator
                            reader.pushCharBack('.');
                            // it is Integer
                            return new TokenizerResult(makeTermFromString(
                                    localstrbuffer.toStringExcludeLastChar(),
                                    TokenizerState.INTEGER), TokenizerState.ATOM, getLastTokenStrPos(), getLastTokenLineNum());
                        } else {
                            // it is just integer number or an atom
                            return new TokenizerResult(makeTermFromString(localstrbuffer.toString(), state),
                                    state, getLastTokenStrPos(), getLastTokenLineNum());
                        }
                    case VARIABLE:
                        if (localstrbuffer.hasOnlyChar('_')) {
                            return new TokenizerResult(new PrologVariable(), state, getLastTokenStrPos(), getLastTokenLineNum());
                        } else {
                            return new TokenizerResult(new PrologVariable(localstrbuffer.toString()),
                                    state, getLastTokenStrPos(), getLastTokenLineNum());
                        }

                    case STRING:
                        throw new PrologParserException("Unclosed string found",
                                lastReadTokenLineNum, lastReadTokenStrPos);
                    case OPERATOR:
                        if (lastFoundFullOperator == null) {
                            return new TokenizerResult(makeTermFromString(localstrbuffer.toString(),
                                    state), state, getLastTokenStrPos(), getLastTokenLineNum());
                        } else {
                            reader.calculateDifferenceAndPushTheResultBack(
                                    lastFoundFullOperator.getText(), localstrbuffer);
                            return new TokenizerResult(lastFoundFullOperator, state, getLastTokenStrPos(), getLastTokenLineNum());
                        }
                    default:
                        throw new CriticalSoftwareDefectError();
                }
            }

            final char chr = (char) readchar;

            switch (state) {
                case LOOKFOR:
                    if (isWhitespace(chr) || isISOControl(chr)) {
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
                            
                            if (isUppercaseLetter(chr)) {
                                state = TokenizerState.VARIABLE;
                            } else {
                                letterOrDigitOnly = isLetterOrDigit(chr);
                                final String operator = Character.toString(chr);
                                if (hasOperatorStartsWith(operator, parser)) {
                                    lastFoundFullOperator = findOperatorForName(
                                            operator, parser);
                                    state = TokenizerState.OPERATOR;
                                } else {
                                    if (isDigit(chr)) {
                                        state = TokenizerState.INTEGER;
                                    } else {
                                        state = TokenizerState.ATOM;
                                    }
                                }
                            }
                    }
                    break;
                case ATOM:
                    if (chr == '_') {
                        localstrbuffer.append(chr);
                    } else if (isWhitespace(chr)
                            || isISOControl(chr)) {
                        return new TokenizerResult(makeTermFromString(
                                localstrbuffer.toString(), state), state, getLastTokenStrPos(), getLastTokenLineNum());
                    } else if (chr == '\''
                            || (letterOrDigitOnly != isLetterOrDigit(chr))
                            || findOperatorForName(Character.toString(chr),
                            parser) != null) {
                        reader.pushCharBack(chr);
                        return new TokenizerResult(makeTermFromString(
                                localstrbuffer.toString(), state), state, getLastTokenStrPos(), getLastTokenLineNum());
                    } else {
                        localstrbuffer.append(chr);
                    }
                    break;
                case INTEGER:
                    if (isDigit(chr)) {
                        localstrbuffer.append(chr);
                    } else {
                        if (chr == '.' || chr == 'e' || chr == 'E') {
                            localstrbuffer.append(chr);
                            state = TokenizerState.FLOAT;
                        } else {
                            reader.pushCharBack(chr);
                            return new TokenizerResult(makeTermFromString(
                                    localstrbuffer.toString(), state),
                                    TokenizerState.INTEGER, getLastTokenStrPos(), getLastTokenLineNum());
                        }
                    }
                    break;
                case FLOAT:
                    if (isDigit(chr)) {
                        localstrbuffer.append(chr);
                    } else {
                        if (chr == '-' || chr == '+') {
                            if (localstrbuffer.charAt(localstrbuffer.length() - 1) == 'e') {
                                localstrbuffer.append(chr);
                            } else {
                                reader.pushCharBack(chr);
                                return new TokenizerResult(
                                        makeTermFromString(localstrbuffer.toString(),
                                        TokenizerState.FLOAT),
                                        TokenizerState.FLOAT, getLastTokenStrPos(), getLastTokenLineNum());
                            }
                        } else if (chr == 'e' || chr == 'E') {
                            if (localstrbuffer.indexOf('e') < 0) {
                                localstrbuffer.append('e');
                            } else {
                                reader.pushCharBack(chr);
                                return new TokenizerResult(makeTermFromString(
                                        localstrbuffer.substring(0,
                                        localstrbuffer.length() - 1),
                                        TokenizerState.FLOAT), TokenizerState.FLOAT, getLastTokenStrPos(), getLastTokenLineNum());
                            }
                        } else {

                            reader.pushCharBack(chr);

                            if (localstrbuffer.hasLastChar('.')) {
                                // it was an integer
                                reader.pushCharBack('.');
                                return new TokenizerResult(makeTermFromString(
                                        localstrbuffer.toStringExcludeLastChar(),
                                        TokenizerState.INTEGER),
                                        TokenizerState.INTEGER, getLastTokenStrPos(), getLastTokenLineNum());
                            } else {
                                // it is float
                                return new TokenizerResult(makeTermFromString(
                                        localstrbuffer.toString(), state), state, getLastTokenStrPos(), getLastTokenLineNum());
                            }
                        }
                    }
                    break;
                case OPERATOR:
                    if (chr != '_'
                            && letterOrDigitOnly != isLetterOrDigit(chr)) {
                        reader.pushCharBack(chr);

                        if (lastFoundFullOperator == null) {
                            return new TokenizerResult(makeTermFromString(
                                    localstrbuffer.toString(), state), state, getLastTokenStrPos(), getLastTokenLineNum());
                        } else {
                            return new TokenizerResult(lastFoundFullOperator, state, getLastTokenStrPos(), getLastTokenLineNum());
                        }
                    } else {
                        final OperatorContainer prevoperators = lastFoundFullOperator;
                        localstrbuffer.append(chr);
                        final String operator = localstrbuffer.toString();
                        lastFoundFullOperator = findOperatorForName(operator,
                                parser);
                        if (prevoperators == null) {
                            if (!hasOperatorStartsWith(operator, parser)) {
                                if (hasOperatorStartsWith(Character.toString(chr),
                                        parser)) {
                                    // next char can be the start char of an
                                    // operator so we need get back it into the
                                    // buffer
                                    localstrbuffer.setLength(localstrbuffer.length() - 1);
                                    reader.pushCharBack(chr);
                                }
                                state = TokenizerState.ATOM;
                            }
                        } else {
                            if (lastFoundFullOperator == null) {
                                if (hasOperatorStartsWith(operator, parser)) {
                                    lastFoundFullOperator = prevoperators;
                                } else {
                                    if (letterOrDigitOnly) {
                                        state = TokenizerState.ATOM;
                                    } else {
                                        reader.calculateDifferenceAndPushTheResultBack(
                                                prevoperators.getText(), localstrbuffer);
                                        return new TokenizerResult(prevoperators,
                                                state, getLastTokenStrPos(), getLastTokenLineNum());
                                    }
                                }
                            } else {
                                if (!hasOperatorStartsWith(operator, parser)) {
                                    reader.calculateDifferenceAndPushTheResultBack(
                                            prevoperators.getText(), localstrbuffer);
                                    return new TokenizerResult(prevoperators, state, getLastTokenStrPos(), getLastTokenLineNum());
                                }
                            }
                        }
                    }
                    break;
                case STRING:
                    if (specialchar) {
                        if (localspecialCharBuffer.length() == 0 && chr == '\n') {
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
                                    localstrbuffer.append(specialCharResult.get().charValue());
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
                                return new TokenizerResult(makeTermFromString(
                                        localstrbuffer.toString(), state), state, getLastTokenStrPos(), getLastTokenLineNum());
                            case '\\':
                                specialchar = true;
                                localspecialCharBuffer.setLength(0);
                                break;
                            default:
                                localstrbuffer.append(chr);
                                break;
                        }
                    }
                    break;
                case VARIABLE:
                    if (isWhitespace(chr) || isISOControl(chr)) {
                        if (localstrbuffer.hasOnlyChar('_')) {
                            return new TokenizerResult(new PrologVariable(), state, getLastTokenStrPos(), getLastTokenLineNum());
                        }
                        return new TokenizerResult(new PrologVariable(localstrbuffer.toString()), state, getLastTokenStrPos(), getLastTokenLineNum());
                    } else if (chr != '_' && !isLetterOrDigit(chr)) {
                        reader.pushCharBack(chr);
                        if (localstrbuffer.hasOnlyChar('_')) {
                            return new TokenizerResult(new PrologVariable(), state, getLastTokenStrPos(), getLastTokenLineNum());
                        }
                        return new TokenizerResult(new PrologVariable(localstrbuffer.toString()), state, getLastTokenStrPos(), getLastTokenLineNum());
                    } else {
                        localstrbuffer.append(chr);
                    }
                    break;
                default:
                    throw new CriticalSoftwareDefectError();
            }
        }
    }

    private static boolean isUppercaseLetter(final char chr){
        final int code = (int)chr;
        if (code>=(int)'A' && code<=(int)'Z') return true;
        if (isLatinLetterOrDigit(code)) return false;
        return Character.isUpperCase(code);
    }
    
    private static boolean isISOControl(final char chr) {
        final int code = (int) chr;
        return Character.isISOControl(code);
    }

    private static boolean isDigit(final char chr) {
        final int code = (int) chr;
        return code >= (int) '0' && code <= (int) '9';
    }

    private static boolean isWhitespace(final char chr) {
        final int code = (int) chr;
        if (code == (int) ' ') {
            return true;
        }
        if (isLatinLetterOrDigit(code)) {
            return false;
        }
        return Character.isWhitespace(code);
    }

    private static boolean isLatinLetterOrDigit(final int code) {
        if (code >= (int) 'a' && code <= (int) 'z') {
            return true;
        }
        if (code >= (int) 'A' && code <= (int) 'Z') {
            return true;
        }
        if (code >= (int) '0' && code <= (int) '9') {
            return true;
        }
        return false;
    }

    private static boolean isLetterOrDigit(final char chr) {
        final int code = (int) chr;
        if (isLatinLetterOrDigit(code)) {
            return true;
        } else {
            return Character.isLetter(code);
        }
    }

    /**
     * Inside auxiliary function to make a term from a String
     *
     * @param string the source string object, must not be null
     * @param state the state of inside state machine which was set during the
     * term reading
     * @return a Term object as the result, not-null value will be returned
     * anyway
     */
    AbstractPrologTerm makeTermFromString(final String string,
            final TokenizerState state) {
        AbstractPrologTerm result;

        switch (state) {
            case INTEGER:
                try {
                    result = new PrologIntegerNumber(string);
                } catch (NumberFormatException ex) {
                    result = null;
                }
                break;
            case FLOAT:
                try {
                    result = new PrologFloatNumber(string);
                } catch (NumberFormatException ex) {
                    result = null;
                }
                break;
            default:
                result = null;
        }

        if (result == null) {
            result = new PrologAtom(string);
        }

        return result;
    }

    /**
     * Function allows to check that there is an operator starts with a string,
     * as the first it checks the system operators then call the prolog context.
     *
     * @param operatorNameStartSubstring the start substring to be checked as
     * the operator start name, must not be null
     * @param parser a prolog parser which context will be used, it can be null
     * @return true if there is any operator starts with the string, else false
     */
    static boolean hasOperatorStartsWith(
            final String operatorNameStartSubstring,
            final PrologParser parser) {

        checkNotNull("The substring is null", operatorNameStartSubstring);

        // check for system
        if (PrologParser.SYSTEM_OPERATORS_PREFIXES.contains(operatorNameStartSubstring)) {
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
     * null
     * @param parser a prolog parser which context will be used, it can be null
     * @return an OperatorContainer if the operator is presented, else null
     */
    static OperatorContainer findOperatorForName(final String operatorName,
            final PrologParser parser) {
        checkNotNull("The operator name is null", operatorName);

        OperatorContainer result = null;

        // check metaoperators as the first ones
        if (operatorName.length() == 1) {
            result = PrologParser.META_SYSTEM_OPERATORS.get(operatorName);
        }

        // check user defined operators because a user can replace a system operator
        if (result == null && parser != null) {
            final ParserContext ctx = parser.getContext();
            if (ctx != null) {
                result = ctx.findOperatorForName(parser, operatorName);
            }
        }

        // check system operators
        if (result == null) {
            result = PrologParser.SYSTEM_OPERATORS.get(operatorName);
        }

        return result;
    }
}
