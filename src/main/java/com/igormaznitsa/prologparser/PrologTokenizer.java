/*
 * Copyright 2011 Igor Maznitsa (http://www.igormaznitsa.com)
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of version 2.1 of the GNU Lesser General Public
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

import java.io.IOException;

import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologFloatNumber;
import com.igormaznitsa.prologparser.terms.PrologIntegerNumber;
import com.igormaznitsa.prologparser.terms.PrologVariable;
import com.igormaznitsa.prologparser.utils.StringUtils;

/**
 * The class implements an intermediate tokenizer between a data stream and a
 * prolog parser.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.00
 */
final class PrologTokenizer {

	/**
	 * The variable contains the last pushed term. The term has been read
	 * already but the reader pushed it back to reread it lately
	 */
	TokenizerResult lastPushedTerm;
	/**
	 * The variable contains the previous value of the read token line number
	 */
	int prevReadTokenLineNum;
	/**
	 * The variable contains the previous value of the read token string
	 * position
	 */
	int prevReadTokenStrPos;
	/**
	 * The variable contains the last value of the read token line number
	 */
	int lastReadTokenLineNum;
	/**
	 * The variable contains the last value of the read token string position
	 */
	int lastReadTokenStrPos;

	/**
	 * The constructor
	 */
	PrologTokenizer() {
		super();
	}

	/**
	 * Push a read object back into buffer to read it lately
	 * 
	 * @param object
	 *            the object to be pushed back into buffer, null will clear the
	 *            buffer
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
	 * @param reader
	 *            the reader to read char data, must not be null
	 * @param parserContext
	 *            the parser context to be used for the operation, it can be
	 *            null
	 * @return a read token as a ProlTokenizerResult, or null if there is not
	 *         any more token in the stream
	 * @throws IOException
	 *             it will be throws if there is any transport problem
	 */
	TokenizerResult peekToken(final PrologCharDataSource reader,
			final ParserContext parserContext) throws PrologParserException,
			IOException {
		TokenizerResult result = null;
		if (lastPushedTerm == null) {
			result = nextToken(reader, parserContext);
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
	 * @param reader
	 *            the reader which position must be fixed within inside
	 *            variables, must not be null
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
	 * @param reader
	 *            the source for char data, must not be null
	 * @throws IOException
	 *             it will be thrown if there is any transport problem during
	 *             the operation
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
	 * @param reader
	 *            the reader to be used as the char data source, must not be
	 *            null
	 * @param parserContext
	 *            the parser context to be used for operator recognizing, it can
	 *            be null
	 * @return the next token found at the stream as a ProlTokenizerResult
	 *         object or null if the end of the stream has been reached
	 * @throws IOException
	 *             it will be thrown if there is any transport error during the
	 *             operation
	 */
	TokenizerResult nextToken(final PrologCharDataSource reader,
			final ParserContext parserContext) throws PrologParserException,
			IOException {

		if (reader == null)
			throw new NullPointerException("Reader is null");

		if (lastPushedTerm != null) {
			try {
				return lastPushedTerm;
			} finally {
				lastPushedTerm = null;
			}
		}

		TokenizerState state = TokenizerState.LOOKFOR;
		boolean specialchar = false;

		final StringBuilder strbuffer = new StringBuilder();
		final StringBuilder specialCharBuffer = new StringBuilder();
		final StringUtils.Mutable<Character> specialCharResult = new StringUtils.Mutable<Character>();

		OperatorContainer lastFoundFullOperator = null;

		boolean letterOrDigitOnly = false;

		while (true) {
			final int readchar = reader.read();

			if (readchar < 0) {
				final String str = strbuffer.toString();
				switch (state) {
				case LOOKFOR:
					return null;
				case FLOAT:
					if (str.charAt(str.length() - 1) == '.') {
						// non ended float then it integer + '.'
						reader.pushCharBack('.');
						// it is Integer
						return new TokenizerResult(makeTermFromString(
								str.substring(0, str.length() - 1),
								TokenizerState.INTEGER), TokenizerState.ATOM);
					}
				case INTEGER:
				case ATOM:
					return new TokenizerResult(makeTermFromString(str, state),
							state);
				case VARIABLE:
					if (str.equals("_")) {
						return new TokenizerResult(new PrologVariable(), state);
					} else {
						return new TokenizerResult(new PrologVariable(str),
								state);
					}

				case STRING:
					throw new PrologParserException("Unclosed string found",
							lastReadTokenLineNum, lastReadTokenStrPos);
				case OPERATOR:
					if (lastFoundFullOperator == null) {
						return new TokenizerResult(makeTermFromString(str,
								state), state);
					} else {
						reader.calculateDifferenceAndPushTheResultBack(
								lastFoundFullOperator.getText(), strbuffer);
						return new TokenizerResult(lastFoundFullOperator, state);
					}
				default:
					throw new Error("Unknown reader state");
				}
			}

			final char chr = (char) readchar;

			switch (state) {
			case LOOKFOR:
				if (Character.isISOControl(chr) || Character.isWhitespace(chr)) {
					continue;
				}

				switch (chr) {
				case '%':
					// comments
					skipUntilNextString(reader);
					break;
				case '_':
					fixPosition(reader);
					strbuffer.append(chr);
					state = TokenizerState.VARIABLE;
					break;
				case '\'':
					fixPosition(reader);
					state = TokenizerState.STRING;
					break;

				default:
					fixPosition(reader);

					strbuffer.append(chr);

					if (Character.isLetter(chr) && Character.isUpperCase(chr)) {
						state = TokenizerState.VARIABLE;
					} else {
						letterOrDigitOnly = Character.isLetterOrDigit(chr);
						String operator = Character.toString(chr);
						if (hasOperatorStartsWith(operator, parserContext)) {
							lastFoundFullOperator = findOperatorForName(
									operator, parserContext);
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
			case ATOM:
				if (chr == '_') {
					strbuffer.append(chr);
				} else if (Character.isWhitespace(chr)
						|| Character.isISOControl(chr)) {
					return new TokenizerResult(makeTermFromString(
							strbuffer.toString(), state), state);
				} else if (chr == '\''
						|| (letterOrDigitOnly != Character.isLetterOrDigit(chr))
						|| findOperatorForName(Character.toString(chr),
								parserContext) != null) {
					reader.pushCharBack(chr);
					return new TokenizerResult(makeTermFromString(
							strbuffer.toString(), state), state);
				} else {
					strbuffer.append(chr);
				}
				break;
			case INTEGER:
				if (Character.isDigit(chr)) {
					strbuffer.append(chr);
				} else {
					if (chr == '.' || chr == 'e' || chr == 'E') {
						strbuffer.append(chr);
						state = TokenizerState.FLOAT;
					} else {
						reader.pushCharBack(chr);
						return new TokenizerResult(makeTermFromString(
								strbuffer.toString(), state),
								TokenizerState.INTEGER);
					}
				}
				break;
			case FLOAT:
				if (Character.isDigit(chr)) {
					strbuffer.append(chr);
				} else {
					if (chr == '-' || chr == '+') {
						if (strbuffer.charAt(strbuffer.length() - 1) == 'e') {
							strbuffer.append(chr);
						} else {
							reader.pushCharBack(chr);
							return new TokenizerResult(makeTermFromString(
									strbuffer.toString(), TokenizerState.FLOAT),
									TokenizerState.FLOAT);
						}
					} else if (chr == 'e' || chr == 'E') {
						if (strbuffer.indexOf("e") < 0) {
							strbuffer.append('e');
						} else {
							reader.pushCharBack(chr);
							return new TokenizerResult(makeTermFromString(
									strbuffer.substring(0,
											strbuffer.length() - 1),
									TokenizerState.FLOAT), TokenizerState.FLOAT);
						}
					} else {

						reader.pushCharBack(chr);

						if (strbuffer.charAt(strbuffer.length() - 1) == '.') {
							// it was an integer
							reader.pushCharBack('.');
							return new TokenizerResult(makeTermFromString(
									strbuffer.substring(0,
											strbuffer.length() - 1),
									TokenizerState.INTEGER), TokenizerState.INTEGER);
						} else {
							// it is float
							return new TokenizerResult(makeTermFromString(
									strbuffer.toString(), state), state);
						}
					}
				}
				break;
			case OPERATOR:
				if (chr != '_'
						&& letterOrDigitOnly != Character.isLetterOrDigit(chr)) {
					reader.pushCharBack(chr);

					if (lastFoundFullOperator != null) {
						return new TokenizerResult(lastFoundFullOperator, state);
					} else {
						return new TokenizerResult(makeTermFromString(
								strbuffer.toString(), state), state);
					}
				} else {
					final OperatorContainer prevoperators = lastFoundFullOperator;
					strbuffer.append(chr);
					final String operator = strbuffer.toString();
					lastFoundFullOperator = findOperatorForName(operator,
							parserContext);
					if (prevoperators != null) {
						if (lastFoundFullOperator == null) {
							if (!hasOperatorStartsWith(operator, parserContext)) {
								if (letterOrDigitOnly) {
									state = TokenizerState.ATOM;
								} else {
									reader.calculateDifferenceAndPushTheResultBack(
											prevoperators.getText(), strbuffer);
									return new TokenizerResult(prevoperators,
											state);
								}
							} else {
								lastFoundFullOperator = prevoperators;
							}

						} else {
							if (!hasOperatorStartsWith(operator, parserContext)) {
								reader.calculateDifferenceAndPushTheResultBack(
										prevoperators.getText(), strbuffer);
								return new TokenizerResult(prevoperators, state);
							}
						}
					} else {
						if (!hasOperatorStartsWith(operator, parserContext)) {
							if (hasOperatorStartsWith(Character.toString(chr),
									parserContext)) {
								// next char can be the start char of an
								// operator so we need get back it into the
								// buffer
								strbuffer.setLength(strbuffer.length() - 1);
								reader.pushCharBack(chr);
							}
							state = TokenizerState.ATOM;
						}
					}
				}
				break;
			case STRING:
				if (specialchar) {
					if (specialCharBuffer.length() == 0 && chr == '\n') {
						// just add the next line code
						strbuffer.append('\n');
						specialchar = false;
					} else {
						// try to parse special char
						specialCharBuffer.append(chr);
						if (StringUtils
								.unescapeCharacter(
										specialCharBuffer.toString(),
										specialCharResult)) {
							// special character detected and it doesn't have
							// errors
							if (specialCharResult.get() != null) {
								// the special char fully parsed
								strbuffer.append(specialCharResult.get()
										.charValue());
								specialchar = false;
							}
						} else {
							if (specialCharResult.get() == null) {
								// error special character detected, so throw an
								// exception
								throw new PrologParserException(
										"Unsupported special char [\\"
												+ specialCharBuffer.toString()
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
								strbuffer.toString(), state), state);
					case '\\':
						specialchar = true;
						specialCharBuffer.setLength(0);
						break;
					default:
						strbuffer.append(chr);
						break;
					}
				}
				break;
			case VARIABLE:
				if (Character.isISOControl(chr) || Character.isWhitespace(chr)) {
					final String name = strbuffer.toString();
					if (name.equals("_")) {
						return new TokenizerResult(new PrologVariable(), state);
					}
					return new TokenizerResult(new PrologVariable(name), state);
				} else if (chr != '_' && !Character.isLetterOrDigit(chr)) {
					reader.pushCharBack(chr);
					final String name = strbuffer.toString();
					if (name.equals("_")) {
						return new TokenizerResult(new PrologVariable(), state);
					}
					return new TokenizerResult(new PrologVariable(name), state);
				} else {
					strbuffer.append(chr);
				}
				break;
			default:
				throw new Error("Unexpected state detected");
			}
		}
	}

	/**
	 * Inside auxiliary function to make a term from a String
	 * 
	 * @param string
	 *            the source string object, must not be null
	 * @param state
	 *            the state of inside state machine which was set during the
	 *            term reading
	 * @return a Term object as the result, not-null value will be returned
	 *         anyway
	 */
	AbstractPrologTerm makeTermFromString(final String string,
			final TokenizerState state) {
		AbstractPrologTerm result = null;

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
	 * @param operatorNameStartSubstring
	 *            the start substring to be checked as the operator start name,
	 *            must not be null
	 * @param parserContext
	 *            a prolog context, must not be null
	 * @return true if there is any operator starts with the string, else false
	 */
	static boolean hasOperatorStartsWith(
			final String operatorNameStartSubstring,
			final ParserContext parserContext) {

		if (operatorNameStartSubstring == null)
			throw new NullPointerException("Substing is null");

		// check for system
		if (PrologParser.SYSTEM_OPERATORS_PREFIXES
				.contains(operatorNameStartSubstring))
			return true;

		// check only context
		if (parserContext != null)
			return parserContext
					.hasOperatorStartsWith(operatorNameStartSubstring);
		else
			return false;
	}

	/**
	 * Function to find an operator for its name, as the first it will search
	 * among system operators then in the prolog context.
	 * 
	 * @param operatorName
	 *            an operator name to be used for search, must not be null
	 * @param parserContext
	 *            a prolog context, it can be null
	 * @return an OperatorContainer if the operator is presented, else null
	 */
	static OperatorContainer findOperatorForName(final String operatorName,
			final ParserContext parserContext) {
		if (operatorName == null)
			throw new NullPointerException("Operator name is null");

		OperatorContainer result = null;
		result = PrologParser.SYSTEM_OPERATORS.get(operatorName);

		if (result == null && parserContext != null)
			result = parserContext.findOperatorForName(operatorName);

		return result;
	}

}
