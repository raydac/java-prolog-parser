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
package com.igormaznitsa.prologparser.exceptions;

/**
 * The class is an exception and being used by the prolog parser to notify user
 * about a syntax prolog problem and its position in the source.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 */
public class PrologParserException extends Exception {
    private static final long serialVersionUID = -4454323844625857706L;

    /**
     * The variable contains the line number of the problem. The first line has
     * the index 1.
     */
    protected final int lineNumber;
    /**
     * The variable contains the string position of the problem. The first char
     * has the index 1.
     */
    protected final int stringPosition;

    /**
     * The constructor.
     * 
     * @param text
     *            the text of the problem. It can be null.
     * @param lineNumber
     *            the line number of the problem.
     * @param stringPos
     *            the string position of the problem.
     */
    public PrologParserException(final String text, final int lineNumber,
            final int stringPos) {
        super(text);
        this.lineNumber = lineNumber;
        this.stringPosition = stringPos;
    }

    /**
     * Get the line number of the problem.
     * 
     * @return the line number as integer.
     */
    public int getLineNumber() {
        return lineNumber;
    }

    /**
     * Get the string position of the problem.
     * 
     * @return the string position as integer.
     */
    public int getStringPosition() {
        return stringPosition;
    }

    /**
     * Allows to check that the exception contains valid line-string position
     * information, they must be more than zero.
     * 
     * @return true if both the line index and the string position are more than
     *         zero, else false
     */
    public boolean containsRightPositionData() {
        return lineNumber > 0 && stringPosition > 0;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return new StringBuilder(getMessage()).append('[').append(lineNumber).append(':').append(stringPosition).append(']').toString();
    }
}
