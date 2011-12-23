/*
 * Copyright 2011 Igor Maznitsa (http://www.igormaznitsa.com)
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
package com.igormaznitsa.prologparser.exceptions;

/**
 * The class is an exception and being used by the prolog parser to notify user
 * about a syntax prolog problem and its position in the source.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.00
 */
@SuppressWarnings("serial")
public class PrologParserException extends Exception {

    /**
     * The variable contains the line number of the problem. The first line has
     * the index 1.
     * 
     * @since 1.00
     */
    protected final int lineNumber;
    /**
     * The variable contains the string position of the problem. The first char
     * has the index 1.
     * 
     * @since 1.00
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
     * @since 1.00
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
