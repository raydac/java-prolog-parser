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
package com.igormaznitsa.prologparser.terms;

/**
 * The abstract class is the base for all prolog numeric term types being used
 * by the prolog parser.
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @see PrologIntegerNumber
 * @see PrologFloatNumber
 */
public abstract class AbstractPrologNumericTerm extends AbstractPrologTerm {

    private static final long serialVersionUID = -1865562758090770438L;

    /**
     * A Constructor.
     */
    public AbstractPrologNumericTerm() {
        super("");
    }

    /**
     * A Constructor allows to set both the string first char position and the
     * line number
     *
     * @param strPosition the first term char string position
     * @param lineNumber the first term char line number
     */
    public AbstractPrologNumericTerm(final int strPosition, final int lineNumber) {
        super("", strPosition, lineNumber);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final PrologTermType getType() {
        return PrologTermType.ATOM;
    }

    /**
     * It will return a text representation for the numeric.
     */
    @Override
    public String getText() {
        return toString();
    }

    /**
     * The method allows to get the negative value of the number saved by an
     * instance as new numeric term instance
     *
     * @return new numeric term instance contains the negative representation of
     * the value
     */
    public abstract AbstractPrologNumericTerm neg();
}
