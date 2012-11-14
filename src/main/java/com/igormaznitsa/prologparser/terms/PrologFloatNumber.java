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

import static com.igormaznitsa.prologparser.utils.AssertionUtils.*;
import java.math.BigDecimal;
import java.math.MathContext;

/**
 * The class describes a float numeric atom for the prolog parser. It looks like
 * the prolog atom but contains a Java BigDecimal value and doesn't save the
 * text value which was used to create the atom.
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 *
 * @see BigDecimal
 */
public final class PrologFloatNumber extends AbstractPrologNumericTerm {

    private static final long serialVersionUID = -8375787317103540082L;
    /**
     * The math context to be used for float numbers
     */
    public static final MathContext MATH_CONTEXT = MathContext.DECIMAL64;
    /**
     * The variable contains the immutable BigDecimal value.
     */
    private final BigDecimal value;

    /**
     * A Constructor. It allows to create new instance based on the text
     * representation.
     *
     * @param text the text compatibles with Java BigDecimal type. Must not be
     * null.
     * @throws NumberFormatException will be thrown if the text can't be parsed
     * as a BigDeclmal value.
     * @see PrologFloatNumber#MATH_CONTEXT
     */
    public PrologFloatNumber(final String text) {
        this(new BigDecimal(text, MATH_CONTEXT));
    }

    /**
     * A Constructor allows to make new instance based on a text representation
     * (BigDecimal compatible) and set the source stream position of the first
     * term char
     *
     * @param text the text in the Java BigDecimal compatible format. Must not
     * be null.
     * @param strPos the first term char string position in the source stream.
     * @param lineNumber the first term char line number in the source stream.
     * @throws NumberFormatException will be thrown if the text format is not
     * compatible with the BigDecimal representation.
     * @see PrologFloatNumber#MATH_CONTEXT
     */
    public PrologFloatNumber(final String text, final int strPos, final int lineNumber) {
        this(new BigDecimal(text, MATH_CONTEXT), strPos, lineNumber);
    }

    /**
     * A Constructor. It allows to create new instance based on a double value.
     *
     * @param value the double value being used for new instance.
     */
    public PrologFloatNumber(final double value) {
        this(BigDecimal.valueOf(value));
    }

    /**
     * A Constructor. It allows to create new instance based on a double value
     * and set the first term char position values in the source stream.
     *
     * @param value the double value to make new instance.
     * @param strPosition the first term char string position
     * @param lineNumber the first term char line number
     */
    public PrologFloatNumber(final double value, final int strPosition, final int lineNumber) {
        this(BigDecimal.valueOf(value), strPosition, lineNumber);
    }

    /**
     * A Constructor. It allows to create new instance based on a BigDecimal
     * value.
     *
     * @param value the BigDecimal value to be represented by the instance, must
     * not be null
     */
    public PrologFloatNumber(final BigDecimal value) {
        super();
        checkNotNull("Value is null", value);
        this.value = value;
    }

    /**
     * A Constructor. It allows to make an instance based on a BigDecimal value
     * and set the first term char position values in the source stream.
     *
     * @param value the BigDecimal value to be used for new instance, must not
     * be null.
     * @param strPosition the first term char string position
     * @param lineNumber the first term char line number
     */
    public PrologFloatNumber(final BigDecimal value, final int strPosition, final int lineNumber) {
        this(value);
        setStrPosition(strPosition);
        setLineNumber(lineNumber);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AbstractPrologNumericTerm neg() {
        return new PrologFloatNumber(value.negate());
    }

    /**
     * Get the BigDecimal value saved by the object.
     *
     * @return the BigDecimal value saved by the object
     */
    public BigDecimal getValue() {
        return value;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        final String result = value.toEngineeringString();
        return result.indexOf('.') < 0 ? result + ".0" : result;
    }
}
