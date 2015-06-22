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
package com.igormaznitsa.prologparser.terms;

import static com.igormaznitsa.prologparser.utils.Assert.*;
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
        assertNotNull("Value is null", value);
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
