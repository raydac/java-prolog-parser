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
import java.math.BigInteger;

/**
 * The class describes an immutable integer numeric atom, it is like a prolog
 * atom but being used to save a Java BigInteger value.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @see BigInteger
 */
public final class PrologIntegerNumber extends AbstractPrologNumericTerm {
    private static final long serialVersionUID = 6955747225309951985L;

    /**
     * The variable contains immutable numeric value for the instance.
     */
    private final BigInteger value;

    /**
     * A Constructor. It allows to create new instance based on a text 
     * numeric representation compatibles with BigInteger object.
     * 
     * @param text
     *            the text represents a BigInteger value, must not be null
     */
    public PrologIntegerNumber(final String text) {
        this(new BigInteger(text,10));
    }

    /**
     * A Constructor. It allows to create new instance based on a text compatibles with Java BigInteger class
     * @param text the text represents numeric value in BigInteger compatible manner, must not be null
     * @param strPos the first term char string position
     * @param lineNum the first term char line number
     * @see BigInteger
     */
    public PrologIntegerNumber(final String text, final int strPos, final int lineNum) {
        this(new BigInteger(text,10), strPos, lineNum);
    }

    /**
     * A Constructor. It allows to create new instances based on a Java long number. 
     * value.
     * @param value a long value to make new instance.
     */
    public PrologIntegerNumber(final long value) {
        this(BigInteger.valueOf(value));
    }

    /**
     * A Constructor. It allows to make new instances based on a Java long number. 
     * @param value a long value to make new instance.
     * @param strPos the first term char string position
     * @param lineNumber the first term char line number
     */
    public PrologIntegerNumber(final long value, final int strPos, final int lineNumber) {
        this(BigInteger.valueOf(value), strPos, lineNumber);
    }

    /**
     * A Constructor. It allows to create new instance based on a Java BigInteger value.
     * 
     * @param value the value to be saved in the new instance, must not be null
     */
    public PrologIntegerNumber(final BigInteger value) {
        super();
        checkNotNull("Value is null", value);
        this.value = value;
    }

    /**
     * A Constructor. It allows to create new instance based on a Java BigInteger value.
     * @param value the value to be saved into the new instance, must not be null.
     * @param strPosition the first term char string position
     * @param lineNumber the first term char line number
     */
    public PrologIntegerNumber(final BigInteger value, final int strPosition, final int lineNumber) {
        this(value);
        setStrPosition(strPosition);
        setLineNumber(lineNumber);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AbstractPrologNumericTerm neg() {
        return new PrologIntegerNumber(value.negate());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return value.toString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getText() {
        return toString();
    }

    /**
     * Get the immutable value saved by the instance
     * 
     * @return the value as BigInteger
     */
    public BigInteger getValue() {
        return value;
    }
}
