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
package com.igormaznitsa.prologparser.terms;

import java.math.BigDecimal;
import java.math.MathContext;

/**
 * The class describes a float numeric atom for the prolog parser. It looks like
 * the prolog atom but contains a Java BigDecimal value and doesn't save the text
 * value which was used to create the atom.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.01
 */
public final class PrologFloatNumber extends AbstractPrologNumericTerm {

	/**
	 * The math context to be used for float numbers
	 * @since 1.01
	 */
	public static final MathContext MATH_CONTEXT = MathContext.DECIMAL64;
	
	/**
	 * The variable contains the immutable BigDecimal value.
	 * @since 1.01
	 */
	private final BigDecimal value;

	/**
	 * A Constructor. It allows to create new instance based on the text
	 * representation.
	 * 
	 * @param text
	 *            the text compatibles with Java BigDecimal type. Must not be null.
	 * @throws NumberFormatException
	 *             will be thrown if the text can't be parsed as a BigDeclmal value.
	 * @since 1.00
	 */
	public PrologFloatNumber(final String text) {
		this(new BigDecimal(text,MATH_CONTEXT));
	}

	/**
	 * A Constructor. It allows to create new instance based on a double value.
	 * 
	 * @param value
	 *            the double value being used for new instance.
	 * @since 1.00
	 */
	public PrologFloatNumber(final double value) {
		this(BigDecimal.valueOf(value));
	}

	/**
	 * A Constructor. It allows to create new instance based on a BigDecimal value.
	 * @param value the BigDecimal value to be represented by the instance, must not be null
	 */
	public PrologFloatNumber(final BigDecimal value) {
		super();
		if (value == null)
			throw new NullPointerException("Value is null");
		this.value = value;
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
	 * @since 1.01
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
		return result.indexOf('.')<0 ? result+".0" : result;
	}
}
