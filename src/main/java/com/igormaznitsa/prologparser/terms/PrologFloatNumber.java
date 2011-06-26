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
package com.igormaznitsa.prologparser.terms;

/**
 * The class describes a float numeric atom for the prolog parser. It looks like the prolog atom but contains a Java double value and doesnt save the text value which was used during the constructor call.
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.00
 */
public final class PrologFloatNumber extends AbstractPrologNumericTerm {

	/**
	 * The variable contains the immutable double value.
	 */
	private final double value;

	/**
	 * A Constructor. It allows to create new instance based on the text representation.
	 * @param text the text compatibles with Java double type. Must not be null.
	 * @throws NumberFormatException will be thrown if the text can't be parsed as a Double value.
	 */
	public PrologFloatNumber(final String text) {
		super();
		value = Double.parseDouble(text);
	}

	/**
	 * A Constructor. It allows to create new instance based on a double value.
	 * @param value the double value being used for new instance.
	 */
	public PrologFloatNumber(final double value) {
		super();
		this.value = value;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public AbstractPrologNumericTerm neg() {
		return new PrologFloatNumber(0 - value);
	}

	/**
	 * Get the double value saved by the object.
	 * @return the double value
	 */
	public double getValue(){
		return value;
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public String toString(){
		return Double.toString(value);
	}
}
