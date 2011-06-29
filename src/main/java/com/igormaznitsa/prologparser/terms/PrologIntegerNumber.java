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

/**
 * The class describes an immutable integer numeric atom, it is like a prolog
 * atom but being used to save a Java long value.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.00
 */
public final class PrologIntegerNumber extends AbstractPrologNumericTerm {
	/**
	 * The variable contains immutable numeric value for the instance.
	 */
	private final long value;

	/**
	 * A Constructor. It allows to create new instance based on a text long
	 * numeric representation.
	 * 
	 * @param text
	 *            the text represents a long value, must not be null
	 * @since 1.00
	 */
	public PrologIntegerNumber(final String text) {
		super();
		if (text == null)
			throw new NullPointerException("Text is null");
		this.value = Long.parseLong(text);
	}

	/**
	 * A Constructor. It allows to create new instance based on a Java long
	 * value.
	 * 
	 * @param value
	 *            the long value being used to create new instance
	 * @since 1.00
	 */
	public PrologIntegerNumber(final long value) {
		super();
		this.value = value;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public AbstractPrologNumericTerm neg() {
		return new PrologIntegerNumber(0 - value);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String toString() {
		return Long.toString(value);
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
	 * @return the value as long
	 * @since 1.00
	 */
	public long getValue() {
		return value;
	}

}
