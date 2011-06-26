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
 * The abstract class describes an abstract prolog term for the prolog parser. All data types being used by the prolog parser are successors of the class.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.00
 */
public abstract class AbstractPrologTerm {
	/**
	 * The variable contains the text for the term
	 */
	protected final String text;

	/**
	 * The constructor allows to make new instance based on a text value.
	 * @param text the text representing the term
	 */
	public AbstractPrologTerm(final String text) {
		if (text == null) {
			throw new NullPointerException("Term text must not be null");
		}
		this.text = text;
	}

	/**
	 * Get the text representation of the term
	 * @return the text as String
	 */
	public String getText() {
		return this.text;
	}

	/**
	 * Get the priority of the term
	 * @return the priority as integer
	 */
	public int getPriority() {
		return 0;
	}

	/**
	 * Get a text prolog like representation for the term.
	 * @return the text representation as a String
	 */
	@Override
	public String toString() {
		return text;
	}

	/**
	 * Get the term type
	 * @return the term type
	 */
	public abstract PrologTermType getType();
}
