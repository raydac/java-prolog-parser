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
 * The abstract class describes an abstract prolog term for the prolog parser.
 * All data types being used by the prolog parser are successors of the class.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.01
 */
public abstract class AbstractPrologTerm {

	/**
	 * The variable save any Java object, it is not being used by the prolog
	 * parser but allows a user to link some data to the structure, the default
	 * value is null
	 * 
	 * @since 1.01
	 */
	protected Object linkedObject;

	/**
	 * The variable contains the text for the term
	 * 
	 * @since 1.00
	 */
	protected final String text;

	/**
	 * The constructor allows to make new instance based on a text value.
	 * 
	 * @param text
	 *            the text representing the term
	 * @since 1.00
	 */
	public AbstractPrologTerm(final String text) {
		if (text == null) {
			throw new NullPointerException("Term text must not be null");
		}
		this.text = text;
	}

	/**
	 * Get the text representation of the term
	 * 
	 * @return the text as String
	 * @since 1.00
	 */
	public String getText() {
		return this.text;
	}

	/**
	 * Get the priority of the term
	 * 
	 * @return the priority as integer
	 * @since 1.00
	 */
	public int getPriority() {
		return 0;
	}

	/**
	 * Get a text prolog like representation for the term.
	 * 
	 * @return the text representation as a String
	 * @since 1.00
	 */
	@Override
	public String toString() {
		return text;
	}

	/**
	 * Set the linked object for the term
	 * 
	 * @param obj
	 *            the new linked object, it can be null
	 * @since 1.01
	 */
	public void setLinkedObject(final Object obj) {
		this.linkedObject = obj;
	}

	/**
	 * Get the linked object for the term
	 * 
	 * @return the linked object, it can be null
	 * @since 1.01
	 */
	public Object getLinkedObject() {
		return this.linkedObject;
	}

	/**
	 * Get the term type
	 * 
	 * @return the term type
	 * @since 1.00
	 */
	public abstract PrologTermType getType();
}
