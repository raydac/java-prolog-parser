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
 * @version 1.02
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
	 * The variable contains the line number for the term in the source stream 
	 * @since 1.02
	 */
	private int lineNumber;
	
	/**
	 * The variable contains the string position of the first term char in the source stream
	 * @since 1.02
	 */
	private int strPosition;
	
	/**
	 * The constructor allows to make new instance based on a text value.
	 * The string position and the line number will have -1 after the constructor.
	 * @param text
	 *            the text representing the term, must not be null
	 * @since 1.00
	 */
	public AbstractPrologTerm(final String text) {
		if (text == null) {
			throw new NullPointerException("Term text must not be null");
		}
		this.text = text;
		this.strPosition = -1;
		this.lineNumber = -1;
	}

	/**
	 * The constructor allows to make new instance based on a text value and set both the string position and the line number values
	 * @param text the text representing the term, must not be null
	 * @param strPosition the string position of the first term char in the source stream, the first char is 1
	 * @param lineNumber the line number of the first term char in the source stream, the first line is 1
	 * @since 1.02
	 */
	public AbstractPrologTerm(final String text, final int strPosition, final int lineNumber) {
		this(text);
		setStrPosition(strPosition);
		setLineNumber(lineNumber);
	}
	
	/**
	 * Get the string position of the first char of the term in the source stream.
	 * @return the string position as integer value, the first line char is 1, it is -1 if undefined
	 * @since 1.02
	 */
	public int getStrPosition() {
		return strPosition;
	}
	
	/**
	 * Set the string position of the term in the source stream. If the position is zero or less than zero then the string position will be -1.
	 * @param strPosition the value to be set as the string position
	 * @since 1.02
	 */
	public void setStrPosition(final int strPosition) {
		this.strPosition = strPosition <= 0 ? -1 : strPosition;
	}
	
	/**
	 * Get the line number of the first term char in the source stream.
	 * @return the line number as integer value, the first line is 1, if undefined then -1
	 * @since 1.02
	 */
	public int getLineNumber() {
		return lineNumber;
	}
	
	/**
	 * Set the first term char line number in the source stream
	 * @param lineNumber the line number, if it is zero or less then the value will be -1
	 * @since 1.02
	 */
	public void setLineNumber(final int lineNumber) {
		this.lineNumber = lineNumber <= 0 ? -1 : lineNumber;
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
