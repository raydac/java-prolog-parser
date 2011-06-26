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
 * The class describes a prolog variable.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.00
 */
public final class PrologVariable extends AbstractPrologTerm {

	/**
	 * The variable contains the flag shows that the variable is an anonymous one
	 */
	private final boolean is_anonymous;

	/**
	 * A Constructor. It allows to create an anonymous variable.
	 */
	public PrologVariable() {
		this("_");
	}

	/**
	 * A Constructor . It allows to create a named variable (but also it can create and an anonymous one if the text is '_')
	 * @param text the name for the new variable, it can't be null and must use the prolog syntax variable naming rules
	 */
	public PrologVariable(final String text) {
		super(text);
		
		if (text.isEmpty()) {
			throw new IllegalArgumentException("Variable name is empty");
		}
			
		final char firstLetter = text.charAt(0); 
		
		if (!Character.isUpperCase(firstLetter) && firstLetter!='_'){
			throw new IllegalArgumentException("The variable name must be started from an upper case letter or '_' ["+text+']');
		}

		is_anonymous = firstLetter == '_' && text.length() == 1; 
	}

	/**
	 * Check that the variable is an anonymous one
	 * @return true if the variable is an anonymous one, else false
	 */
	public boolean isAnonymous() {
		return is_anonymous;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public PrologTermType getType() {
		return PrologTermType.VAR;
	}

}
