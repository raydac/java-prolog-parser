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

import com.igormaznitsa.prologparser.utils.StringUtils;

/**
 * The class describes the main prolog type called 'atom', in some words it is
 * an immutable text part. A Prolog atom has always the top possible priority
 * (0).
 * 
 * @author Igor Maznitsa
 * @version 1.00
 */
public final class PrologAtom extends AbstractPrologTerm {

	/**
	 * The constructor allows to make an instance based on a text part.
	 * 
	 * @param text
	 *            the text to be used for new instance, must not be null
	 */
	public PrologAtom(final String text) {
		super(text);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public PrologTermType getType() {
		return PrologTermType.ATOM;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public int getPriority() {
		return 0;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String toString() {
		return new StringBuilder("\'").append(StringUtils.escapeString(text))
				.append('\'').toString();
	}

}
