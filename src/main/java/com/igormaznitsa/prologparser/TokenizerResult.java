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
package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologTermType;

/**
 * The class is a container to save information about parsed token, tokenizer
 * state during that operation and the parsed term.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.00
 */
public final class TokenizerResult {
	/**
	 * The variable contains the tokenizer state during the reading of the term
	 */
	private final TokenizerState parserState;

	/**
	 * The variable contains the result term
	 */
	private final AbstractPrologTerm resultTerm;

	/**
	 * The constructor.
	 * 
	 * @param term
	 *            the term to be saved in the object, must not be null.
	 * @param parserState
	 *            the tokenizer state to be saved in the object, must not be
	 *            null.
	 * @since 1.00
	 */
	public TokenizerResult(final AbstractPrologTerm term,
			final TokenizerState parserState) {
		if (term == null)
			throw new NullPointerException("Term is null");

		if (parserState == null)
			throw new NullPointerException("Parser state is null");

		this.resultTerm = term;
		this.parserState = parserState;
	}

	/**
	 * Get the tokenizer state.
	 * 
	 * @return the tokenizer state.
	 * @since 1.00
	 */
	public TokenizerState getTokenizerState() {
		return parserState;
	}

	/**
	 * Get the prolog term.
	 * 
	 * @return the prolog term.
	 * @since 1.00
	 */
	public AbstractPrologTerm getResult() {
		return resultTerm;
	}

	/**
	 * Get the prolog term type.
	 * 
	 * @return the prolog term type.
	 * @since 1.00
	 */
	public PrologTermType getTermType() {
		return resultTerm.getType();
	}
}
