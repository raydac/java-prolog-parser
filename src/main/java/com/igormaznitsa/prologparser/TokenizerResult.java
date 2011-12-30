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
import static com.igormaznitsa.prologparser.utils.AssertionUtils.*;

/**
 * The class is a container to save information about parsed token, tokenizer
 * state during that operation and the parsed term.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.01
 */
@SuppressWarnings("serial")
public final class TokenizerResult {

    /**
     * The variable contains the tokenizer state during the reading of the term
     * @since 1.00
     */
    private final TokenizerState parserState;
    /**
     * The variable contains the result term
     * @since 1.00
     */
    private final AbstractPrologTerm resultTerm;
    /**
     * The variable contains the string position where the first result char is presented
     * @since 1.01
     */
    private final int stringPosition;
    /**
     * The variable contains the line number where the first result char is presented
     * @since 1.01
     */
    private final int lineNumber;

    /**
     * The constructor.
     * 
     * @param term
     *            the term to be saved in the object, must not be null.
     * @param parserState
     *            the tokenizer state to be saved in the object, must not be
     *            null.
     * @param stringPosition the first result char string position
     * @param lineNumber the first result char line number
     * @since 1.01
     */
    public TokenizerResult(final AbstractPrologTerm term,
            final TokenizerState parserState, final int stringPosition, final int lineNumber) {
        checkNotNull("The term is null", term);
        checkNotNull("The Parser state is null", parserState);

        this.stringPosition = stringPosition;
        this.lineNumber = lineNumber;
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

    /**
     * Get the first result char string position
     * @return the first result char position at string
     * @since 1.01
     */
    public int getStringPosition() {
        return stringPosition;
    }

    /**
     * Get the first result char line number
     * @return the first result char line number
     * @since 1.01
     */
    public int getLineNumber() {
        return lineNumber;
    }
}
