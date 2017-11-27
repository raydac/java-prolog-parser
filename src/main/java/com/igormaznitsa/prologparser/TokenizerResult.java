/* 
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import com.igormaznitsa.prologparser.utils.ringbuffer.SoftCache;
import com.igormaznitsa.prologparser.utils.ringbuffer.SoftCacheItem;

import static com.igormaznitsa.prologparser.utils.Assert.assertNotNull;

/**
 * The class is a container to save information about parsed token, tokenizer
 * state during that operation and the parsed term.
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 */
public final class TokenizerResult implements SoftCacheItem {

    private SoftCache<? extends SoftCacheItem> ringBuffer;

    /**
     * The variable contains the tokenizer state during the reading of the term.
     */
    private TokenizerState parserState;
    /**
     * The variable contains the result term.
     */
    private AbstractPrologTerm resultTerm;
    /**
     * The variable contains the string position where the first result char is
     * presented.
     */
    private int stringPosition;
    /**
     * The variable contains the line number where the first result char is
     * presented.
     */
    private int lineNumber;


    public TokenizerResult() {

    }

    /**
     * The constructor.
     *
     * @param term           the term to be saved in the object, must not be null.
     * @param parserState    the tokenizer state to be saved in the object, must
     *                       not be null.
     * @param stringPosition the first result char string position
     * @param lineNumber     the first result char line number
     */
    public TokenizerResult(final AbstractPrologTerm term,
                           final TokenizerState parserState, final int stringPosition, final int lineNumber) {
        setData(term, parserState, stringPosition, lineNumber);
    }

    @Override
    public void setSoftCache(final SoftCache<? extends SoftCacheItem> owner) {
        this.ringBuffer = owner;
    }

    @Override
    public void dispose() {
        if (ringBuffer != null) {
            ringBuffer.dispose(this);
        }
    }

    @Override
    public void reset() {
        this.resultTerm = null;
        this.parserState = null;
        this.lineNumber = -1;
        this.stringPosition = -1;
    }

    public void setData(final AbstractPrologTerm term,
                        final TokenizerState parserState, final int stringPosition, final int lineNumber) {
        assertNotNull("The term is null", term);
        assertNotNull("The Parser state is null", parserState);

        this.stringPosition = stringPosition;
        this.lineNumber = lineNumber;
        this.resultTerm = term;
        this.parserState = parserState;
    }

    /**
     * Get the tokenizer state.
     *
     * @return the tokenizer state.
     */
    public TokenizerState getTokenizerState() {
        return parserState;
    }

    /**
     * Get the prolog term.
     *
     * @return the prolog term.
     */
    public AbstractPrologTerm getResult() {
        return resultTerm;
    }

    /**
     * Get the prolog term type.
     *
     * @return the prolog term type.
     */
    public PrologTermType getTermType() {
        return resultTerm.getType();
    }

    /**
     * Get the first result char string position
     *
     * @return the first result char position at string
     */
    public int getStringPosition() {
        return stringPosition;
    }

    /**
     * Get the first result char line number
     *
     * @return the first result char line number
     */
    public int getLineNumber() {
        return lineNumber;
    }
}
