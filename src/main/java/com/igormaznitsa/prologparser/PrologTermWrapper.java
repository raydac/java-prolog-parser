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

import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import com.igormaznitsa.prologparser.utils.ringbuffer.SoftCache;
import com.igormaznitsa.prologparser.utils.ringbuffer.SoftCacheItem;

/**
 * An Auxiliary class allows to make a wrapper containing a source stream position and a linked object for a singleton prolog term.
 * It is used by the prolog parser during tree building so it is a package level class.
 * 
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * 
 * @see Operator
 * @see OperatorContainer
 */
final class PrologTermWrapper extends AbstractPrologTerm implements SoftCacheItem {
    private static final long serialVersionUID = 9006607815982718325L;
    private volatile AbstractPrologTerm wrappedTerm;
    private volatile SoftCache<PrologTermWrapper> ringBuffer;
    
    PrologTermWrapper() {
        super("termWrapper");
    }

    public void setWrappedTerm(final AbstractPrologTerm term){
      this.wrappedTerm = term;
    }
    
    public AbstractPrologTerm getWrappedTerm() {
        return this.wrappedTerm;
    }

    @Override
    public PrologTermType getType() {
        return wrappedTerm.getType();
    }

    @Override
    public String getText() {
        return wrappedTerm.getText();
    }

    @Override
    public int getPriority() {
        return wrappedTerm.getPriority();
    }

    @Override
    public String toString() {
        return wrappedTerm.toString();
    }

    @Override
    @SuppressWarnings("unchecked")
    public void setSoftCache(final SoftCache<? extends SoftCacheItem> ringBuffer) {
        this.ringBuffer = (SoftCache<PrologTermWrapper>)ringBuffer;
    }

    @Override
    public void reset() {
        this.wrappedTerm = null;
    }

    @Override
    public void dispose() {
        this.ringBuffer.dispose(this);
    }
}
