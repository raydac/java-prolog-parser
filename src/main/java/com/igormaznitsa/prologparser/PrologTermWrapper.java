/*
 * Copyright 2011-2013 Igor Maznitsa (http://www.igormaznitsa.com)
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
