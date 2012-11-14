/*
 * Copyright 2011-2012 Igor Maznitsa (http://www.igormaznitsa.com)
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

/**
 * The class allows to make a wrapper containing a source stream position and a linked object for a singleton prolog term.
 * It is used by the prolog parser during tree building so it is a package level class.
 * 
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * 
 * @see Operator
 * @see OperatorContainer
 */
@SuppressWarnings("serial")
class PrologTermWrapper extends AbstractPrologTerm {

    private final AbstractPrologTerm wrappedTerm;

    public PrologTermWrapper(final AbstractPrologTerm wrappedTerm) {
        super("wrapper[" + wrappedTerm.getText() + ']');
        this.wrappedTerm = wrappedTerm;
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
}
