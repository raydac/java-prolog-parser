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
package com.igormaznitsa.prologparser.terms;

/**
 * The abstract class is the base for all prolog numeric term types being used
 * by the prolog parser.
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @see PrologIntegerNumber
 * @see PrologFloatNumber
 */
public abstract class AbstractPrologNumericTerm extends AbstractPrologTerm {

    private static final long serialVersionUID = -1865562758090770438L;

    /**
     * A Constructor.
     */
    public AbstractPrologNumericTerm() {
        super("");
    }

    /**
     * A Constructor allows to set both the string first char position and the
     * line number
     *
     * @param strPosition the first term char string position
     * @param lineNumber  the first term char line number
     */
    public AbstractPrologNumericTerm(final int strPosition, final int lineNumber) {
        super("", strPosition, lineNumber);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final PrologTermType getType() {
        return PrologTermType.ATOM;
    }

    /**
     * It will return a text representation for the numeric.
     */
    @Override
    public String getText() {
        return toString();
    }

    /**
     * The method allows to get the negative value of the number saved by an
     * instance as new numeric term instance
     *
     * @return new numeric term instance contains the negative representation of
     * the value
     */
    public abstract AbstractPrologNumericTerm neg();
}
