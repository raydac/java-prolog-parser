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

import com.igormaznitsa.prologparser.annotations.PrologOperator;
import com.igormaznitsa.prologparser.annotations.PrologOperators;

/**
 * The enumeration contains all prolog term types being used by the prolog
 * parser
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.00
 */
public enum PrologTermType {

    /**
     * The constant describes a prolog atom
     * 
     * @see PrologAtom
     * @since 1.00
     */
    ATOM,
    /**
     * The constant describes a prolog structure
     * 
     * @see PrologStructure
     * @since 1.00
     */
    STRUCT,
    /**
     * The constant describes a prolog variable
     * 
     * @see PrologVariable
     * @since 1.00
     */
    VAR,
    /**
     * The constant describes a prolog operator
     * 
     * @see PrologOperator
     * @since 1.00
     */
    OPERATOR,
    /**
     * The constant describes an operator container, it is an auxiliary object
     * to save similar named operators on the intermediate processing level
     * 
     * @see PrologOperators
     * @since 1.00
     */
    OPERATORS,
    /**
     * The constant describes a prolog list
     * 
     * @see PrologList
     * @since 1.00
     */
    LIST
}
