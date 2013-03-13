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

/**
 * This enumeration contains all inside states for the prolog tokenizer
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @see PrologTokenizer
 */
public enum TokenizerState {

    /**
     * It shows that the parser is looking for the next token.
     */
    LOOKFOR,
    /**
     * It shows that the parser in the atom reading state.
     */
    ATOM,
    /**
     * It shows that the parser in the string reading state.
     */
    STRING,
    /**
     * It shows that the parser in the operator reading state.
     */
    OPERATOR,
    /**
     * It shows that the parser in the variable reading state.
     */
    VARIABLE,
    /**
     * It shows that the parser in the integer number reading state.
     */
    INTEGER,
    /**
     * It shows that the parser in the float number reading state.
     */
    FLOAT;
}
