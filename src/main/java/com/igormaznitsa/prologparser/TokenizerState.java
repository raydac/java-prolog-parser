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

/**
 * This enumeration contains all inside states for the prolog tokenizer
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.00
 * @see PrologTokenizer
 */
public enum TokenizerState {

    /**
     * It shows that the parser is looking for the next token
     * 
     * @since 1.00
     */
    LOOKFOR,
    /**
     * It shows that the parser in the atom reading state
     * 
     * @since 1.00
     */
    ATOM,
    /**
     * It shows that the parser in the string reading state
     * 
     * @since 1.00
     */
    STRING,
    /**
     * It shows that the parser in the operator reading state
     * 
     * @since 1.00
     */
    OPERATOR,
    /**
     * It shows that the parser in the variable reading state
     * 
     * @since 1.00
     */
    VARIABLE,
    /**
     * It shows that the parser in the integer number reading state
     * 
     * @since 1.00
     */
    INTEGER,
    /**
     * It shows that the parser in the float number reading state
     * 
     * @since 1.00
     */
    FLOAT;
}
