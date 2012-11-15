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
package com.igormaznitsa.prologparser.operators;

/**
 * The enumeration describes prolog operator types being used by the prolog
 * parser.
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 */
public enum OperatorType {

    XF("xf",1), YF("yf",1), FX("fx",1), FY("fy",1), XFX("xfx",2), XFY("xfy",2), YFX("yfx",2);
    /**
     * The variable contains right prolog text representation of the type
     */
    private final String text;

    /**
     * The variable saves the arity for the type.
     */
    private final int arity;
    
    /**
     * The constructor to create an instance and set the right text
     *
     * @param text the right prolog text for the type, must not be null
     */
    private OperatorType(final String text, final int arity) {
        this.text = text;
        this.arity = arity;
    }

    /**
     * Get the arity for the type.
     * @return the arity, it can be either 1 or 2
     */
    public int getArity(){
        return this.arity;
    }
    

    /**
     * Find an operator type for its prolog text representation
     *
     * @param str the prolog text representation for the finding type
     * @return found type or null if the type has not been found
     */
    public static OperatorType getForName(final String str) {
        for (final OperatorType type : values()) {
            if (type.text.equals(str)) {
                return type;
            }
        }
        return null;
    }

    /**
     * Get right prolog text representation
     *
     * @return the text as String
     */
    public String getText() {
        return text;
    }
}
