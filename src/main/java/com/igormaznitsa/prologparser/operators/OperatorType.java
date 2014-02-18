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
