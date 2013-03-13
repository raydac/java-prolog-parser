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
package com.igormaznitsa.prologparser.utils;

public abstract class CharacterProcessor {
    /**
     * The set contains flags or letter and digits chars.
     */
    protected static final CharFlagSet mapLettersOrDigits = new CharFlagSet();
    /**
     * The set contains flags for digits.
     */
    protected static final CharFlagSet mapDigits = new CharFlagSet();
    /**
     * The set contains flags for white spaces.
     */
    protected static final CharFlagSet mapWhitespace = new CharFlagSet();
    /**
     * The set contains flags for iso controls.
     */
    protected static final CharFlagSet mapIsoControl = new CharFlagSet();
    /**
     * The set contains flags for spaces and iso controls.
     */
    protected static final CharFlagSet mapWhiteSpaceOrIsoControl = new CharFlagSet();
    /**
     * The set contains flags for upper cased letters.
     */
    protected static final CharFlagSet mapUpperCaseLetters = new CharFlagSet();

    static {
        // init char flag sets
        for (int i = 0; i < 0x10000; i++) {
            final char ch = (char) i;

            if (Character.isLetterOrDigit(ch)) {
                mapLettersOrDigits.addChar(ch);
            }

            if (Character.isDigit(ch)) {
                mapDigits.addChar(ch);
            }

            if (Character.isWhitespace(ch)) {
                mapWhitespace.addChar(ch);
                mapWhiteSpaceOrIsoControl.addChar(ch);
            }

            if (Character.isISOControl(ch)) {
                mapIsoControl.addChar(ch);
                mapWhiteSpaceOrIsoControl.addChar(ch);
            }

            if (Character.isUpperCase(ch)) {
                mapUpperCaseLetters.addChar(ch);
            }
        }
    }
}
