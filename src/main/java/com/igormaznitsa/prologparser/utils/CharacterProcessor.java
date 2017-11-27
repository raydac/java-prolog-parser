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
