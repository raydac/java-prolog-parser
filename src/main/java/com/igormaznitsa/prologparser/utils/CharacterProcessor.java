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
