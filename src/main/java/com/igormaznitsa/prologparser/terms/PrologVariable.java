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

/**
 * The class describes a prolog variable.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.01
 */
@SuppressWarnings("serial")
public final class PrologVariable extends AbstractPrologTerm {

    /**
     * The variable contains the flag shows that the variable is an anonymous
     * one
     */
    private final boolean is_anonymous;
    /**
     * The variable contains a linked prolog variable, a variable with the same name in bounds of the same sentence
     * @since 1.01
     */
    private PrologVariable linkedVariable;

    /**
     * A Constructor. It allows to create an anonymous variable.
     * 
     * @since 1.00
     */
    public PrologVariable() {
        this("_");
    }

    /**
     * A Constructor. It allows to create an anonymous variable and set the source stream position
     * @param strPosition the variable char string position in the source stream
     * @param lineNumber the variable char line number in the source stream
     * @since 1.01
     */
    public PrologVariable(final int strPosition, final int lineNumber) {
        this();
        setStrPosition(strPosition);
        setLineNumber(lineNumber);
    }

    /**
     * A Constructor . It allows to create a named variable (but also it can
     * create and an anonymous one if the text is '_')
     * 
     * @param text
     *            the name for the new variable, it can't be null and must use
     *            the prolog syntax variable naming rules
     * @since 1.00
     */
    public PrologVariable(final String text) {
        super(text);

        if (text.isEmpty()) {
            throw new IllegalArgumentException("Variable name is empty");
        }

        final char firstLetter = text.charAt(0);

        if (!Character.isUpperCase(firstLetter) && firstLetter != '_') {
            throw new IllegalArgumentException(
                    "The variable name must be started from an upper case letter or '_' ["
                    + text + ']');
        }

        is_anonymous = firstLetter == '_' && text.length() == 1;
    }

    /**
     * A Constructor . It allows to create a named variable (but also it can create and an anonymous one if the text is '_') and set the first variable char position in the source string
     * 
     * @param text
     *            the name for the new variable, it can't be null and must use
     *            the prolog syntax variable naming rules
     * @param strPosition the first variable char string position
     * @param lineNumber the first variable char line number
     * @since 1.01
     */
    public PrologVariable(final String text, final int strPosition, final int lineNumber) {
        this(text);
        setStrPosition(strPosition);
        setLineNumber(lineNumber);
    }

    /**
     * Check that the variable is an anonymous one
     * 
     * @return true if the variable is an anonymous one, else false
     * @since 1.00
     */
    public boolean isAnonymous() {
        return is_anonymous;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PrologTermType getType() {
        return PrologTermType.VAR;
    }

    /**
     * Get the linked variable, mainly it is a variable with the same name meet by a parser in the sentence before. 
     * @return the linked variable a prolog variable or null
     * 
     * @since 1.01
     */
    public PrologVariable getLinkedVariable() {
        return linkedVariable;
    }

    /**
     * Set the linked variable.
     * @param variable the linked variable, it can be null
     * @throws UnsupportedOperationException will be thrown if the variable is an anonymous, it is impossible to set a linked variable to an anonymous one
     * @throws IllegalArgumentException will be thrown if the linked variable has a different name with the variable
     * 
     * @since 1.01
     */
    public void setLinkedVariable(final PrologVariable variable) {
        if (isAnonymous()) {
            throw new UnsupportedOperationException("It is impossible to set a linked variable for an anonimous one");
        }

        if (variable != null && !variable.getText().equals(getText())) {
            throw new IllegalArgumentException("Linked variable must have the same name");
        }
        linkedVariable = variable;
    }
}
