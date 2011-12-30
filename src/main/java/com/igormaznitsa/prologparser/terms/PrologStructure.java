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

import com.igormaznitsa.prologparser.exceptions.CriticalSoftwareDefectError;
import java.util.Arrays;

import com.igormaznitsa.prologparser.operators.Operator;
import static com.igormaznitsa.prologparser.utils.AssertionUtils.*;

/**
 * The class describes a prolog structure.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.02
 */
@SuppressWarnings("serial")
public class PrologStructure extends AbstractPrologTerm {

    /**
     * An auxiliary constant contains an empty prolog term array
     * 
     * @since 1.00
     */
    public static final AbstractPrologTerm[] EMPTY_TERM_ARRAY = new AbstractPrologTerm[0];
    /**
     * An auxiliary constant contains an empty atom, as empty I mean that the
     * atom contains the empty string
     * 
     * @since 1.00
     */
    public static final PrologAtom EMPTY_ATOM = new PrologAtom("");
    /**
     * The functor of the structure
     * 
     * @since 1.00
     */
    protected final AbstractPrologTerm functor;
    /**
     * The array contains structure elements
     * 
     * @since 1.00
     */
    protected final AbstractPrologTerm[] elements;

    /**
     * A Constructor. It allows to create a structure for a functor and a term
     * array
     * 
     * @param functor
     *            the functor for the new structure, must not be null
     * @param elements
     *            the elements of the new structure, must not be null and must
     *            not contain null (!)
     * @since 1.00
     */
    public PrologStructure(final AbstractPrologTerm functor,
            final AbstractPrologTerm[] elements) {
        super(functor.getText());

        if (functor.getType() != PrologTermType.ATOM
                && functor.getType() != PrologTermType.OPERATOR) {
            throw new IllegalArgumentException(
                    "Functor must be either atom or operator");
        }
        if (functor instanceof AbstractPrologNumericTerm) {
            throw new IllegalArgumentException("Number can't be a functor");
        }

        checkNotNull("Element array is null", elements);

        this.functor = functor;
        this.elements = elements.clone();

        checkArrayForNullElements("There is a null element in the array, use EMPTY_ATOM instead of null", this.elements);
    }

    /**
     * A Constructor. It allows to create a structure for a functor and a term
     * array and set the first term char position in the source stream
     * 
     * @param functor
     *            the functor for the new structure, must not be null
     * @param elements
     *            the elements of the new structure, must not be null and must
     *            not contain null (!)
     * @param strPosition
     * @param lineNumber
     * @since 1.02
     */
    public PrologStructure(final AbstractPrologTerm functor, final AbstractPrologTerm[] elements, final int strPosition, final int lineNumber) {
        this(functor, elements);
        setStrPosition(strPosition);
        setLineNumber(lineNumber);
    }

    /**
     * A Constructor. It allows to create a zero (I mean a zero arity one)
     * structure with a prolog atom as the functor.
     * 
     * @param text
     *            the text to create the functor, must not be null
     * @since 1.00
     */
    public PrologStructure(final String text) {
        this(new PrologAtom(text), 0);
    }

    /**
     * A Constructor. It allows to create a zero arity structure and set the first term char position in the source stream 
     * @param text the text to create the functor, must not be null
     * @param strPosition the first term string position
     * @param lineNumber the first term char line number 
     * @since 1.02
     */
    public PrologStructure(final String text, final int strPosition, final int lineNumber) {
        this(text);
        setStrPosition(strPosition);
        setLineNumber(lineNumber);
    }

    /**
     * A Constructor. It allows to create a zero (I mean a zero arity one)
     * structure with a prolog term as a functor
     * 
     * @param functor
     *            a prolog term to be used as the functor, must not be null
     * @since 1.00
     */
    public PrologStructure(final AbstractPrologTerm functor) {
        this(functor, 0);
    }

    /**
     * A Constructor. It allows to create a zero (I mean a zero arity one)
     * structure with a prolog term as a functor
     * 
     * @param functor
     *            a prolog term to be used as the functor, must not be null
     * @param strPosition the first term char string position in the source stream
     * @param lineNumber the first term char line number in the source stream
     * @since 1.02
     */
    public PrologStructure(final AbstractPrologTerm functor, final int strPosition, final int lineNumber) {
        this(functor);
        setStrPosition(strPosition);
        setLineNumber(lineNumber);
    }

    /**
     * A Constructor. It allows to create a prolog structure for a functor and
     * needed arity (it will use EMPTY_ATOM as each element)
     * 
     * @param functor
     *            a prolog term to be used as the structure functor, it must not
     *            be null.
     * @param arity
     *            the arity of the new structure, must not be less than zero.
     * @since 1.00
     */
    protected PrologStructure(final AbstractPrologTerm functor, final int arity) {
        super(functor.getText());

        if (functor.getType() != PrologTermType.ATOM
                && functor.getType() != PrologTermType.OPERATOR
                && functor.getType() != PrologTermType.OPERATORS) {
            throw new IllegalArgumentException(
                    "Wrong functor type, must be either atom or operator(s)");
        }

        if (functor instanceof AbstractPrologNumericTerm) {
            throw new IllegalArgumentException(
                    "Functor must not be a numeric term");
        }

        if (arity < 0) {
            throw new IllegalArgumentException("Negative arity");
        }

        this.functor = functor;
        elements = new AbstractPrologTerm[arity];
        Arrays.fill(elements, EMPTY_ATOM);
    }

    /**
     * A Constructor. It allows to create a prolog structure for a functor and
     * needed arity (it will use EMPTY_ATOM as each element) and set the source stream position
     * 
     * @param functor
     *            a prolog term to be used as the structure functor, it must not
     *            be null.
     * @param arity
     *            the arity of the new structure, must not be less than zero.
     * @param strPosition the first term char string position
     * @param lineNumber the first term char line number
     * @since 1.02
     */
    protected PrologStructure(final AbstractPrologTerm functor, final int arity, final int strPosition, final int lineNumber) {
        this(functor, arity);
        setStrPosition(strPosition);
        setLineNumber(lineNumber);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PrologTermType getType() {
        return PrologTermType.STRUCT;
    }

    /**
     * Get the arity of the structure
     * 
     * @return the arity as integer
     * @since 1.00
     */
    public int getArity() {
        return elements.length;
    }

    /**
     * Get a structure element for an index
     * 
     * @param index
     *            the index of the element, the first element is 0
     * @return the structure element at the needed position, it can't be null
     *         (!)
     * @since 1.00
     */
    public AbstractPrologTerm getElement(final int index) {
        return elements[index];
    }

    /**
     * Set a structure element at a position by a prolog term
     * 
     * @param index
     *            the position of the element, the first is 0
     * @param term
     *            the term to be set into the position, must not be null
     * @since 1.00
     */
    public void setElement(final int index, final AbstractPrologTerm term) {
        if (index < 0 || index >= getArity()) {
            throw new ArrayIndexOutOfBoundsException();
        }
        checkNotNull("Attempt to set null as a structure element", term);
        elements[index] = term;
    }

    /**
     * Get the functor of the structure
     * 
     * @return the functor
     * @since 1.00
     */
    public AbstractPrologTerm getFunctor() {
        return functor;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getPriority() {
        if (functor.getType() == PrologTermType.OPERATOR) {
            return functor.getPriority();
        } else {
            return 0;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        final StringBuilder builder = new StringBuilder();
        if (functor.getType() == PrologTermType.OPERATOR) {
            // an operator based struct
            final Operator operatorFunctor = (Operator) functor;
            final String opName = operatorFunctor.getText();
            final int priority = operatorFunctor.getPriority();

            final String text = getElement(0).toString();
            final String text2 = getArity() > 1 ? getElement(1).toString()
                    : null;

            switch (operatorFunctor.getOperatorType()) {
                case FX: {
                    builder.append(opName).append(' ');

                    if (getElement(0).getPriority() >= priority) {
                        builder.append('(').append(text).append(')');
                    } else {
                        builder.append(text);
                    }
                }
                break;
                case FY: {
                    builder.append(opName);
                    builder.append(' ');

                    if (getElement(0).getPriority() > priority) {
                        builder.append('(').append(text).append(')');
                    } else {
                        builder.append(text);
                    }
                }
                break;
                case XF: {
                    if (getElement(0).getPriority() >= priority) {
                        builder.append('(').append(text).append(')');
                    } else {
                        builder.append(text);
                    }

                    builder.append(' ').append(opName);
                }
                break;
                case YF: {
                    if (getElement(0).getPriority() > priority) {
                        builder.append('(').append(text).append(')');
                    } else {
                        builder.append(text);
                    }

                    builder.append(' ').append(opName);
                }
                break;
                case XFX: {
                    if (getElement(0).getPriority() >= priority) {
                        builder.append('(').append(text).append(')');
                    } else {
                        builder.append(text);
                    }

                    builder.append(' ').append(opName).append(' ');

                    if (getElement(1).getPriority() >= priority) {
                        builder.append('(').append(text2).append(')');
                    } else {
                        builder.append(text2);
                    }
                }
                break;
                case YFX: {
                    if (getElement(0).getPriority() > priority) {
                        builder.append('(').append(text).append(')');
                    } else {
                        builder.append(text);
                    }

                    builder.append(' ').append(opName).append(' ');

                    if (getElement(1).getPriority() >= priority) {
                        builder.append('(').append(text2).append(')');
                    } else {
                        builder.append(text2);
                    }
                }
                break;
                case XFY: {
                    if (getElement(0).getPriority() >= priority) {
                        builder.append('(').append(text).append(')');
                    } else {
                        builder.append(text);
                    }

                    builder.append(' ').append(opName).append(' ');

                    if (getElement(1).getPriority() > priority) {
                        builder.append('(').append(text2).append(')');
                    } else {
                        builder.append(text2);
                    }
                }
                break;
                default:
                    throw new CriticalSoftwareDefectError();
            }

        } else {
            String functorText = functor.getText();

            if ("!".equals(functorText) && getArity() == 0) {
                // special structure detected
                return functorText;
            }

            // just structure
            functorText = functor.toString();
            builder.append(functorText);
            builder.append('(');
            boolean next = false;
            for (final AbstractPrologTerm term : elements) {
                if (next) {
                    builder.append(", ");
                } else {
                    next = true;
                }
                builder.append(term);
            }
            builder.append(')');

        }
        return builder.toString();
    }
}
