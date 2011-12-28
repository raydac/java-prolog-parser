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
package com.igormaznitsa.prologparser.operators;

import com.igormaznitsa.prologparser.annotations.PrologOperator;
import com.igormaznitsa.prologparser.annotations.PrologOperators;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologStructure;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import static com.igormaznitsa.prologparser.utils.AssertionUtils.*;

/**
 * The class describes a prolog operator for the prolog parser.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.02
 * 
 * @see PrologOperator
 * @see PrologOperators
 */
public final class Operator extends AbstractPrologTerm {
    /**
     * Describes the left bracket meta-operator ('(')
     * @since 1.02
     */
    public static final Operator METAOPERATOR_LEFT_BRACKET = makeMetaOperator(-1, OperatorType.FX, "("); 
    /**
     * Describes the right bracket meta-operator (')')
     * @since 1.02
     */
    public static final Operator METAOPERATOR_RIGHT_BRACKET = makeMetaOperator(-1, OperatorType.XF, ")"); 
    /**
     * Describes the left square bracket meta-operator ('[')
     * @since 1.02
     */
    public static final Operator METAOPERATOR_LEFT_SQUARE_BRACKET = makeMetaOperator(-1, OperatorType.FX, "["); 
    /**
     * Describes the right square bracket meta-operator (']')
     * @since 1.02
     */
    public static final Operator METAOPERATOR_RIGHT_SQUARE_BRACKET = makeMetaOperator(-1, OperatorType.XF, "]"); 
    /**
     * Describes the dot meta-operator ('.')
     * @since 1.02
     */
    public static final Operator METAOPERATOR_DOT = makeMetaOperator(Integer.MAX_VALUE, OperatorType.XF, "."); 
    /**
     * Describes the vertical bar meta-operator ('|')
     * @since 1.02
     */
    public static final Operator METAOPERATOR_VERTICAL_BAR = makeMetaOperator(1105, OperatorType.XFY, "|");
    
    /**
     * The constant describes the maximum priority for a prolog operator.
     * 
     * @since 1.00
     */
    public static final int PRIORITY_MAX = 0;
    /**
     * The constant describes the minimum priority for a prolog operator.
     * 
     * @since 1.00
     */
    public static final int PRIORITY_MIN = 1200;
    /**
     * The variable contains the operator type.
     */
    private final OperatorType opType;
    /**
     * The variable contains the operator priority value
     */
    private final int opPriority;
    /**
     * The variable contains the pre-calculated hash code for the operator
     */
    private final int precalculatedHashCode;

    /**
     * This auxiliary function allows to generate a lot of similar operators
     * from a string array
     * 
     * @param priority
     *            the priority for all generated operators 0..1200
     * @param type
     *            the type for all generated operators, must not be null
     * @param names
     *            a string array contains names for new operators, must not be
     *            null
     * @return an array of new Operator objects which were generated for the
     *         arguments and they have the same type and priority but different
     *         names.
     * @see OperatorType
     * @since 1.00
     */
    public static Operator[] makeOperators(final int priority,
            final OperatorType type, final String[] names) {
        if (priority < PRIORITY_MAX || priority > PRIORITY_MIN) {
            throw new IllegalArgumentException(
                    "Priority must be in the PRIORITY_MAX(0)..PRIORITY_MIN(1200)");
        }
        checkNotNull("Type is null", type);
        checkNotNull("Name array is null", names);

        final Operator[] result = new Operator[names.length];
        for (int li = 0; li < names.length; li++) {
            result[li] = makeOperator(priority, type, names[li]);
        }
        return result;
    }

    /**
     * This factory method allows to generate new operator with desired parameters, it will generate new instance every time because there is not any inside logic to cache instances(!).
     * @param priority the operator priority must be in the [1..1200] interval
     * @param type the operator type, must not be null
     * @param name the operator name, must not be null or empty
     * @return the new generated operator instance for arguments
     * @throws IllegalArgumentException if there is a wrong priority value
     * @since 1.02
     */
    public static Operator makeOperator(final int priority, final OperatorType type, final String name) {
        if (priority < PRIORITY_MAX || priority > PRIORITY_MIN) {
            throw new IllegalArgumentException("Wrong priority value");
        }

        return new Operator(priority, type, name);
    }   
    
    /**
     * This inside factory method is used to generate operators without check of their priority 
     * @param priority the operator priority, it can be any integer value
     * @param type the operator type, it must not be null
     * @param name the operator name, it must not be null or empty
     * @return the new generated operator instance
     * @since 1.02
     */
    static Operator makeMetaOperator(final int priority, final OperatorType type, final String name) {
        checkNotNull("Type is null", type);
        checkNotNull("Name array is null", name);
        return new Operator(priority, type, name);
    }
    
    /**
     * The constructor. It has been hidden since 1.02 version because we must avoid direct operator creation (!)
     * 
     * @param priority
     *            the operator priority 0..1200
     * @param type
     *            the operator type, must not be null
     * @param name
     *            the operator name, must not be null
     * @throws java.lang.IllegalArgumentException
     *             will be thrown if there is some incompatible value at
     *             arguments
     * @see OperatorType
     * @since 1.00
     * @see Operator#makeOperator(int, com.igormaznitsa.prologparser.operators.OperatorType, java.lang.String) 
     * @see Operator#makeOperators(int, com.igormaznitsa.prologparser.operators.OperatorType, java.lang.String[]) 
     */
    private Operator(final int priority, final OperatorType type,
            final String name) {
        super(name);

        checkNotNull("Type is null", type);

        if (name.isEmpty()) {
            throw new IllegalArgumentException("Empty operator name");
        }

        if (Character.isSpaceChar(name.charAt(0))) {
            throw new IllegalArgumentException(
                    "Space char as the first one at name");
        }

        if (Character.isUpperCase(name.charAt(0))) {
            throw new IllegalArgumentException(
                    "Capital char as the first one at name");
        }

        if (name.charAt(0) == '_') {
            throw new IllegalArgumentException("'_' as the first char");
        }

        opType = type;
        opPriority = priority;

        precalculatedHashCode = (name + "!" + this.opType + "!" + this.opPriority).hashCode();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PrologTermType getType() {
        return PrologTermType.OPERATOR;
    }

    /**
     * Get the type of the operator
     * 
     * @return the operator type
     * @since 1.00
     */
    public OperatorType getOperatorType() {
        return opType;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getPriority() {
        return opPriority;
    }

    /**
     * Check that the operator is compatible with a prolog structure and can be
     * the functor for the structure.
     * 
     * @param struct
     *            the structure to be checked, must not be null
     * @return true if the operator is compatible with the structure else false
     * @since 1.00
     */
    public boolean compatibleWith(final PrologStructure struct) {
        boolean result = false;
        if (struct != null) {

            switch (struct.getArity()) {
                case 1:
                    switch (opType) {
                        case XFY:
                        case XFX:
                        case YFX:
                            result = false;
                            break;
                        case XF:
                        case FX: {
                            final AbstractPrologTerm atom = struct.getElement(0);
                            if (atom == null) {
                                result = false;
                            } else {
                                result = atom.getPriority() < getPriority();
                            }
                        }
                        break;
                        case YF:
                        case FY: {
                            final AbstractPrologTerm atom = struct.getElement(0);
                            if (atom == null) {
                                result = false;
                            } else {
                                result = atom.getPriority() <= getPriority();
                            }
                        }
                        break;
                        default:
                            throw new Error("Unsupported type detected");
                    }
                    break;
                case 2:
                    switch (opType) {
                        case XFY:
                        case XFX:
                        case YFX:
                            final AbstractPrologTerm elementLeft = struct.getElement(0);
                            final AbstractPrologTerm elementRight = struct.getElement(1);

                            if (elementLeft == null || elementRight == null) {
                                result = false;
                            } else {

                                switch (opType) {
                                    case XFX:
                                        result = elementLeft.getPriority() < getPriority()
                                                && elementRight.getPriority() < getPriority();
                                        break;
                                    case YFX:
                                        result = elementLeft.getPriority() <= getPriority()
                                                && elementRight.getPriority() < getPriority();
                                        break;
                                    case XFY:
                                        result = elementLeft.getPriority() < getPriority()
                                                && elementRight.getPriority() <= getPriority();
                                        break;
                                    default:
                                        result = false;
                                        break;
                                }
                            }
                            break;

                        case XF:
                        case FX: {
                            final AbstractPrologTerm atom = struct.getElement(opType == OperatorType.XF ? 0 : 1);
                            if (atom == null) {
                                result = false;
                            } else {
                                result = atom.getPriority() < getPriority();
                            }
                        }
                        break;
                        case YF:
                        case FY: {
                            final AbstractPrologTerm atom = struct.getElement(opType == OperatorType.YF ? 0 : 1);
                            if (atom == null) {
                                result = false;
                            } else {
                                result = atom.getPriority() <= getPriority();
                            }

                        }
                        break;
                        default:
                            throw new Error("Unsupported type detected");

                    }

                    break;
                default:
                    result = false;
                    break;
            }
        }
        return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return precalculatedHashCode;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(final Object obj) {
        boolean result = false;

        if (obj != null) {
            if (this == obj) {
                result = true;
            } else {
                if (obj instanceof Operator) {
                    final Operator op = (Operator) obj;
                    if (op.precalculatedHashCode == precalculatedHashCode
                            && text.equals(op.text)
                            && opPriority == op.opPriority
                            && opType == op.opType) {
                        result = true;
                    }
                }
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return "op(" + getPriority() + ','
                + getOperatorType().toString().toLowerCase() + ",\'"
                + getText() + "\').";
    }

    /**
     * The method is overridden to disable change of the string position for operator because it is a singleton.
     * @param value the value doesn't make sense
     * @throws UnsupportedOperationException will be thrown in any case of call
     * @since 1.01 
     */
    @Override
    public void setStrPosition(final int value) {
        throw new UnsupportedOperationException("It is unsupported for operator");
    }

    /**
     * The method is overridden to disable change of the line number for operator because it is a singleton.
     * @param value the value doesn't make sense
     * @throws UnsupportedOperationException will be thrown in any case of call
     * @since 1.01 
     */
    @Override
    public void setLineNumber(final int value) {
        throw new UnsupportedOperationException("It is unsupported for operator");
    }
}
