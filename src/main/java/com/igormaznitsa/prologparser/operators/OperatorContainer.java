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

import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologTermType;

/**
 * The class being used by the prolog parser to save operators with the same
 * names but with different types.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.01
 */
public final class OperatorContainer extends AbstractPrologTerm {

    /**
     * The variable contains a FZ operator (fx,fy)
     */
    private Operator opFZ;
    /**
     * The variable contains a ZF operator (xf,yf)
     */
    private Operator opZF;
    /**
     * The variable contains a ZF operator (xfx,yfx,xfy)
     */
    private Operator opZFZ;
    /**
     * The counter of operators being saved by the container
     */
    private int numberAtContainer;

    /**
     * A constructor. Create a container based on an operator.
     * 
     * @param operator
     *            an operator as the ground for the container
     * @since 1.00
     */
    public OperatorContainer(final Operator operator) {
        super(operator.getText());
        addOperator(operator);
    }

    /**
     * Add an operator into the container
     * 
     * @param operator
     *            an operator to be added into the container, must not be null
     * @return true if the operator has been added, else false
     * @throws IllegalArgumentException
     *             if the operator has different name than the container
     * @since 1.00
     */
    public boolean addOperator(final Operator operator) {
        if (operator == null) {
            throw new NullPointerException("Operator must not be null");
        }

        if (!getText().equals(operator.getText())) {
            throw new IllegalArgumentException(
                    "Wrong operator name for the container");
        }

        switch (operator.getOperatorType()) {
            case FX:
            case FY:
                if (opFZ != null) {
                    return false;
                }
                opFZ = operator;
                numberAtContainer++;
                break;
            case XF:
            case YF:
                if (opZF != null) {
                    return false;
                }
                opZF = operator;
                numberAtContainer++;
                break;
            case XFX:
            case XFY:
            case YFX:
                if (opZFZ != null) {
                    return false;
                }
                opZFZ = operator;
                numberAtContainer++;
                break;
            default:
                throw new Error(
                        "Programming error detected, Unsupported operator type");
        }
        return true;
    }

    /**
     * Remove all operators from the container, but pay your attention that the
     * container will not lost its name and you will not be able to add
     * operators with other names.
     * 
     * @since 1.00
     */
    public void removeAll() {
        opFZ = null;
        opZF = null;
        opZFZ = null;
        numberAtContainer = 0;
    }

    /**
     * Remove an operator from the container
     * 
     * @param op
     *            the operator to be removed, must not be null
     * @return true if the operator has been found and removed from the
     *         container, else false
     * @since 1.00
     */
    public boolean remove(final Operator op) {
        if (op == null) {
            throw new NullPointerException("Operator is null");
        }

        if (!getText().equals(op.getText())) {
            throw new IllegalArgumentException(
                    "Wrong operator name for the container");
        }

        boolean result = false;
        if (opFZ != null && opFZ.equals(op)) {
            opFZ = null;
            numberAtContainer--;
            result = true;
        } else if (opZF != null && opZF.equals(op)) {
            opZF = null;
            numberAtContainer--;
            result = true;
        }
        if (opZFZ != null && opZFZ.equals(op)) {
            opZFZ = null;
            numberAtContainer--;
            result = true;
        }
        return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PrologTermType getType() {
        return PrologTermType.OPERATORS;
    }

    /**
     * Return the current number of saved operators within the container
     * 
     * @return the operator number as integer
     * @since 1.00
     */
    public int size() {
        return numberAtContainer;
    }

    /**
     * Get an operator from the container if the operator is the only saved
     * operator within the container.
     * 
     * @return an operator if it is only saved operator, else null
     * @since 1.00
     */
    public Operator getOperatorIfSingle() {
        if (numberAtContainer == 1) {
            if (opZFZ != null) {
                return opZFZ;
            }
            if (opFZ != null) {
                return opFZ;
            }
            return opZF;
        }

        return null;
    }

    /**
     * Get an operator from the container which can be used for a situation, as
     * situation I mean the left right arguments existence.
     * 
     * @param leftPresented
     *            true if there is the left argument in the situation, false if
     *            there is not any left one
     * @param rightPresented
     *            false if thee is the right argument in the situation, false if
     *            there is not any right one
     * @return the found operator or null if there is not anyone found
     * @since 1.00
     */
    public Operator findCompatibleOperator(final boolean leftPresented,
            final boolean rightPresented) {
        if (leftPresented && rightPresented) {
            if (opZFZ != null) {
                return opZFZ;
            }
            if (opFZ != null) {
                return opFZ;
            }
            return opZF;
        }
        if (leftPresented && !rightPresented) {
            if (opZF != null) {
                return opZF;
            }
            return opFZ;
        }
        if (!leftPresented && rightPresented) {
            if (opFZ != null) {
                return opFZ;
            }
            return opZF;
        }
        return null;
    }

    /**
     * Get an operator for its type.
     * 
     * @param type
     *            the operator type
     * @return the found operator or null
     * @since 1.00
     */
    public Operator getOperatorForType(final OperatorType type) {
        if (type == null) {
            throw new NullPointerException("The type is null");
        }
        Operator result = null;
        switch (type) {
            case FY:
            case FX:
                if (opFZ != null) {
                    result = opFZ;
                }
                break;
            case XF:
            case YF:
                if (opZF != null) {
                    result = opZF;
                }
                break;
            case XFX:
            case YFX:
            case XFY:
                if (opZFZ != null) {
                    result = opZFZ;
                }
                break;
            default:
                throw new Error("Unsupported operator type");
        }

        if (result != null && result.getOperatorType() == type) {
            return result;
        }
        return null;
    }

    /**
     * Find a similar type operator. As the similar type I mean the case of
     * argument position (ZF, FZ, ZFZ). If you find for XF and there is YF then
     * you will get YF.
     * 
     * @param type
     *            the type for search
     * @return the found operator or null
     * @since 1.00
     */
    public Operator getOperatorForSimilarType(final OperatorType type) {
        switch (type) {
            case FX:
            case FY:
                return opFZ;
            case XF:
            case YF:
                return opZF;
            case XFX:
            case YFX:
            case XFY:
                return opZFZ;
            default:
                throw new Error("Unsupported type");
        }
    }

    /**
     * Remove an operator for its type
     * 
     * @param type
     *            the operator type to be removed
     * @return true if the operator was found and removed, else false
     * @since 1.00
     */
    public boolean removeOperatorForType(final OperatorType type) {
        if (type == null) {
            throw new NullPointerException("Type is null");
        }
        boolean result = false;
        switch (type) {
            case FX:
            case FY:
                if (opFZ != null && opFZ.getOperatorType() == type) {
                    opFZ = null;
                    result = true;
                }
                break;
            case XF:
            case YF:
                if (opZF != null && opZF.getOperatorType() == type) {
                    opZF = null;
                    result = true;
                }
                break;
            case XFX:
            case YFX:
            case XFY:
                if (opZFZ != null && opZFZ.getOperatorType() == type) {
                    opZFZ = null;
                    result = true;
                }
                break;
            default:
                throw new Error("Unsupported type detected, it's a conceptual error!");
        }
        return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getPriority() {
        return 0;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        final StringBuilder result = new StringBuilder();
        result.append("OperatorContainer [");

        boolean added = false;
        final Operator[] ops = new Operator[]{opFZ, opZF, opZFZ};
        for (final Operator op : ops) {
            if (op != null) {
                if (added) {
                    result.append(' ');
                }
                result.append(op.toString());
                added = true;
            }
        }

        return result.append(']').toString();
    }

    /**
     * The method is overridden to disable change of the string position for operator container because it is a singleton.
     * @param value the value doesn't make sense
     * @throws UnsupportedOperationException will be thrown in any case of call
     * @since 1.01 
     */
    @Override
    public void setStrPosition(final int value) {
        throw new UnsupportedOperationException("It is unsupported for an operator container");
    }

    /**
     * The method is overridden to disable change of the line number for operator container because it is a singleton.
     * @param value the value doesn't make sense
     * @throws UnsupportedOperationException will be thrown in any case of call
     * @since 1.01 
     */
    @Override
    public void setLineNumber(final int value) {
        throw new UnsupportedOperationException("It is unsupported for an operator container");
    }
}
