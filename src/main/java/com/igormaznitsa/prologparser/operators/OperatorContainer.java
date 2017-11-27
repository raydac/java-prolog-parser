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

import com.igormaznitsa.prologparser.exceptions.CriticalSoftwareDefectError;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import com.igormaznitsa.prologparser.utils.FastStringBuilder;

import static com.igormaznitsa.prologparser.utils.Assert.assertNotNull;

/**
 * The class being used by the prolog parser to save operators with the same
 * names but with different types.
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 */
public final class OperatorContainer extends AbstractPrologTerm {

    private static final long serialVersionUID = 4946799717661204529L;
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
     * @param operator an operator as the ground for the container
     */
    public OperatorContainer(final Operator operator) {
        super(operator.getText());
        addOperator(operator);
    }

    /**
     * Add an operator into the container
     *
     * @param operator an operator to be added into the container, must not be
     * null
     * @return true if the operator has been added, else false
     * @throws IllegalArgumentException if the operator has different name than
     * the container
     */
    public boolean addOperator(final Operator operator) {
        assertNotNull("Operator must not be null", operator);

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
                throw new CriticalSoftwareDefectError();
        }
        return true;
    }

    /**
     * Remove all operators from the container, but pay your attention that the
     * container will not lost its name and you will not be able to add
     * operators with other names.
     */
    public void removeAll() {
        opFZ = null;
        opZF = null;
        opZFZ = null;
        numberAtContainer = 0;
    }

    /**
     * Get an operator for its arity.
     *
     * @param arity the arity.
     * @return the found operator or null otherwise.
     */
    public Operator findForArity(final int arity) {
        Operator result;
        switch (arity) {
            case 1: {
                if (opFZ != null) {
                    result = opFZ;
                } else {
                    result = opZF;
                }
            }
            break;
            case 2: {
                result = opZFZ;
            }
            break;
            default:
                result = null;
                break;
        }
        return result;
    }

    /**
     * Remove an operator from the container
     *
     * @param op the operator to be removed, must not be null
     * @return true if the operator has been found and removed from the
     * container, else false
     */
    public boolean remove(final Operator op) {
        assertNotNull("Operator is null", op);

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
     */
    public int size() {
        return numberAtContainer;
    }

    /**
     * Get an operator from the container if the operator is the only saved
     * operator within the container.
     *
     * @return an operator if it is only saved operator, else null
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
     * @param leftPresented true if there is the left argument in the situation,
     * false if there is not any left one
     * @param rightPresented false if thee is the right argument in the
     * situation, false if there is not any right one
     * @return the found operator or null if there is not anyone found
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
     * @param type the operator type
     * @return the found operator or null
     */
    public Operator getOperatorForType(final OperatorType type) {
        assertNotNull("Operator type is null", type);
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
                throw new CriticalSoftwareDefectError();
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
     * @param type the type for search
     * @return the found operator or null
     */
    public Operator getOperatorForSimilarType(final OperatorType type) {
        Operator result;
        switch (type) {
            case FX:
            case FY:
                result = opFZ;
                break;
            case XF:
            case YF:
                result = opZF;
                break;
            case XFX:
            case YFX:
            case XFY:
                result = opZFZ;
                break;
            default:
                throw new CriticalSoftwareDefectError();
        }
        return result;
    }

    /**
     * Remove an operator for its type
     *
     * @param type the operator type to be removed
     * @return true if the operator was found and removed, else false
     */
    public boolean removeOperatorForType(final OperatorType type) {
        assertNotNull("Operator type is null", type);
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
                throw new CriticalSoftwareDefectError();
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
        final FastStringBuilder result = new FastStringBuilder("OperatorContainer [");

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
}
