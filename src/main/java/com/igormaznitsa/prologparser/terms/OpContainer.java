/*
 * Copyright (c) 2011-2018 Igor Maznitsa. All rights reserved.
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.tokenizer.Op;
import com.igormaznitsa.prologparser.tokenizer.OpType;
import com.igormaznitsa.prologparser.utils.StringBuilderEx;

/**
 * Container of operators with the same name.
 */
public final class OpContainer extends SpecServiceCompound {

  private static final long serialVersionUID = 4946799717661204529L;

  private Op opFZ;
  private Op opZF;
  private Op opZFZ;
  private int numberAtContainer;

  private OpContainer(final Op operator) {
    super(operator.getTermText());
    operator.streamOp().forEach(this::add);
  }

  /**
   * Create new container based on operator name.
   *
   * @param operator operator which name will be used and which will be added to the new container
   * @return generated container with the operator
   */
  public static OpContainer make(final Op operator) {
    return new OpContainer(operator);
  }

  /**
   * Add operator into the container.
   *
   * @param operator the operator to be added
   * @return true if the operator has been added, false if it was impossible because similar operator already presented
   * @throws IllegalArgumentException if operator has different name
   */
  public boolean add(final Op operator) {
    if (!getTermText().equals(operator.getTermText())) {
      throw new IllegalArgumentException("Illegal operator name, must be '" + getTermText() + "'");
    }

    switch (operator.getOpType()) {
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
        throw new CriticalUnexpectedError();
    }
    return true;
  }

  public void removeAll() {
    opFZ = null;
    opZF = null;
    opZFZ = null;
    numberAtContainer = 0;
  }

  /**
   * Find operator with arity in the container
   *
   * @param arity arity to be used (1..2)
   * @return found operator or null
   */
  public Op findArity(final int arity) {
    Op result;
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
   * Remove operator from the container
   *
   * @param op operator to be removed
   * @return true if operator was removed, false otherwise
   * @throws IllegalArgumentException if operator name is not compatible with the container
   */
  public boolean remove(final Op op) {
    if (!getTermText().equals(op.getTermText())) {
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

  @Override
  public TermType getTermType() {
    return TermType.__OPERATOR_CONTAINER__;
  }

  @Override
  public int getArity() {
    return 2;
  }

  @Override
  public PrologTerm getElementAt(int position) {
    throw new UnsupportedOperationException("Can't get element from operator container");
  }

  /**
   * Get current size of the container.
   *
   * @return size of the container
   */
  public int size() {
    return this.numberAtContainer;
  }

  /**
   * Get operator if it is only one in the container
   *
   * @return the found only operator, null if there are severe operators
   */
  public Op getOperatorIfSingle() {
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
   * Find operator for presented arguments.
   *
   * @param hasLeftArg  should have left argument
   * @param hasRightArg should have ritgh argument
   * @return operator for needed condition, null if not found
   */
  public Op findSimilar(final boolean hasLeftArg, final boolean hasRightArg) {
    final Op result;
    if (hasLeftArg) {
      if (hasRightArg) {
        if (opZFZ != null) {
          result = opZFZ;
        } else if (opFZ != null) {
          result = opFZ;
        } else {
          result = opZF;
        }
      } else {
        if (opZF != null) {
          result = opZF;
        } else {
          result = opFZ;
        }
      }
    } else {
      if (hasRightArg) {
        if (opFZ != null) {
          result = opFZ;
        } else {
          result = opZF;
        }
      } else {
        result = null;
      }
    }
    return result;
  }

  /**
   * Find operator for its type
   *
   * @param type type of needed operator
   * @return found operator, null if not found
   */
  public Op findForType(final OpType type) {
    Op result = null;
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
        throw new CriticalUnexpectedError();
    }

    if (result != null && result.getOpType() == type) {
      return result;
    }
    return null;
  }

  /**
   * Find operator with similar type. For instance FX is similar to FY and XF is similar to YF.
   *
   * @param type type of operator to find
   * @return operator with similar type or null if not found
   */
  public Op findSimilar(final OpType type) {
    Op result;
    switch (type) {
      case FX:
      case FY:
        result = this.opFZ;
        break;
      case XF:
      case YF:
        result = this.opZF;
        break;
      case XFX:
      case YFX:
      case XFY:
        result = this.opZFZ;
        break;
      default:
        throw new CriticalUnexpectedError();
    }
    return result;
  }

  /**
   * Remove operator for type
   *
   * @param type type of removing operator
   * @return true if removed, false if not found
   */
  public boolean removeForType(final OpType type) {
    boolean result = false;
    switch (type) {
      case FX:
      case FY:
        if (opFZ != null && opFZ.getOpType() == type) {
          opFZ = null;
          result = true;
        }
        break;
      case XF:
      case YF:
        if (opZF != null && opZF.getOpType() == type) {
          opZF = null;
          result = true;
        }
        break;
      case XFX:
      case YFX:
      case XFY:
        if (opZFZ != null && opZFZ.getOpType() == type) {
          opZFZ = null;
          result = true;
        }
        break;
      default:
        throw new CriticalUnexpectedError();
    }
    return result;
  }

  @Override
  public int getPrecedence() {
    return 0;
  }

  @Override
  public String toString() {
    final StringBuilderEx result = new StringBuilderEx("OpContainer [");

    boolean added = false;
    final Op[] ops = new Op[] {opFZ, opZF, opZFZ};
    for (final Op op : ops) {
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