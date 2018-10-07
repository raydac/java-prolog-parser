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

package com.igormaznitsa.prologparser.operators;

import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.utils.StringBuilderEx;

public final class OpContainer extends PrologTerm {

  private static final long serialVersionUID = 4946799717661204529L;

  private Op opFZ;
  private Op opZF;
  private Op opZFZ;
  private int numberAtContainer;

  private OpContainer(final Op operator) {
    super(operator.getText());
    addOp(operator);
  }

  public static OpContainer newOpCont(final Op operator) {
    return new OpContainer(operator);
  }

  public boolean addOp(final Op operator) {
    if (!getText().equals(operator.getText())) {
      throw new IllegalArgumentException("Illegal operator name, must be '" + getText() + "'");
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

  public Op findForArity(final int arity) {
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

  public boolean remove(final Op op) {
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

  @Override
  public TermType getType() {
    return TermType.OPERATORS;
  }

  public int size() {
    return numberAtContainer;
  }

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
