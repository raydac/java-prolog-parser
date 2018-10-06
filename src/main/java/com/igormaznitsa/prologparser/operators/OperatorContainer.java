package com.igormaznitsa.prologparser.operators;

import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import com.igormaznitsa.prologparser.utils.StrBuffer;

public final class OperatorContainer extends AbstractPrologTerm {

  private static final long serialVersionUID = 4946799717661204529L;

  private Operator opFZ;
  private Operator opZF;
  private Operator opZFZ;
  private int numberAtContainer;

  private OperatorContainer(final Operator operator) {
    super(operator.getText());
    addOp(operator);
  }

  public static OperatorContainer newOpCont(final Operator operator) {
    return new OperatorContainer(operator);
  }

  public boolean addOp(final Operator operator) {
    if (!getText().equals(operator.getText())) {
      throw new IllegalArgumentException(
          "Wrong operator name for the container");
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

  public boolean remove(final Operator op) {
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
  public PrologTermType getType() {
    return PrologTermType.OPERATORS;
  }

  public int size() {
    return numberAtContainer;
  }

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

  public Operator findSimilar(final boolean hasLeftArg, final boolean hasRightArg) {
    final Operator result;
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

  public Operator findForType(final OpType type) {
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
        throw new CriticalUnexpectedError();
    }

    if (result != null && result.getOpType() == type) {
      return result;
    }
    return null;
  }

  public Operator findSimilar(final OpType type) {
    Operator result;
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
    final StrBuffer result = new StrBuffer("OpContainer [");

    boolean added = false;
    final Operator[] ops = new Operator[] {opFZ, opZF, opZFZ};
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
