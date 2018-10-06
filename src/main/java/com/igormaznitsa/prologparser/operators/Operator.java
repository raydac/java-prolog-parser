package com.igormaznitsa.prologparser.operators;

import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologStructure;
import com.igormaznitsa.prologparser.terms.PrologTermType;

import java.util.Locale;

public final class Operator extends AbstractPrologTerm {
  public static final Operator METAOPERATOR_LEFT_BRACKET = makeMeta(-1, OpType.FX, "(");
  public static final Operator METAOPERATOR_RIGHT_BRACKET = makeMeta(-1, OpType.XF, ")");
  public static final Operator METAOPERATOR_LEFT_SQUARE_BRACKET = makeMeta(-1, OpType.FX, "[");
  public static final Operator METAOPERATOR_RIGHT_SQUARE_BRACKET = makeMeta(-1, OpType.XF, "]");
  public static final Operator METAOPERATOR_DOT = makeMeta(Integer.MAX_VALUE, OpType.XF, ".");
  public static final Operator METAOPERATOR_VERTICAL_BAR = makeMeta(Integer.MAX_VALUE - 1, OpType.XFY, "|");
  public static final int PRECEDENCE_MAX = 0;
  public static final int PRECEDENCE_MIN = 1200;
  private static final long serialVersionUID = -5954313127778538548L;
  private final OpType opType;
  private final int precedence;
  private final int precalcHash;

  private Operator(final int precedence, final OpType type, final String name) {
    super(name);

    assertOpValidOpName(name);

    this.opType = type;
    this.precedence = precedence;

    this.precalcHash = (name + '/' + this.opType.name() + '/' + this.precedence).hashCode();
  }

  private static void assertOpValidOpName(final String name) {
    if (name.isEmpty()) {
      throw new IllegalArgumentException("Name must not be empty");
    }

    final char firstChar = name.charAt(0);

    if (Character.isWhitespace(firstChar) || Character.isISOControl(firstChar)) {
      throw new IllegalArgumentException("Space char as first one");
    }

    if (Character.isUpperCase(firstChar)) {
      throw new IllegalArgumentException("Capital char as first one");
    }

    if (firstChar == '_') {
      throw new IllegalArgumentException("'_' can't be firs char");
    }

  }

  public static Operator[] makeOps(final int precedence, final OpType type, final String[] names) {
    if (precedence < PRECEDENCE_MAX || precedence > PRECEDENCE_MIN) {
      throw new IllegalArgumentException(
          "Precedence must be in 0..1200");
    }

    if (type == null) {
      throw new NullPointerException("Type is null");
    }

    if (names == null) {
      throw new NullPointerException("Name array is null");
    }

    final Operator[] result = new Operator[names.length];
    for (int li = 0; li < names.length; li++) {
      result[li] = makeOp(precedence, type, names[li]);
    }
    return result;
  }

  public static Operator makeOp(final int precedence, final OpType type, final String name) {
    if (precedence < PRECEDENCE_MAX || precedence > PRECEDENCE_MIN) {
      throw new IllegalArgumentException("Wrong precedence value");
    }

    return new Operator(precedence, type, name);
  }

  private static Operator makeMeta(final int precedence, final OpType type, final String name) {
    return new Operator(precedence, type, name);
  }

  @Override
  public PrologTermType getType() {
    return PrologTermType.OPERATOR;
  }

  public OpType getOpType() {
    return this.opType;
  }

  @Override
  public int getPrecedence() {
    return this.precedence;
  }

  public boolean isCompatibleWith(final PrologStructure struct) {
    final boolean result;

    if (struct != null) {
      switch (struct.getArity()) {
        case 1: {
          switch (this.opType) {
            case XFY:
            case XFX:
            case YFX: {
              result = false;
            }
            break;
            case XF:
            case FX: {
              final AbstractPrologTerm atom = struct.getElement(0);
              result = atom != null && atom.getPrecedence() < getPrecedence();
            }
            break;
            case YF:
            case FY: {
              final AbstractPrologTerm atom = struct.getElement(0);
              result = atom != null && atom.getPrecedence() <= getPrecedence();
            }
            break;
            default: {
              throw new CriticalUnexpectedError();
            }
          }
        }
        break;
        case 2: {
          switch (this.opType) {
            case XFY:
            case XFX:
            case YFX: {
              final AbstractPrologTerm elementLeft = struct.getElement(0);
              final AbstractPrologTerm elementRight = struct.getElement(1);

              if (elementLeft == null || elementRight == null) {
                result = false;
              } else {

                switch (this.opType) {
                  case XFX: {
                    result = elementLeft.getPrecedence() < getPrecedence() && elementRight.getPrecedence() < getPrecedence();
                  }
                  break;
                  case YFX: {
                    result = elementLeft.getPrecedence() <= getPrecedence() && elementRight.getPrecedence() < getPrecedence();
                  }
                  break;
                  case XFY: {
                    result = elementLeft.getPrecedence() < getPrecedence() && elementRight.getPrecedence() <= getPrecedence();
                  }
                  break;
                  default: {
                    result = false;
                  }
                  break;
                }
              }
            }
            break;

            case XF:
            case FX: {
              final AbstractPrologTerm atom = struct.getElement(this.opType == OpType.XF ? 0 : 1);
              result = atom != null && atom.getPrecedence() < getPrecedence();
            }
            break;

            case YF:
            case FY: {
              final AbstractPrologTerm atom = struct.getElement(this.opType == OpType.YF ? 0 : 1);
              result = atom != null && atom.getPrecedence() <= getPrecedence();
            }
            break;

            default: {
              throw new CriticalUnexpectedError();
            }
          }
        }
        break;
        default: {
          result = false;
        }
        break;
      }
    } else {
      result = false;
    }
    return result;
  }

  @Override
  public int hashCode() {
    return this.precalcHash;
  }

  @Override
  public boolean equals(final Object obj) {
    boolean result = false;

    if (this == obj) {
      result = true;
    } else if (obj instanceof Operator) {
      final Operator op = (Operator) obj;
      if (this.precalcHash == op.precalcHash
          && this.precedence == op.precedence
          && this.opType == op.opType
          && this.text.equals(op.text)) {
        result = true;
      }
    }

    return result;
  }

  @Override
  public String toString() {
    return String.format("op(%d,%s,'%s').", getPrecedence(), getOpType().toString().toLowerCase(Locale.ENGLISH), getText());
  }

  private Object readResolve() {
    final Object result = GenericPrologParser.findSystemOperatorForNameAndType(this.text, this.opType);
    return result == null ? this : result;
  }
}
