package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.utils.StrBuffer;

import java.util.Arrays;

public class PrologStructure extends AbstractPrologTerm {

  public static final AbstractPrologTerm[] EMPTY_TERM_ARRAY = new AbstractPrologTerm[0];
  public static final PrologAtom EMPTY_ATOM = new PrologAtom("");
  private static final long serialVersionUID = 9000641998734217154L;
  protected final AbstractPrologTerm functor;
  protected final AbstractPrologTerm[] elements;

  public PrologStructure(final AbstractPrologTerm functor,
                         final AbstractPrologTerm[] elements) {
    super(functor.getText());

    if (functor.getType() != PrologTermType.ATOM
        && functor.getType() != PrologTermType.OPERATOR) {
      throw new IllegalArgumentException(
          "Functor must be either an atom or an operator");
    }
    if (functor instanceof AbstractPrologNumericTerm) {
      throw new IllegalArgumentException("Number can't be a functor");
    }

    if (elements == null) {
      throw new NullPointerException("Elements must not be null");
    }

    this.functor = functor;
    this.elements = elements.clone();
  }

  public PrologStructure(final AbstractPrologTerm functor, final AbstractPrologTerm[] elements, final int strPosition, final int lineNumber) {
    this(functor, elements);
    setStrPosition(strPosition);
    setLineNumber(lineNumber);
  }

  public PrologStructure(final String text) {
    this(new PrologAtom(text), 0);
  }

  public PrologStructure(final String text, final int strPosition, final int lineNumber) {
    this(text);
    setStrPosition(strPosition);
    setLineNumber(lineNumber);
  }

  public PrologStructure(final AbstractPrologTerm functor) {
    this(functor, 0);
  }

  public PrologStructure(final AbstractPrologTerm functor, final int strPosition, final int lineNumber) {
    this(functor);
    setStrPosition(strPosition);
    setLineNumber(lineNumber);
  }

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

  protected PrologStructure(final AbstractPrologTerm functor, final int arity, final int strPosition, final int lineNumber) {
    this(functor, arity);
    setStrPosition(strPosition);
    setLineNumber(lineNumber);
  }

  @Override
  public PrologTermType getType() {
    return PrologTermType.STRUCT;
  }

  public int getArity() {
    return elements.length;
  }

  public AbstractPrologTerm getElement(final int index) {
    return elements[index];
  }

  public void setElement(final int index, final AbstractPrologTerm term) {
    if (index < 0 || index >= getArity()) {
      throw new ArrayIndexOutOfBoundsException();
    }
    if (term == null) {
      throw new NullPointerException("Term must not be null");
    }
    elements[index] = term;
  }

  public AbstractPrologTerm getFunctor() {
    return functor;
  }

  @Override
  public int getPrecedence() {
    if (functor.getType() == PrologTermType.OPERATOR) {
      return functor.getPrecedence();
    } else {
      return 0;
    }
  }

  public PrologStructure copyWithAnotherFunctor(final AbstractPrologTerm newFunctor) {
    return new PrologStructure(newFunctor, elements);
  }

  @Override
  public String toString() {
    final StrBuffer builder = new StrBuffer(64);
    if (functor.getType() == PrologTermType.OPERATOR) {
      // an operator based struct
      final Operator operatorFunctor = (Operator) functor;
      final String opName = operatorFunctor.getText();
      final int priority = operatorFunctor.getPrecedence();

      final String text1 = getElement(0).toString();
      final String text2 = getArity() > 1 ? getElement(1).toString()
          : null;

      switch (operatorFunctor.getOpType()) {
        case FX: {
          builder.append(opName).append(' ');

          if (getElement(0).getPrecedence() >= priority) {
            builder.append('(').append(text1).append(')');
          } else {
            builder.append(text1);
          }
        }
        break;
        case FY: {
          builder.append(opName);
          builder.append(' ');

          if (getElement(0).getPrecedence() > priority) {
            builder.append('(').append(text1).append(')');
          } else {
            builder.append(text1);
          }
        }
        break;
        case XF: {
          if (getElement(0).getPrecedence() >= priority) {
            builder.append('(').append(text1).append(')');
          } else {
            builder.append(text1);
          }

          builder.append(' ').append(opName);
        }
        break;
        case YF: {
          if (getElement(0).getPrecedence() > priority) {
            builder.append('(').append(text1).append(')');
          } else {
            builder.append(text1);
          }

          builder.append(' ').append(opName);
        }
        break;
        case XFX: {
          if (getElement(0).getPrecedence() >= priority) {
            builder.append('(').append(text1).append(')');
          } else {
            builder.append(text1);
          }

          builder.append(' ').append(opName).append(' ');

          if (getElement(1).getPrecedence() >= priority) {
            builder.append('(').append(text2).append(')');
          } else {
            builder.append(text2);
          }
        }
        break;
        case YFX: {
          if (getElement(0).getPrecedence() > priority) {
            builder.append('(').append(text1).append(')');
          } else {
            builder.append(text1);
          }

          builder.append(' ').append(opName).append(' ');

          if (getElement(1).getPrecedence() >= priority) {
            builder.append('(').append(text2).append(')');
          } else {
            builder.append(text2);
          }
        }
        break;
        case XFY: {
          if (getElement(0).getPrecedence() >= priority) {
            builder.append('(').append(text1).append(')');
          } else {
            builder.append(text1);
          }

          builder.append(' ').append(opName).append(' ');

          if (getElement(1).getPrecedence() > priority) {
            builder.append('(').append(text2).append(')');
          } else {
            builder.append(text2);
          }
        }
        break;
        default:
          throw new CriticalUnexpectedError();
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
        builder.append(term.toString());
      }
      builder.append(')');

    }
    return builder.toString();
  }
}
