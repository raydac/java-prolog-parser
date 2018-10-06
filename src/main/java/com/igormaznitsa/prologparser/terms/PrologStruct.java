package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.operators.Op;
import com.igormaznitsa.prologparser.utils.AssertUtils;
import com.igormaznitsa.prologparser.utils.StringBuilderEx;

import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import static com.igormaznitsa.prologparser.utils.AssertUtils.assertNotNull;

public class PrologStruct extends PrologTerm implements Iterable<PrologTerm> {

  public static final PrologAtom EMPTY_ATOM = new PrologAtom("");
  private static final long serialVersionUID = 9000641998734217154L;

  protected final PrologTerm functor;
  protected final PrologTerm[] elements;

  public PrologStruct(final PrologTerm functor, final PrologTerm[] elements) {
    super(functor.getText());

    if (functor.getType() != TermType.ATOM && functor.getType() != TermType.OPERATOR) {
      throw new IllegalArgumentException("Functor must be either atom or operator");
    }
    if (functor instanceof PrologNumeric) {
      throw new IllegalArgumentException("Functor can't be number");
    }

    this.elements = assertNotNull(elements.clone());
    this.functor = functor;
  }

  public PrologStruct(final PrologTerm functor, final PrologTerm[] elements, final int line, final int pos) {
    this(functor, elements);
    setPos(pos);
    setLine(line);
  }

  public PrologStruct(final String text) {
    this(new PrologAtom(text), 0);
  }

  public PrologStruct(final String text, final int line, final int pos) {
    this(text);
    setPos(pos);
    setLine(line);
  }

  public PrologStruct(final PrologTerm functor) {
    this(functor, 0);
  }

  public PrologStruct(final PrologTerm functor, final int line, final int pos) {
    this(functor);
    setPos(pos);
    setLine(line);
  }

  protected PrologStruct(final PrologTerm functor, final int arity) {
    super(functor.getText());

    if (functor.getType() != TermType.ATOM
        && functor.getType() != TermType.OPERATOR
        && functor.getType() != TermType.OPERATORS) {
      throw new IllegalArgumentException("Functor type must be either atom or operator");
    }

    if (functor instanceof PrologNumeric) {
      throw new IllegalArgumentException("Numeric term as functor");
    }

    if (arity < 0) {
      throw new IllegalArgumentException("Negative arity");
    }

    this.functor = functor;
    this.elements = new PrologTerm[arity];
    Arrays.fill(elements, EMPTY_ATOM);
  }

  protected PrologStruct(final PrologTerm functor, final int arity, final int line, final int pos) {
    this(functor, arity);
    setPos(pos);
    setLine(line);
  }

  @Override
  public TermType getType() {
    return TermType.STRUCT;
  }

  public int getArity() {
    return elements.length;
  }

  public PrologTerm getElement(final int index) {
    return elements[index];
  }

  public void setElement(final int index, final PrologTerm term) {
    if (index < 0 || index >= getArity()) {
      throw new ArrayIndexOutOfBoundsException();
    }
    elements[index] = AssertUtils.assertNotNull(term);
  }

  public PrologTerm getFunctor() {
    return functor;
  }

  @Override
  public int getPrecedence() {
    if (functor.getType() == TermType.OPERATOR) {
      return functor.getPrecedence();
    } else {
      return 0;
    }
  }

  public PrologStruct copyWithAnotherFunctor(final PrologTerm newFunctor) {
    return new PrologStruct(newFunctor, elements);
  }

  @Override
  public String toString() {
    final StringBuilderEx builder = new StringBuilderEx(64);

    if (functor.getType() == TermType.OPERATOR) {

      final Op operatorFunctor = (Op) functor;
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
      for (final PrologTerm term : elements) {
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


  @Override
  public Stream<PrologTerm> stream() {
    return StreamSupport.stream(
        Spliterators.spliterator(
            this.elements,
            Spliterator.ORDERED | Spliterator.NONNULL
        ), false);
  }

  @Override
  public Iterator<PrologTerm> iterator() {
    return new Iterator<PrologTerm>() {
      PrologTerm[] terms = elements.clone();
      int index = 0;

      @Override
      public boolean hasNext() {
        return index < this.terms.length;
      }

      @Override
      public PrologTerm next() {
        if (this.index < this.terms.length) {
          return this.terms[this.index++];
        } else {
          throw new NoSuchElementException();
        }
      }
    };
  }
}
