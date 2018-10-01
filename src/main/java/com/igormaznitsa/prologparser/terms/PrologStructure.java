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

package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.utils.StrBuffer;

import java.util.Arrays;

/**
 * The class describes a prolog structure.
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 */
public class PrologStructure extends AbstractPrologTerm {

  /**
   * An auxiliary constant contains an empty prolog term array
   */
  public static final AbstractPrologTerm[] EMPTY_TERM_ARRAY = new AbstractPrologTerm[0];
  /**
   * An auxiliary constant contains an empty atom, as empty I mean that the
   * atom contains the empty string
   */
  public static final PrologAtom EMPTY_ATOM = new PrologAtom("");
  private static final long serialVersionUID = 9000641998734217154L;
  /**
   * The functor of the structure
   */
  protected final AbstractPrologTerm functor;
  /**
   * The array contains structure elements
   */
  protected final AbstractPrologTerm[] elements;

  /**
   * A Constructor. It allows to create a structure for a functor and a term
   * array
   *
   * @param functor  the functor for the new structure, must not be null
   * @param elements the elements of the new structure, must not be null and
   *                 must not contain null (!)
   */
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

  /**
   * A Constructor. It allows to create a structure for a functor and a term
   * array and set the first term char position in the source stream
   *
   * @param functor     the functor for the new structure, must not be null
   * @param elements    the elements of the new structure, must not be null and
   *                    must not contain null (!)
   * @param strPosition string position
   * @param lineNumber  line number
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
   * @param text the text to create the functor, must not be null
   */
  public PrologStructure(final String text) {
    this(new PrologAtom(text), 0);
  }

  /**
   * A Constructor. It allows to create a zero arity structure and set the
   * first term char position in the source stream
   *
   * @param text        the text to create the functor, must not be null
   * @param strPosition the first term string position
   * @param lineNumber  the first term char line number
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
   * @param functor a prolog term to be used as the functor, must not be null
   */
  public PrologStructure(final AbstractPrologTerm functor) {
    this(functor, 0);
  }

  /**
   * A Constructor. It allows to create a zero (I mean a zero arity one)
   * structure with a prolog term as a functor
   *
   * @param functor     a prolog term to be used as the functor, must not be null
   * @param strPosition the first term char string position in the source
   *                    stream
   * @param lineNumber  the first term char line number in the source stream
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
   * @param functor a prolog term to be used as the structure functor, it must
   *                not be null.
   * @param arity   the arity of the new structure, must not be less than zero.
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
   * needed arity (it will use EMPTY_ATOM as each element) and set the source
   * stream position
   *
   * @param functor     a prolog term to be used as the structure functor, it must
   *                    not be null.
   * @param arity       the arity of the new structure, must not be less than zero.
   * @param strPosition the first term char string position
   * @param lineNumber  the first term char line number
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
   */
  public int getArity() {
    return elements.length;
  }

  /**
   * Get a structure element for an index
   *
   * @param index the index of the element, the first element is 0
   * @return the structure element at the needed position, it can't be null
   * (!)
   */
  public AbstractPrologTerm getElement(final int index) {
    return elements[index];
  }

  /**
   * Set a structure element at a position by a prolog term
   *
   * @param index the position of the element, the first is 0
   * @param term  the term to be set into the position, must not be null
   */
  public void setElement(final int index, final AbstractPrologTerm term) {
    if (index < 0 || index >= getArity()) {
      throw new ArrayIndexOutOfBoundsException();
    }
    if (term == null) {
      throw new NullPointerException("Term must not be null");
    }
    elements[index] = term;
  }

  /**
   * Get the functor of the structure
   *
   * @return the functor
   */
  public AbstractPrologTerm getFunctor() {
    return functor;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getPrecedence() {
    if (functor.getType() == PrologTermType.OPERATOR) {
      return functor.getPrecedence();
    } else {
      return 0;
    }
  }

  /**
   * Make a copy of the structure with another term as the functor.
   *
   * @param newFunctor the new functor for the copy, it must not be null.
   * @return the new prolog structure with the same arity and terms in the body but with the new functor.
   */
  public PrologStructure copyWithAnotherFunctor(final AbstractPrologTerm newFunctor) {
    return new PrologStructure(newFunctor, elements);
  }

  /**
   * {@inheritDoc}
   */
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

      switch (operatorFunctor.getOperatorType()) {
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
