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
import com.igormaznitsa.prologparser.utils.StringBuilderEx;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import static com.igormaznitsa.prologparser.terms.TermType.LIST;
import static com.igormaznitsa.prologparser.utils.AssertUtils.assertNotNull;

/**
 * Representation of prolog structure.
 */
public class PrologStruct extends PrologCompound implements Iterable<PrologTerm> {

  public static final PrologAtom EMPTY_ATOM = new PrologAtom("", Quotation.SINGLE);
  private static final long serialVersionUID = 9000641998734217154L;

  protected final PrologTerm functor;
  protected final PrologTerm[] elements;

  public PrologStruct(final PrologTerm functor, final PrologTerm[] elements) {
    super(functor.getText());
    this.functor = assertFunctor(functor);
    this.elements = assertNotNull(elements.clone());
  }

  public PrologStruct(
          final PrologTerm functor,
          final PrologTerm[] elements,
          final int line, final int pos
  ) {
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

    if (arity < 0) {
      throw new IllegalArgumentException("Negative arity");
    }

    this.functor = assertFunctor(functor);

    this.elements = new PrologTerm[arity];
    Arrays.fill(elements, EMPTY_ATOM);
  }

  protected PrologStruct(final PrologTerm functor, final int arity, final int line, final int pos) {
    this(functor, arity);
    setPos(pos);
    setLine(line);
  }

  private static PrologTerm assertFunctor(final PrologTerm functor) {
    if (functor.getType() == LIST || functor instanceof InternalSpecialCompoundTerm) {
      throw new IllegalArgumentException("Non-allowed functor type: " + functor.getType());
    }
    if (functor instanceof PrologNumeric) {
      throw new IllegalArgumentException("Functor can't be number: " + functor);
    }
    return functor;
  }

  @Override
  public TermType getType() {
    return TermType.STRUCT;
  }

  @Override
  public int getArity() {
    return this.elements.length;
  }

  @Override
  public PrologTerm getTermAt(final int index) {
    return this.elements[index];
  }

  /**
   * Set element for its position.
   *
   * @param index zero based index
   * @param term term to set to position
   * @throws ArrayIndexOutOfBoundsException if wrong index
   */
  public void setElementAt(final int index, final PrologTerm term) {
    if (index < 0 || index >= getArity()) {
      throw new ArrayIndexOutOfBoundsException(index);
    }
    this.elements[index] = assertNotNull(term);
  }

  @Override
  public PrologTerm getFunctor() {
    return functor;
  }

  @Override
  public int getPrecedence() {
    if (this.functor.getType() == TermType.OPERATOR) {
      return this.functor.getPrecedence();
    } else {
      return 0;
    }
  }

  /**
   * Make copy of the structure with replacement of functor.
   *
   * @param newFunctor the new functor, must not be null
   * @return the new instance with replaced functor.
   */
  public PrologStruct copyWithAnotherFunctor(final PrologTerm newFunctor) {
    return new PrologStruct(newFunctor, this.elements);
  }

  @Override
  public boolean isAnyBlock() {
    return this.functor == Op.VIRTUAL_OPERATOR_BLOCK
            || this.functor == Op.VIRTUAL_OPERATOR_CURLY_BLOCK;
  }

  @Override
  public boolean isCurlyBlock() {
    return this.functor == Op.VIRTUAL_OPERATOR_CURLY_BLOCK;
  }

  @Override
  public boolean isBlock() {
    return this.functor == Op.VIRTUAL_OPERATOR_BLOCK;
  }

  @Override
  public List<PrologTerm> flatComma(final List<PrologTerm> list) {
    if (this.functor == Op.METAOPERATOR_COMMA) {
      for (final PrologTerm t : this.elements) {
        t.flatComma(list);
      }
    } else {
      super.flatComma(list);
    }
    return list;
  }

  @Override
  public String toString() {
    final StringBuilderEx builder = new StringBuilderEx(64);

    if (this.functor.getType() == TermType.OPERATOR) {
      if (this.isAnyBlock()) {
        if (this.isCurlyBlock()) {
          if (this.isEmpty()) {
            builder.append("{}");
          } else {
            builder.append('{').append(this.elements[0].toString()).append('}');
          }
        } else {
          if (this.isEmpty()) {
            builder.append("()");
          } else {
            builder.append('(').append(this.elements[0].toString()).append(')');
          }
        }
      } else {
        final Op operatorFunctor = (Op) functor;
        final String opName = operatorFunctor.getText();
        final int functorPrecedence = operatorFunctor.getPrecedence();

        final PrologTerm arg1 = getTermAt(0);
        final String text1 = getTermAt(0).toString();

        final PrologTerm arg2 = getArity() > 1 ? getTermAt(1) : null;
        final String text2 = arg2 == null ? null : getTermAt(1).toString();

        switch (operatorFunctor.getAssoc()) {
          case FX: {
            builder.append(opName).append(' ');

            if (arg1.isBlock() || arg1.getPrecedence() >= functorPrecedence) {
              builder.append('(').append(text1).append(')');
            } else {
              builder.append(text1);
            }
          }
          break;
          case FY: {
            builder.append(opName);
            builder.append(' ');

            if (arg1.isBlock() || arg1.getPrecedence() > functorPrecedence) {
              builder.append('(').append(text1).append(')');
            } else {
              builder.append(text1);
            }
          }
          break;
          case XF: {
            if (arg1.isBlock() || arg1.getPrecedence() >= functorPrecedence) {
              builder.append('(').append(text1).append(')');
            } else {
              builder.append(text1);
            }

            builder.append(' ').append(opName);
          }
          break;
          case YF: {
            if (arg1.isBlock() || arg1.getPrecedence() > functorPrecedence) {
              builder.append('(').append(text1).append(')');
            } else {
              builder.append(text1);
            }

            builder.append(' ').append(opName);
          }
          break;
          case XFX: {
            if (arg1.getPrecedence() >= functorPrecedence) {
              builder.append('(').append(text1).append(')');
            } else {
              builder.append(text1);
            }

            builder.append(' ').append(opName).append(' ');

            if (arg2.getPrecedence() >= functorPrecedence) {
              builder.append('(').append(text2).append(')');
            } else {
              builder.append(text2);
            }
          }
          break;
          case YFX: {
            if (arg1.getPrecedence() > functorPrecedence) {
              builder.append('(').append(text1).append(')');
            } else {
              builder.append(text1);
            }

            builder.append(' ').append(opName).append(' ');

            if (arg2.getPrecedence() >= functorPrecedence) {
              builder.append('(').append(text2).append(')');
            } else {
              builder.append(text2);
            }
          }
          break;
          case XFY: {
            if (arg1.getPrecedence() >= functorPrecedence) {
              builder.append('(').append(text1).append(')');
            } else {
              builder.append(text1);
            }

            builder.append(' ').append(opName).append(' ');

            if (arg2.getPrecedence() > functorPrecedence) {
              builder.append('(').append(text2).append(')');
            } else {
              builder.append(text2);
            }
          }
          break;
          default:
            throw new CriticalUnexpectedError();
        }
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

  /**
   * Check that the structure has elements.
   *
   * @return true if there is not elements, false otherwise
   */
  public boolean isEmpty() {
    return this.elements.length == 0;
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
      final PrologTerm[] terms = elements.clone();
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
