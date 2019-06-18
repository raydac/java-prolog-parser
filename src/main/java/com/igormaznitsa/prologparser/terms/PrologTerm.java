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

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;
import java.util.stream.Stream;

import static com.igormaznitsa.prologparser.terms.Quotation.NONE;
import static com.igormaznitsa.prologparser.terms.Quotation.SINGLE;
import static com.igormaznitsa.prologparser.terms.TermType.ATOM;
import static com.igormaznitsa.prologparser.terms.TermType.VAR;
import static com.igormaznitsa.prologparser.utils.AssertUtils.assertNotNull;

/**
 * Base class describes a Prolog term
 */
public abstract class PrologTerm implements Serializable, Comparable<PrologTerm> {

  private static final long serialVersionUID = 1402429096900255841L;
  protected final Quotation quotation;
  protected final String text;
  private int line;
  private int pos;

  public PrologTerm(final String text, final Quotation quotation) {
    this.text = assertNotNull(text);
    this.pos = -1;
    this.line = -1;
    this.quotation = quotation == null ? NONE : quotation;
  }

  public PrologTerm(final String text, final Quotation quotation, final int line, final int pos) {
    this(text, quotation);
    setLine(line);
    setPos(pos);
  }

  /**
   * Find appropriate quotation for text.
   *
   * @param atomText text, must not be null
   * @return founded quotation
   */
  public static Quotation findQuotation(final String atomText) {
    Quotation result = NONE;

    if (atomText.length() == 0) {
      result = SINGLE;
    } else {
      char chr = atomText.charAt(0);
      if (!Character.isLetter(chr) || Character.isDigit(chr) || Character.isUpperCase(chr) || Character.isISOControl(chr) || Character.isWhitespace(chr)) {
        result = SINGLE;
      } else {

        for (int i = 1; i < atomText.length(); i++) {
          chr = atomText.charAt(i);
          if (Character.isWhitespace(chr) || (chr != '_' && !Character.isLetterOrDigit(chr)) || Character.isISOControl(chr)) {
            result = SINGLE;
            break;
          }
        }
      }
    }
    return result;
  }

  /**
   * Flat content of the term for comma, make sense for structures.
   *
   * @param list list to be filled by comma separated terms
   * @return the same list
   */
  public List<PrologTerm> flatComma(final List<PrologTerm> list) {
    list.add(this);
    return list;
  }

  /**
   * Arity of element.
   *
   * @return arity of element, make sense for compound terms, for primitive
   * terms is 1
   */
  public int getArity() {
    return 1;
  }

  /**
   * Check the term describes '()' or '{}' block.
   *
   * @return true if the term is structure describes any kind of supported block
   * @since 2.0.1
   */
  public boolean isAnyBlock() {
    return false;
  }

  /**
   * Check the term describes '()' block.
   *
   * @return true if the term is structure with '()' as functor
   */
  public boolean isBlock() {
    return false;
  }

  /**
   * Check the term describes '{}' block.
   *
   * @return true if the term is structure with '{}' as functor
   */
  public boolean isCurlyBlock() {
    return false;
  }

  /**
   * Get quotation for the term.
   *
   * @return the quotation for the term, must not be null
   */
  public Quotation getQuotation() {
    return this.quotation;
  }

  /**
   * Position of the term in the line, first position is 1
   *
   * @return position in line or -1 if unknown
   */
  public final int getPos() {
    return pos;
  }

  public final void setPos(final int pos) {
    this.pos = pos <= 0 ? -1 : pos;
  }

  /**
   * Line of the term, first line is 1
   *
   * @return line or -1 if unknown
   */
  public final int getLine() {
    return line;
  }

  public final void setLine(final int line) {
    this.line = line <= 0 ? -1 : line;
  }

  /**
   * Get the term text.
   *
   * @return the term text, must not be null
   */
  public String getText() {
    return this.text;
  }

  /**
   * Get precedence of the term.
   *
   * @return precedence, must not be negative
   */
  public int getPrecedence() {
    return 0;
  }

  @Override
  public String toString() {
    final String result;
    if (this.quotation == NONE) {
      result = this.text;
    } else {
      result = this.quotation.quotateString(this.text);
    }
    return result;
  }

  /**
   * Get term type.
   *
   * @return term type, must not be null
   */
  public abstract TermType getType();

  public Stream<PrologTerm> stream() {
    return Stream.of(this);
  }

  /**
   * Get the functor, make sense of structure and its successors, for primitive
   * returns the same term.
   *
   * @return functor for structure or the same term
   */
  public PrologTerm getFunctor() {
    return this;
  }

  @Override
  public int compareTo(final PrologTerm that) {
    final int result;
    switch (this.getType()) {
      case VAR: {
        if (that.getType() == VAR) {
          result = this.getText().compareTo(that.getText());
        } else {
          result = -1;
        }
      }
      break;
      case ATOM: {
        if (that.getType() == ATOM) {
          if (this instanceof PrologNumeric) {
            if (that instanceof PrologNumeric) {
              if (this instanceof PrologInt) {
                if (that instanceof PrologInt) {
                  result = ((PrologInt) this).getIntValue().compareTo(((PrologInt) that).getIntValue());
                } else {
                  result = new BigDecimal(((PrologInt) this).getIntValue()).compareTo(((PrologFloat) that).getFloatValue());
                }
              } else if (that instanceof PrologInt) {
                result = ((PrologFloat) this).getFloatValue().compareTo((new BigDecimal(((PrologInt) that).getIntValue())));
              } else {
                result = ((PrologFloat) this).getFloatValue().compareTo(((PrologFloat) that).getFloatValue());
              }
            } else {
              result = -1;
            }
          } else if (that instanceof PrologNumeric) {
            result = 1;
          } else {
            result = this.getText().compareTo(that.getText());
          }
        } else if (that.getType() == VAR) {
          result = 1;
        } else {
          result = -1;
        }
      }
      break;
      case LIST:
      case STRUCT: {
        if (that instanceof PrologCompound) {
          int leftResult = Integer.compare(this.getArity(), that.getArity());
          if (leftResult == 0) {
            leftResult = this.getText().compareTo(that.getText());
          }
          if (leftResult == 0) {
            for (int i = 0; i < this.getArity(); i++) {
              leftResult = ((PrologCompound) this).getTermAt(i).compareTo(((PrologCompound) that).getTermAt(i));
              if (leftResult != 0) {
                break;
              }
            }
          }
          result = leftResult;
        } else {
          result = 1;
        }
      }
      break;
      default:
        throw new CriticalUnexpectedError();
    }
    return result;
  }

}
