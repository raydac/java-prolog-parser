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
import java.util.stream.Stream;

import static com.igormaznitsa.prologparser.utils.AssertUtils.assertNotNull;

/**
 * Base class for all prolog terms.
 */
public abstract class PrologTerm implements Serializable, Comparable<PrologTerm> {

  private static final long serialVersionUID = 1402429096900255841L;

  protected final String text;
  private volatile Serializable payload;
  private int line;
  private int pos;

  public PrologTerm(final String text) {
    this.text = assertNotNull(text);
    this.pos = -1;
    this.line = -1;
  }

  public PrologTerm(final String text, final int line, final int pos) {
    this(text);
    setLine(line);
    setPos(pos);
  }

  public final int getPos() {
    return pos;
  }

  public final void setPos(final int pos) {
    this.pos = pos <= 0 ? -1 : pos;
  }

  public final int getLine() {
    return line;
  }

  public final void setLine(final int line) {
    this.line = line <= 0 ? -1 : line;
  }

  public String getTermText() {
    return this.text;
  }

  public int getPrecedence() {
    return 0;
  }

  @Override
  public String toString() {
    return text;
  }

  public Serializable getPayload() {
    return this.payload;
  }

  public void setPayload(final Serializable obj) {
    this.payload = obj;
  }

  public abstract TermType getTermType();

  public Stream<PrologTerm> stream() {
    return Stream.of(this);
  }

  @Override
  public int compareTo(final PrologTerm that) {
    if (that instanceof SpecServiceCompound) {
      return -1;
    }

    final int result;
    switch (this.getTermType()) {
      case VAR: {
        if (that.getTermType() == TermType.VAR) {
          result = this.getTermText().compareTo(that.getTermText());
        } else {
          result = -1;
        }
      }
      break;
      case ATOM: {
        if (that.getTermType() == TermType.ATOM) {
          if (this instanceof PrologNumeric) {
            if (that instanceof PrologNumeric) {
              if (this instanceof PrologInteger) {
                if (that instanceof PrologInteger) {
                  result = ((PrologInteger) this).getIntValue().compareTo(((PrologInteger) that).getIntValue());
                } else {
                  result = new BigDecimal(((PrologInteger) this).getIntValue()).compareTo(((PrologFloat) that).getFloatValue());
                }
              } else if (that instanceof PrologInteger) {
                result = ((PrologFloat) this).getFloatValue().compareTo((new BigDecimal(((PrologInteger) that).getIntValue())));
              } else {
                result = ((PrologFloat) this).getFloatValue().compareTo(((PrologFloat) that).getFloatValue());
              }
            } else {
              result = -1;
            }
          } else if (that instanceof PrologNumeric) {
            result = 1;
          } else {
            result = this.getTermText().compareTo(that.getTermText());
          }
        } else if (that.getTermType() == TermType.VAR) {
          result = 1;
        } else {
          result = -1;
        }
      }
      break;
      case LIST:
      case STRUCT: {
        if (that instanceof PrologCompound) {
          int lresult = Integer.compare(((PrologCompound) this).getArity(), ((PrologCompound) that).getArity());
          if (lresult == 0) {
            lresult = this.getTermText().compareTo(that.getTermText());
          }
          if (lresult == 0) {
            for (int i = 0; i < ((PrologCompound) this).getArity(); i++) {
              lresult = ((PrologCompound) this).getElementAt(i).compareTo(((PrologCompound) that).getElementAt(i));
              if (lresult != 0) {
                break;
              }
            }
          }
          result = lresult;
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
