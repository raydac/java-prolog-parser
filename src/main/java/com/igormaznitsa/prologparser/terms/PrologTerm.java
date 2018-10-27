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
import com.igormaznitsa.prologparser.utils.StringUtils;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.stream.Stream;

import static com.igormaznitsa.prologparser.utils.AssertUtils.assertNotNull;

/**
 * Base class for all prolog terms.
 */
public abstract class PrologTerm implements Serializable, Comparable<PrologTerm> {

  private static final long serialVersionUID = 1402429096900255841L;
  protected final QuotingType quotingType;
  protected final String text;
  private int line;
  private int pos;

  public PrologTerm(final String text, final QuotingType quotingType) {
    this.text = assertNotNull(text);
    this.pos = -1;
    this.line = -1;
    this.quotingType = quotingType;
  }

  public PrologTerm(final String text, final QuotingType quotingType, final int line, final int pos) {
    this(text, quotingType);
    setLine(line);
    setPos(pos);
  }

  public static QuotingType findAppropriateQuoting(final String atomText) {
    QuotingType result = QuotingType.NO_QUOTED;

    if (atomText.length() == 0) {
      result = QuotingType.SINGLE_QUOTED;
    } else {
      char chr = atomText.charAt(0);
      if (!Character.isLetter(chr) || Character.isDigit(chr) || Character.isUpperCase(chr) || Character.isISOControl(chr) || Character.isWhitespace(chr)) {
        result = QuotingType.SINGLE_QUOTED;
      } else {

        for (int i = 1; i < atomText.length(); i++) {
          chr = atomText.charAt(i);
          if (Character.isWhitespace(chr) || (chr != '_' && !Character.isLetterOrDigit(chr)) || Character.isISOControl(chr)) {
            result = QuotingType.SINGLE_QUOTED;
            break;
          }
        }
      }
    }
    return result;
  }

  public int getArity() {
    return 1;
  }

  public boolean isBlock() {
    return false;
  }

  public QuotingType getQuotingType() {
    return this.quotingType;
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
    final String result;
    if (this.quotingType == QuotingType.NO_QUOTED) {
      result = this.text;
    } else {
      result = this.quotingType.makeString(this.text);
    }
    return result;
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
          int leftResult = Integer.compare(this.getArity(), that.getArity());
          if (leftResult == 0) {
            leftResult = this.getTermText().compareTo(that.getTermText());
          }
          if (leftResult == 0) {
            for (int i = 0; i < this.getArity(); i++) {
              leftResult = ((PrologCompound) this).getElementAt(i).compareTo(((PrologCompound) that).getElementAt(i));
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

  public enum QuotingType {
    NO_QUOTED(""),
    SINGLE_QUOTED("\'"),
    DOUBLE_QUOTED("\""),
    BACK_QUOTED("`");

    private final String quote;

    QuotingType(final String quote) {
      this.quote = quote;
    }

    public String getDelimiter() {
      return this.quote;
    }

    public String makeString(final String atomText) {
      return this.quote + StringUtils.escapeString(atomText, this) + this.quote;
    }
  }

}
