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

package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.SpecServiceCompound;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.utils.AssertUtils;

import java.util.Locale;
import java.util.stream.Stream;

/**
 * Prolog operator definition.
 */
public final class Op extends SpecServiceCompound {
  public static final Op METAOPERATOR_LEFT_BRACKET = makeSystem(-1, OpType.FX, "(");
  public static final Op METAOPERATOR_RIGHT_BRACKET = makeSystem(-1, OpType.XF, ")");
  public static final Op METAOPERATOR_LEFT_SQUARE_BRACKET = makeSystem(-1, OpType.FX, "[");
  public static final Op METAOPERATOR_RIGHT_SQUARE_BRACKET = makeSystem(-1, OpType.XF, "]");
  public static final Op METAOPERATOR_DOT = makeSystem(Integer.MAX_VALUE, OpType.XF, ".");
  public static final Op METAOPERATOR_VERTICAL_BAR = makeSystem(Integer.MAX_VALUE - 1, OpType.XFY, "|");
  public static final int PRECEDENCE_MAX = 0;
  public static final int PRECEDENCE_MIN = 1200;
  private static final long serialVersionUID = -5954313127778138548L;
  private static final Op[] EMPTY = new Op[0];
  private final OpType opType;
  private final int precedence;
  private final int preparedHash;

  private final String[] multiNames;

  private Op(final int precedence, final OpType type, final String name, final String[] multiNames) {
    super(name);

    assertOpValidOpName(name);

    this.multiNames = multiNames;
    this.opType = type;
    this.precedence = precedence;

    this.preparedHash = (name + '/' + this.opType.name() + '/' + this.precedence).hashCode();
  }

  private static void assertOpValidOpName(final String name) {
    AssertUtils.assertStringNotNullAndNotEmpty(name);

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

  /**
   * Gemerate operators for names.
   *
   * @param precedence the priority
   * @param type       the type of operators
   * @param names      names of operators to generate
   * @return array of generated operators
   * @see OpType
   */
  public static Op make(final int precedence, final OpType type, final String... names) {
    if (precedence < PRECEDENCE_MAX || precedence > PRECEDENCE_MIN) {
      throw new IllegalArgumentException("Precedence must be in 0..1200");
    }

    AssertUtils.assertNotNull(type);
    AssertUtils.assertNotNull(names);

    if (names.length == 0) {
      throw new IllegalArgumentException("Operator name must be defined");
    }

    return names.length == 1 ? new Op(precedence, type, names[0], null)
        : new Op(precedence, type, ".multi.", names);
  }

  static Op makeSystem(final int precedence, final OpType type, final String... names) {
    AssertUtils.assertNotNull(type);
    AssertUtils.assertNotNull(names);

    if (names.length == 0) {
      throw new IllegalArgumentException("Operator name must be defined");
    }

    return names.length == 1 ? new Op(precedence, type, names[0], null)
        : new Op(precedence, type, ".system.", names);
  }

  public Stream<Op> streamOp() {
    if (this.multiNames == null) {
      return Stream.of(this);
    } else {
      return Stream.of(this.multiNames).map(x -> new Op(this.precedence, this.opType, x, null));
    }
  }

  @Override
  public int getArity() {
    return this.opType.getArity();
  }

  @Override
  public PrologTerm getElementAt(int position) {
    throw new UnsupportedOperationException("Can't get element from operator");
  }

  @Override
  public TermType getTermType() {
    return TermType.__OPERATOR__;
  }

  public OpType getOpType() {
    return this.opType;
  }

  @Override
  public int getPrecedence() {
    return this.precedence;
  }

  public boolean isCompatibleWith(final PrologStruct struct) {
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
              final PrologTerm atom = struct.getElementAt(0);
              result = atom != null && atom.getPrecedence() < getPrecedence();
            }
            break;
            case YF:
            case FY: {
              final PrologTerm atom = struct.getElementAt(0);
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
              final PrologTerm elementLeft = struct.getElementAt(0);
              final PrologTerm elementRight = struct.getElementAt(1);

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
              final PrologTerm atom = struct.getElementAt(this.opType == OpType.XF ? 0 : 1);
              result = atom != null && atom.getPrecedence() < getPrecedence();
            }
            break;

            case YF:
            case FY: {
              final PrologTerm atom = struct.getElementAt(this.opType == OpType.YF ? 0 : 1);
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
    return this.preparedHash;
  }

  @Override
  public boolean equals(final Object obj) {
    boolean result = false;

    if (this == obj) {
      result = true;
    } else if (obj instanceof Op) {
      final Op op = (Op) obj;
      if (this.preparedHash == op.preparedHash
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
    return String.format("of(%d,%s,'%s').", getPrecedence(), getOpType().toString().toLowerCase(Locale.ENGLISH), getTermText());
  }

  private Object readResolve() {
    final Object result = GenericPrologParser.findSystemOperatorForNameAndType(this.text, this.opType);
    return result == null ? this : result;
  }
}
