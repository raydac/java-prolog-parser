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
import com.igormaznitsa.prologparser.terms.Quotation;
import com.igormaznitsa.prologparser.terms.TermType;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.*;
import static java.util.Objects.requireNonNull;
import static java.util.stream.Stream.of;

/**
 * Prolog operator definition.
 */
public final class Op extends PrologTerm {

  public static final int PRECEDENCE_MAX = 0;
  public static final int PRECEDENCE_MIN = 1200;

  public static final Op GNU_UNARY_PLUS = make(200, FY, "+");
  public static final Op ISO_BITWISE_NEGATION = make(200, FY, "\\");
  public static final Op ISO_UNARY_MINUS = make(200, FY, "-");
  public static final Op ISO_UNIFICATION = make(700, XFX, "=", "\\=", "=..");
  public static final Op ISO_BITWISE_SHIFT = make(400, YFX, "<<", ">>");
  public static final Op ISO_ORDER_TERM = make(700, OpAssoc.XFX, "==", "\\==", "@<", "@=<", "@>", "@>=");

  public static final Op ISO_ORDER_ARITH = make(700, XFX, "<", "=<", ">", ">=", "=:=", "=\\=", "is");
  public static final Op ISO_ARITH_PLUS_MINUS = make(500, YFX, "+", "-");
  public static final Op ISO_ARITH_MUL_DIV = make(400, YFX, "*", "/");
  public static final Op ISO_ARITH_POWER = make(200, OpAssoc.XFX, "**");
  public static final Op ISO_ARITH_DIVIDE = make(400, OpAssoc.YFX, "//", "mod", "rem");
  public static final Op ISO_NEGATE = make(900, OpAssoc.FY, "\\+");

  public static final Op ISO_CLAUSES = make(1200, OpAssoc.XFX, ":-", "-->");
  public static final Op ISO_DIRECTIVES = make(1200, OpAssoc.FX, "?-", ":-");
  public static final Op ISO_BITWISE_AND_OR = make(500, OpAssoc.YFX, "/\\", "\\/");

  public static final Op MODIFIERS = make(1150, OpAssoc.FX,
      "public",
      "dynamic",
      "volatile",
      "discontiguous",
      "multifile",
      "initialization"
  );

  public static final Op GNU_DIV_RDIV = make(400, OpAssoc.YFX, "div", "rdiv");

  public static final Op ISO_OR = make(1100, OpAssoc.XFY, ";");
  public static final Op ISO_THEN = make(1050, OpAssoc.XFY, "->");
  public static final Op GNU_STAR_THEN = make(1050, OpAssoc.XFY, "*->");
  public static final Op GNU_DOUBLE_DOT = make(600, OpAssoc.XFY, ":");

  public static final List<Op> SICTUS_SPECIFIC = Collections.unmodifiableList(Arrays.asList(
      MODIFIERS,
      make(1150, FX, "mode", "block", "meta_predicate"),
      make(1100, XFY, "do"),
      make(900, FY, "spy", "nospy"),
      make(550, XFY, ":"),
      make(500, YFX, "\\"),
      GNU_UNARY_PLUS
  ));

  /**
   * Set of operators for ISO Prolog standard.
   */
  public static final List<Op> ISO = Collections.unmodifiableList(Arrays.asList(
      ISO_CLAUSES,
      ISO_DIRECTIVES,
      ISO_OR,
      ISO_THEN,
      ISO_NEGATE,
      ISO_UNIFICATION,
      ISO_ORDER_ARITH,
      ISO_ORDER_TERM,
      ISO_ARITH_PLUS_MINUS,
      ISO_BITWISE_AND_OR,
      ISO_ARITH_MUL_DIV,
      ISO_BITWISE_SHIFT,
      ISO_ARITH_DIVIDE,
      ISO_ARITH_POWER,
      make(200, OpAssoc.XFY, "^"),
      ISO_UNARY_MINUS,
      ISO_BITWISE_NEGATION,
      make(100, OpAssoc.XFX, "@")
  ));

  /**
   * Set of operators is specific for GNU Prolog use.
   */
  public static final List<Op> GNU_SPECIFIC = Collections.unmodifiableList(Arrays.asList(
      GNU_STAR_THEN,
      GNU_DOUBLE_DOT,
      GNU_DIV_RDIV,
      GNU_UNARY_PLUS
  ));

  /**
   * Set of operators is specific for SWI Prolog use.
   */
  public static final List<Op> SWI_SPECIFIC = Collections.unmodifiableList(Arrays.asList(
      MODIFIERS,
      make(1150, OpAssoc.FX, "meta_predicate", "module_transparent", "thread_local", "thread_initialization"),
      GNU_STAR_THEN,
      make(990, OpAssoc.FY, ":="),
      make(700, OpAssoc.XFX, "=@=", "\\=@=", "as", ">:<", ":<"),
      GNU_DOUBLE_DOT,
      make(500, OpAssoc.YFX, "xor"),
      make(500, OpAssoc.FX, "?"),
      GNU_DIV_RDIV,
      GNU_UNARY_PLUS,
      make(1, OpAssoc.FX, "$")
  ));

  /**
   * Set of operators for SWI Prolog.
   */
  public static final List<Op> SWI = Collections.unmodifiableList(Op.join(ISO, SWI_SPECIFIC));

  /**
   * Set of Finite Domain operators for GNU Prolog.
   */
  public static final List<Op> GNU_FD = Collections.unmodifiableList(Arrays.asList(
      make(750, XFY, "#<=>", "#\\<=>"),
      make(740, XFY, "#==>", "#\\==>"),
      make(730, XFY, "##"),
      make(730, YFX, "#\\/", "#\\\\/"),
      make(720, YFX, "#/\\", "#\\/\\"),
      make(710, FY, "#\\"),
      make(700, XFX, "#=", "#\\=", "#<", "#=<", "#>", "#>=", "#=#", "#\\=#", "#<#", "#=<#", "#>#", "#>=#")
  ));

  /**
   * Set of operators for GNU Prolog.
   */
  public static final List<Op> GNU = Collections.unmodifiableList(Op.join(ISO, GNU_SPECIFIC));

  /**
   * Set of Constraint Logic Programming operators for SWI Prolog.
   */
  public static final List<Op> SWI_CPL = Collections.unmodifiableList(Arrays.asList(
      make(300, FY, "~"),
      make(500, YFX, "#"),
      make(760, YFX, "#<==>"),
      make(750, XFY, "#==>"),
      make(750, YFX, "#<=="),
      make(740, YFX, "#\\/"),
      make(730, YFX, "#\\"),
      make(720, YFX, "#/\\"),
      make(710, FY, "#\\"),
      make(700, XFX, "#>", "#<", "#>=", "#=<", "#=", "#\\=", "in", "ins"),
      make(450, XFX, "..")
  ));

  public static final Op VIRTUAL_OPERATOR_BLOCK = makeSystem(-1, OpAssoc.FX, "()");
  public static final Op VIRTUAL_OPERATOR_CURLY_BLOCK = makeSystem(-1, OpAssoc.FX, "{}");
  public static final Op METAOPERATOR_COMMA = makeSystem(1000, OpAssoc.XFY, ",");
  public static final Op METAOPERATOR_LEFT_BRACKET = makeSystem(-1, OpAssoc.FX, "(");
  public static final Op METAOPERATOR_LEFT_CURLY_BRACKET = makeSystem(-1, OpAssoc.FX, "{");
  public static final Op METAOPERATOR_RIGHT_BRACKET = makeSystem(-1, OpAssoc.XF, ")");
  public static final Op METAOPERATOR_RIGHT_CURLY_BRACKET = makeSystem(-1, OpAssoc.XF, "}");
  public static final Op METAOPERATOR_LEFT_SQUARE_BRACKET = makeSystem(-1, OpAssoc.FX, "[");
  public static final Op METAOPERATOR_RIGHT_SQUARE_BRACKET = makeSystem(-1, OpAssoc.XF, "]");
  public static final Op METAOPERATOR_DOT = makeSystem(Integer.MAX_VALUE, OpAssoc.XF, ".");
  public static final Op METAOPERATOR_VERTICAL_BAR = makeSystem(1105, OpAssoc.XFY, "|");

  private static final long serialVersionUID = -5914313127778138548L;
  private final OpAssoc opAssoc;
  private final int precedence;
  private final int preparedHash;

  private final String[] multiNames;

  private Op(final int precedence, final OpAssoc type, final String name, final String[] multiNames) {
    super(name, Quotation.NONE);

    assertOpValidOpName(name);

    this.multiNames = multiNames;
    this.opAssoc = type;
    this.precedence = precedence;

    this.preparedHash = (name + '/' + this.opAssoc.name() + '/' + this.precedence).hashCode();
  }

  private static String[] assertOpValidOpName(final String[] names) {
    Arrays.stream(names).forEach(Op::assertOpValidOpName);
    return names;
  }

  private static String assertOpValidOpName(final String name) {
    final char firstChar = assertNonEmptyString(name).charAt(0);

    if (Character.isWhitespace(firstChar) || Character.isISOControl(firstChar)) {
      throw new IllegalArgumentException("Space char as first one");
    }

    if (Character.isUpperCase(firstChar)) {
      throw new IllegalArgumentException("Capital char as first one");
    }

    if (firstChar == '_') {
      throw new IllegalArgumentException("'_' can't be firs char");
    }

    return name;
  }

  /**
   * Make operator descriptor describing bunch of operators with same
   * characteristics but differently named.
   *
   * @param precedence the precedence
   * @param type       the type of operators
   * @param names      names of operators, must not be empty or contain null
   * @return generated operator descriptor
   * @see OpAssoc
   */
  public static Op make(final int precedence, final OpAssoc type, final String... names) {
    if (precedence < PRECEDENCE_MAX || precedence > PRECEDENCE_MIN) {
      throw new IllegalArgumentException("Precedence must be in 0..1200");
    }

    requireNonNull(type);
    requireNonNull(names);

    if (names.length == 0) {
      throw new IllegalArgumentException("Operator name must be defined");
    }

    return names.length == 1 ? new Op(precedence, type, requireNonNull(names[0]), null)
        : new Op(precedence, type, ".multi.", assertOpValidOpName(names));
  }

  public static Op makeSystem(final int precedence, final OpAssoc type, final String... names) {
    requireNonNull(type);
    requireNonNull(names);

    if (names.length == 0) {
      throw new IllegalArgumentException("Operator name must be defined");
    }

    return names.length == 1 ? new Op(precedence, type, assertOpValidOpName(names[0]), null)
        : new Op(precedence, type, ".system.", assertOpValidOpName(names));
  }

  @SafeVarargs
  public static List<Op> join(final List<Op>... args) {
    final List<Op> result = new ArrayList<>();
    for (final List<Op> l : args) {
      result.addAll(l);
    }
    return result;
  }

  public static Op[] add(final Op[] args, final Op... ops) {
    final int newLen = args.length + ops.length;
    final Op[] result = Arrays.copyOf(args, newLen);

    int sourceIndex = 0;
    for (int i = args.length; i < newLen; i++) {
      result[i] = ops[sourceIndex++];
    }
    return result;
  }

  public boolean isMultiName() {
    return this.multiNames != null;
  }

  public Stream<Op> streamOp() {
    if (this.multiNames == null) {
      return of(this);
    } else {
      return of(this.multiNames).map(x -> new Op(this.precedence, this.opAssoc, x, null));
    }
  }

  @Override
  public int getArity() {
    return this.opAssoc.getArity();
  }

  @Override
  public TermType getType() {
    return TermType.OPERATOR;
  }

  public OpAssoc getAssoc() {
    return this.opAssoc;
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
          switch (this.opAssoc) {
            case XFY:
            case XFX:
            case YFX: {
              result = false;
            }
            break;
            case XF:
            case FX: {
              final PrologTerm atom = struct.getTermAt(0);
              result = atom != null && atom.getPrecedence() < getPrecedence();
            }
            break;
            case YF:
            case FY: {
              final PrologTerm atom = struct.getTermAt(0);
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
          switch (this.opAssoc) {
            case XFY:
            case XFX:
            case YFX: {
              final PrologTerm elementLeft = struct.getTermAt(0);
              final PrologTerm elementRight = struct.getTermAt(1);

              if (elementLeft == null || elementRight == null) {
                result = false;
              } else {

                switch (this.opAssoc) {
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
              final PrologTerm atom = struct.getTermAt(this.opAssoc == OpAssoc.XF ? 0 : 1);
              result = atom != null && atom.getPrecedence() < getPrecedence();
            }
            break;

            case YF:
            case FY: {
              final PrologTerm atom = struct.getTermAt(this.opAssoc == OpAssoc.YF ? 0 : 1);
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
          && this.opAssoc == op.opAssoc
          && this.text.equals(op.text)) {
        result = true;
      }
    }

    return result;
  }

  @Override
  public String toString() {
    if (isMultiName()) {
      return String.format("op(%d, %s, [%s]).", getPrecedence(), getAssoc().toString().toLowerCase(Locale.ENGLISH), of(this.multiNames).map(x -> '\'' + x + '\'').collect(Collectors.joining(",")));
    } else {
      return String.format("op(%d, %s, '%s').", getPrecedence(), getAssoc().toString().toLowerCase(Locale.ENGLISH), getText());
    }
  }

  private Object readResolve() {
    final Object result = GenericPrologParser.findBaseMetaOperator(this.text, this.opAssoc);
    return result == null ? this : result;
  }
}
