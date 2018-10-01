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

package com.igormaznitsa.prologparser.operators;

import com.igormaznitsa.prologparser.AbstractPrologParser;
import com.igormaznitsa.prologparser.annotations.PrologOperator;
import com.igormaznitsa.prologparser.annotations.PrologOperators;
import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologStructure;
import com.igormaznitsa.prologparser.terms.PrologTermType;

import java.util.Locale;

/**
 * The class describes a prolog operator for the prolog parser.
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @see PrologOperator
 * @see PrologOperators
 */
public final class Operator extends AbstractPrologTerm {

  /**
   * Describes the left bracket meta-operator ('(')
   */
  public static final Operator METAOPERATOR_LEFT_BRACKET = makeMetaOperator(-1, OperatorType.FX, "(");
  /**
   * Describes the right bracket meta-operator (')')
   */
  public static final Operator METAOPERATOR_RIGHT_BRACKET = makeMetaOperator(-1, OperatorType.XF, ")");
  /**
   * Describes the left square bracket meta-operator ('[')
   */
  public static final Operator METAOPERATOR_LEFT_SQUARE_BRACKET = makeMetaOperator(-1, OperatorType.FX, "[");
  /**
   * Describes the right square bracket meta-operator (']')
   */
  public static final Operator METAOPERATOR_RIGHT_SQUARE_BRACKET = makeMetaOperator(-1, OperatorType.XF, "]");
  /**
   * Describes the dot meta-operator ('.')
   */
  public static final Operator METAOPERATOR_DOT = makeMetaOperator(Integer.MAX_VALUE, OperatorType.XF, ".");
  /**
   * Describes the vertical bar meta-operator ('|')
   */
  public static final Operator METAOPERATOR_VERTICAL_BAR = makeMetaOperator(Integer.MAX_VALUE - 1, OperatorType.XFY, "|");
  /**
   * The constant describes the maximum priority for a prolog operator.
   */
  public static final int PRIORITY_MAX = 0;
  /**
   * The constant describes the minimum priority for a prolog operator.
   */
  public static final int PRIORITY_MIN = 1200;
  private static final long serialVersionUID = -5954317427778538548L;
  /**
   * The variable contains the operator type.
   */
  private final OperatorType opType;
  /**
   * The variable contains the operator priority value.
   */
  private final int opPriority;
  /**
   * The variable contains the pre-calculated hash code for the operator.
   */
  private final int precalculatedHashCode;

  /**
   * The constructor. It has been hidden since 1.02 version because we must
   * avoid direct operator creation (!)
   *
   * @param priority the operator priority 0..1200
   * @param type     the operator type, must not be null
   * @param name     the operator name, must not be null
   * @throws java.lang.IllegalArgumentException will be thrown if there is
   *                                            some incompatible value at arguments
   * @see OperatorType
   * @see Operator#makeOperator(int,
   * com.igormaznitsa.prologparser.operators.OperatorType, java.lang.String)
   * @see Operator#makeOperators(int,
   * com.igormaznitsa.prologparser.operators.OperatorType, java.lang.String[])
   */
  private Operator(final int priority, final OperatorType type,
                   final String name) {
    super(name);

    if (type == null) {
      throw new NullPointerException("Type must not be null");
    }
    if (name.isEmpty()) {
      throw new IllegalArgumentException("Operator name must not be empty");
    }

    final char firstLetter = name.charAt(0);

    if (Character.isWhitespace(firstLetter) || Character.isISOControl(firstLetter)) {
      throw new IllegalArgumentException(
          "Space char as the first one at name");
    }

    if (Character.isUpperCase(firstLetter)) {
      throw new IllegalArgumentException(
          "Capital char as the first one at name");
    }

    if (firstLetter == '_') {
      throw new IllegalArgumentException("'_' as the first char");
    }

    opType = type;
    opPriority = priority;

    precalculatedHashCode = (name + "!" + this.opType + "!" + this.opPriority).hashCode();
  }

  /**
   * This auxiliary function allows to generate a lot of similar operators
   * from a string array
   *
   * @param priority the priority for all generated operators 0..1200
   * @param type     the type for all generated operators, must not be null
   * @param names    a string array contains names for new operators, must not be
   *                 null
   * @return an array of new Operator objects which were generated for the
   * arguments and they have the same type and priority but different names.
   * @see OperatorType
   */
  public static Operator[] makeOperators(final int priority,
                                         final OperatorType type, final String[] names) {
    if (priority < PRIORITY_MAX || priority > PRIORITY_MIN) {
      throw new IllegalArgumentException(
          "Priority must be in the PRIORITY_MAX(0)..PRIORITY_MIN(1200)");
    }

    if (type == null) {
      throw new NullPointerException("Type is null");
    }
    if (names == null) {
      throw new NullPointerException("Name array is null");
    }

    final Operator[] result = new Operator[names.length];
    for (int li = 0; li < names.length; li++) {
      result[li] = makeOperator(priority, type, names[li]);
    }
    return result;
  }

  /**
   * This factory method allows to generate new operator with desired
   * parameters, it will generate new instance every time because there is not
   * any inside logic to cache instances(!).
   *
   * @param priority the operator priority must be in the [1..1200] interval
   * @param type     the operator type, must not be null
   * @param name     the operator name, must not be null or empty
   * @return the new generated operator instance for arguments
   * @throws IllegalArgumentException if there is a wrong priority value
   */
  public static Operator makeOperator(final int priority, final OperatorType type, final String name) {
    if (priority < PRIORITY_MAX || priority > PRIORITY_MIN) {
      throw new IllegalArgumentException("Wrong priority value");
    }

    return new Operator(priority, type, name);
  }

  /**
   * This inside factory method is used to generate operators without check of
   * their priority
   *
   * @param priority the operator priority, it can be any integer value
   * @param type     the operator type, it must not be null
   * @param name     the operator name, it must not be null or empty
   * @return the new generated operator instance
   */
  private static Operator makeMetaOperator(final int priority, final OperatorType type, final String name) {
    return new Operator(priority, type, name);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public PrologTermType getType() {
    return PrologTermType.OPERATOR;
  }

  /**
   * Get the type of the operator
   *
   * @return the operator type
   */
  public OperatorType getOperatorType() {
    return opType;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getPriority() {
    return opPriority;
  }

  /**
   * Check that the operator is compatible with a prolog structure and can be
   * the functor for the structure.
   *
   * @param struct the structure to be checked, must not be null
   * @return true if the operator is compatible with the structure else false
   */
  public boolean compatibleWith(final PrologStructure struct) {
    final boolean result;
    if (struct != null) {

      switch (struct.getArity()) {
        case 1:
          switch (opType) {
            case XFY:
            case XFX:
            case YFX:
              result = false;
              break;
            case XF:
            case FX: {
              final AbstractPrologTerm atom = struct.getElement(0);
              result = atom != null && atom.getPriority() < getPriority();
            }
            break;
            case YF:
            case FY: {
              final AbstractPrologTerm atom = struct.getElement(0);
              result = atom != null && atom.getPriority() <= getPriority();
            }
            break;
            default:
              throw new CriticalUnexpectedError();
          }
          break;
        case 2:
          switch (opType) {
            case XFY:
            case XFX:
            case YFX:
              final AbstractPrologTerm elementLeft = struct.getElement(0);
              final AbstractPrologTerm elementRight = struct.getElement(1);

              if (elementLeft == null || elementRight == null) {
                result = false;
              } else {

                switch (opType) {
                  case XFX:
                    result = elementLeft.getPriority() < getPriority()
                        && elementRight.getPriority() < getPriority();
                    break;
                  case YFX:
                    result = elementLeft.getPriority() <= getPriority()
                        && elementRight.getPriority() < getPriority();
                    break;
                  case XFY:
                    result = elementLeft.getPriority() < getPriority()
                        && elementRight.getPriority() <= getPriority();
                    break;
                  default:
                    result = false;
                    break;
                }
              }
              break;

            case XF:
            case FX: {
              final AbstractPrologTerm atom = struct.getElement(opType == OperatorType.XF ? 0 : 1);
              result = atom != null && atom.getPriority() < getPriority();
            }
            break;
            case YF:
            case FY: {
              final AbstractPrologTerm atom = struct.getElement(opType == OperatorType.YF ? 0 : 1);
              result = atom != null && atom.getPriority() <= getPriority();

            }
            break;
            default:
              throw new CriticalUnexpectedError();
          }

          break;
        default:
          result = false;
          break;
      }
    } else {
      result = false;
    }
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode() {
    return precalculatedHashCode;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean equals(final Object obj) {
    boolean result = false;

    if (obj != null) {
      if (this == obj) {
        result = true;
      } else {
        if (obj instanceof Operator) {
          final Operator op = (Operator) obj;
          if (op.precalculatedHashCode == precalculatedHashCode
              && text.equals(op.text)
              && opPriority == op.opPriority
              && opType == op.opType) {
            result = true;
          }
        }
      }
    }

    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return String.format("op(%d,%s,'%s').", getPriority(), getOperatorType().toString().toLowerCase(Locale.ENGLISH), getText());
  }

  // The method makes all system operators as singletons for serialization, but only system ones!
  private Object readResolve() {
    final Object result = AbstractPrologParser.findSystemOperatorForNameAndType(text, opType);
    return result == null ? this : result;
  }
}
