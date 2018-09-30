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

/**
 * The class describes a prolog variable.
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 */
public final class PrologVariable extends AbstractPrologTerm {
  private static final long serialVersionUID = 1058349084517573220L;
  /**
   * The variable contains the flag shows that the variable is an anonymous
   * one
   */
  private final boolean is_anonymous;

  /**
   * A Constructor. It allows to create an anonymous variable.
   */
  public PrologVariable() {
    this("_");
  }

  /**
   * A Constructor. It allows to create an anonymous variable and set the
   * source stream position
   *
   * @param strPosition the variable char string position in the source stream
   * @param lineNumber  the variable char line number in the source stream
   */
  public PrologVariable(final int strPosition, final int lineNumber) {
    this();
    setStrPosition(strPosition);
    setLineNumber(lineNumber);
  }

  /**
   * A Constructor . It allows to create a named variable (but also it can
   * create and an anonymous one if the text is '_')
   *
   * @param text the name for the new variable, it can't be null and must use
   *             the prolog syntax variable naming rules
   */
  public PrologVariable(final String text) {
    super(text);

    if (text.isEmpty()) {
      throw new IllegalArgumentException("Variable name is empty");
    }

    final char firstLetter = text.charAt(0);

    if (!Character.isUpperCase(firstLetter) && firstLetter != '_') {
      throw new IllegalArgumentException(
          "The variable name must be started from an upper case letter or '_' ["
              + text + ']');
    }

    is_anonymous = text.length() == 1 && firstLetter == '_';
  }

  /**
   * A Constructor . It allows to create a named variable (but also it can
   * create and an anonymous one if the text is '_') and set the first
   * variable char position in the source string
   *
   * @param text        the name for the new variable, it can't be null and must use
   *                    the prolog syntax variable naming rules
   * @param strPosition the first variable char string position
   * @param lineNumber  the first variable char line number
   */
  public PrologVariable(final String text, final int strPosition, final int lineNumber) {
    this(text);
    setStrPosition(strPosition);
    setLineNumber(lineNumber);
  }

  /**
   * Check that the variable is an anonymous one
   *
   * @return true if the variable is an anonymous one, else false
   */
  public boolean isAnonymous() {
    return is_anonymous;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public PrologTermType getType() {
    return PrologTermType.VAR;
  }
}
