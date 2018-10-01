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

import com.igormaznitsa.prologparser.utils.StrBuffer;
import com.igormaznitsa.prologparser.utils.StringUtils;

/**
 * The class describes the main prolog type called 'atom', in some words it is
 * an immutable text part. A Prolog atom has always the top possible priority
 * (0).
 *
 * @author Igor Maznitsa
 */
public final class PrologAtom extends AbstractPrologTerm {

  private static final long serialVersionUID = -1859006002358498466L;

  /**
   * A Constructor allows to make an instance based on a text part.
   *
   * @param text the text to be used for new instance, must not be null
   */
  public PrologAtom(final String text) {
    super(text);
  }

  /**
   * A Constructor to clone a Prolog term as an atom.
   *
   * @param term a prolog term, it must not be null.
   */
  public PrologAtom(final AbstractPrologTerm term) {
    super(term.getText(), term.getStrPosition(), term.getLineNumber());
  }

  /**
   * A Constructor allows to make an instance based on a text part and set the
   * position values for the text at the source stream
   *
   * @param text       the text to be used for new instance, must not be null
   * @param strPos     the first term char string position
   * @param lineNumber the first term char line number
   */
  public PrologAtom(final String text, final int strPos, final int lineNumber) {
    super(text, strPos, lineNumber);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public PrologTermType getType() {
    return PrologTermType.ATOM;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getPriority() {
    return 0;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    final String escaped = StringUtils.escapeString(text);
    return new StrBuffer(escaped.length() + 2).append('\'').append(escaped).append('\'').toString();
  }
}
