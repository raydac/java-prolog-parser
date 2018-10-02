package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.utils.StrBuffer;
import com.igormaznitsa.prologparser.utils.StringUtils;

public final class PrologAtom extends AbstractPrologTerm {

  private static final long serialVersionUID = -1859006002358498466L;

  public PrologAtom(final String text) {
    super(text);
  }

  public PrologAtom(final AbstractPrologTerm term) {
    super(term.getText(), term.getStrPosition(), term.getLineNumber());
  }

  public PrologAtom(final String text, final int strPos, final int lineNumber) {
    super(text, strPos, lineNumber);
  }

  @Override
  public PrologTermType getType() {
    return PrologTermType.ATOM;
  }

  @Override
  public int getPrecedence() {
    return 0;
  }

  @Override
  public String toString() {
    final String escaped = StringUtils.escapeString(text);
    return new StrBuffer(escaped.length() + 2).append('\'').append(escaped).append('\'').toString();
  }
}
