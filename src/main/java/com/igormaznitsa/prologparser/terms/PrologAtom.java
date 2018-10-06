package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.utils.StringBuilderEx;
import com.igormaznitsa.prologparser.utils.StringUtils;

public final class PrologAtom extends PrologTerm {

  private static final long serialVersionUID = -1859006002358498466L;

  public PrologAtom(final String text) {
    super(text);
  }

  public PrologAtom(final PrologTerm term) {
    super(term.getText(), term.getLine(), term.getPos());
  }

  public PrologAtom(final String text, final int line, final int pos) {
    super(text, line, pos);
  }

  @Override
  public TermType getType() {
    return TermType.ATOM;
  }

  @Override
  public int getPrecedence() {
    return 0;
  }

  @Override
  public String toString() {
    final String escaped = StringUtils.escapeString(text);
    return new StringBuilderEx(escaped.length() + 2).append('\'').append(escaped).append('\'').toString();
  }
}
