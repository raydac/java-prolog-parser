package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.operators.OperatorType;

import static com.igormaznitsa.prologparser.operators.OperatorDef.of;

public class EdinburghPrologParser extends GenericPrologParser {

  static {
    registerAsSystemOperators(of(1200, OperatorType.XFX, ":-", "-->"),
        of(1200, OperatorType.FX, "?-", ":-"),
        of(1100, OperatorType.XFY, ";"),
        of(1150, OperatorType.XFY, "->"),
        of(900, OperatorType.FY, "\\+"),
        of(700, OperatorType.XFX, "=", "\\=", "==", "\\==", "@<", "@=<", "@>", "@>+", "=..", "is", "=:=", "=\\=", "<", "=<", ">", ">="),
        of(500, OperatorType.YFX, "+", "-", "/\\", "\\/"),
        of(400, OperatorType.YFX, "*", "/", "//", "<<", ">>", "rem", "mod"),
        of(200, OperatorType.XFX, "**"),
        of(200, OperatorType.XFY, "^"),
        of(200, OperatorType.FY, "-"),
        of(200, OperatorType.FY, "\\"));
  }

  public EdinburghPrologParser(final ParserContext context) {
    super(context);
  }
}
