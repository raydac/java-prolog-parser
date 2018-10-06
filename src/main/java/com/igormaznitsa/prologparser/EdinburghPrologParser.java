package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.operators.OpType;

import java.io.Reader;

import static com.igormaznitsa.prologparser.operators.OpDef.of;

public class EdinburghPrologParser extends GenericPrologParser {

  static {
    registerSysOp(of(1200, OpType.XFX, ":-", "-->"),
        of(1200, OpType.FX, "?-", ":-"),
        of(1100, OpType.XFY, ";"),
        of(1150, OpType.XFY, "->"),
        of(900, OpType.FY, "\\+"),
        of(700, OpType.XFX, "=", "\\=", "==", "\\==", "@<", "@=<", "@>", "@>+", "=..", "is", "=:=", "=\\=", "<", "=<", ">", ">="),
        of(500, OpType.YFX, "+", "-", "/\\", "\\/"),
        of(400, OpType.YFX, "*", "/", "//", "<<", ">>", "rem", "mod"),
        of(200, OpType.XFX, "**"),
        of(200, OpType.XFY, "^"),
        of(200, OpType.FY, "-"),
        of(200, OpType.FY, "\\"));
  }

  public EdinburghPrologParser(final Reader reader, final ParserContext context) {
    super(reader, context);
  }
}
