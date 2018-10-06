package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.operators.OpType;

import java.io.Reader;

import static com.igormaznitsa.prologparser.operators.OpDef.op;

public class EdinburghPrologParser extends GenericPrologParser {

  static {
    registerSysOp(op(1200, OpType.XFX, ":-", "-->"),
        op(1200, OpType.FX, "?-", ":-"),
        op(1100, OpType.XFY, ";"),
        op(1150, OpType.XFY, "->"),
        op(900, OpType.FY, "\\+"),
        op(700, OpType.XFX, "=", "\\=", "==", "\\==", "@<", "@=<", "@>", "@>+", "=..", "is", "=:=", "=\\=", "<", "=<", ">", ">="),
        op(500, OpType.YFX, "+", "-", "/\\", "\\/"),
        op(400, OpType.YFX, "*", "/", "//", "<<", ">>", "rem", "mod"),
        op(200, OpType.XFX, "**"),
        op(200, OpType.XFY, "^"),
        op(200, OpType.FY, "-"),
        op(200, OpType.FY, "\\"));
  }

  public EdinburghPrologParser(final Reader reader, final ParserContext context) {
    super(reader, context);
  }

  public EdinburghPrologParser(final Reader reader) {
    this(reader, null);
  }
}
