package com.igormaznitsa.prologparser.tokenizer;

public enum TokenizerState {

  LOOKFOR,
  ATOM,
  STRING,
  OPERATOR,
  VAR,
  INTEGER,
  FLOAT
}
