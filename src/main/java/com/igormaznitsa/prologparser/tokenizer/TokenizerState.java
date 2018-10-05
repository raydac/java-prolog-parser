package com.igormaznitsa.prologparser.tokenizer;

enum TokenizerState {

  LOOKFOR,
  ATOM,
  STRING,
  OPERATOR,
  VAR,
  INTEGER,
  FLOAT
}
