package com.igormaznitsa.prologparser;

public enum TokenizerState {

  LOOKFOR,
  ATOM,
  STRING,
  OPERATOR,
  VAR,
  INTEGER,
  FLOAT
}
