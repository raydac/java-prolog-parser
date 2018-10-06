package com.igormaznitsa.prologparser.exceptions;

public class CriticalUnexpectedError extends Error {
  private static final long serialVersionUID = -8219655356191420973L;

  public CriticalUnexpectedError() {
    super("Critical unexpected software defect, contact developer please!");
  }
}
