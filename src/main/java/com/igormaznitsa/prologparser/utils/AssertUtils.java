package com.igormaznitsa.prologparser.utils;

public final class AssertUtils {
  private AssertUtils() {
  }

  public static <T> T assertNotNull(final T value) {
    if (value == null) {
      throw new NullPointerException();
    }
    return value;
  }

  public static String assertStringNotNullAndNotEmpty(final String value) {
    if (assertNotNull(value).isEmpty()) {
      throw new IllegalArgumentException("String is empty");
    }
    return value;
  }
}
