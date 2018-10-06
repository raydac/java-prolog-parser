package com.igormaznitsa.prologparser.utils;

public final class StringBuilderEx {

  private final StringBuilder stringBuilder;

  public StringBuilderEx(final String initialString) {
    this.stringBuilder = new StringBuilder(initialString);
  }

  public StringBuilderEx(final int capacity) {
    this.stringBuilder = new StringBuilder(capacity);
  }

  public StringBuilderEx append(final char chr) {
    this.stringBuilder.append(chr);
    return this;
  }

  public int length() {
    return this.stringBuilder.length();
  }

  public char charAt(final int position) {
    return this.stringBuilder.charAt(position);
  }

  public int lastIndexOf(final char chr) {
    for (int i = this.stringBuilder.length() - 1; i >= 0; i--) {
      if (this.stringBuilder.charAt(i) == chr) {
        return i;
      }
    }
    return -1;
  }

  public boolean isSingleChar(final char chr) {
    return this.stringBuilder.length() == 1 && this.stringBuilder.charAt(0) == chr;
  }

  public boolean isLastChar(final char chr) {
    final int length = this.stringBuilder.length();
    return length > 0 && this.stringBuilder.charAt(length - 1) == chr;
  }

  public String toStringExcludeLastChar() {
    return this.stringBuilder.substring(0, this.stringBuilder.length() - 1);
  }

  public boolean hasSeveralChars() {
    return this.stringBuilder.length() > 1;
  }

  public char getLastChar() {
    return this.stringBuilder.charAt(this.stringBuilder.length() - 1);
  }

  public String substring(final int start) {
    return this.stringBuilder.substring(start);
  }

  public String substring(final int start, final int end) {
    return this.stringBuilder.substring(start, end);
  }

  public StringBuilderEx append(final String str) {
    this.stringBuilder.append(str);
    return this;
  }

  public void clear() {
    this.stringBuilder.setLength(0);
  }

  @Override
  public String toString() {
    return this.stringBuilder.toString();
  }

  public void push(final char chr) {
    this.stringBuilder.append(chr);
  }

  public char pop() {
    final char chr = this.stringBuilder.charAt(this.stringBuilder.length() - 1);
    this.stringBuilder.setLength(this.stringBuilder.length() - 1);
    return chr;
  }

  public boolean isEmpty() {
    return this.stringBuilder.length() == 0;
  }

}
