package com.igormaznitsa.prologparser.utils;

import java.util.function.Supplier;

public class SoftObjectPool<T> {

  private final T[] pool;
  private final int maxSize;
  private Supplier<T> factory;
  private int size;

  public SoftObjectPool(final int size) {
    this.maxSize = size;
    this.pool = (T[]) new Object[size];
    this.size = 0;
  }

  public void fill() {
    while (this.size < this.maxSize) {
      this.push(this.factory.get());
    }
  }

  public void setFactory(final Supplier<T> factory) {
    this.factory = factory;
  }

  public T get() {
    if (this.size == 0) {
      return this.factory.get();
    } else {
      return this.pool[--size];
    }
  }

  public void push(final T object) {
    if (this.size < this.maxSize) {
      this.pool[this.size++] = object;
    }
  }
}
