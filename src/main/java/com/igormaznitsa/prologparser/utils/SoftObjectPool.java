package com.igormaznitsa.prologparser.utils;

import java.util.function.Supplier;

public abstract class SoftObjectPool<T> implements Supplier<T> {

  private final T[] pool;
  private final int maxSize;
  private int size;

  @SuppressWarnings("unchecked")
  public SoftObjectPool(final int size) {
    this.maxSize = size;
    this.pool = (T[]) new Object[size];
    this.size = 0;
  }

  public int size() {
    return this.size;
  }

  public void fill() {
    while (this.size < this.maxSize) {
      this.push(this.get());
    }
  }

  public T findCached() {
    if (this.size == 0) {
      return this.get();
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
