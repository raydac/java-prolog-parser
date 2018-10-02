package com.igormaznitsa.prologparser.utils.ringbuffer;

import java.util.function.Supplier;

import static java.util.stream.IntStream.range;

public class SoftCache<T extends SoftCacheItem> implements Supplier<T> {

  private final Supplier<T> factory;
  private final SoftCacheItem[] buffer;
  private final int limit;
  private int head;

  @SuppressWarnings("unchecked")
  public SoftCache(final Supplier<T> factory, final int limit) {
    this.factory = factory;
    this.buffer = new SoftCacheItem[limit];
    range(0, limit).forEach(i -> {
      T x = this.factory.get();
      x.setCache(this);
      this.buffer[i] = x;
    });
    this.limit = limit;
    this.head = 0;
  }

  @Override
  @SuppressWarnings("unchecked")
  public T get() {
    T result;
    int pointer = this.head - 1;
    if (pointer < 0) {
      // create new one
      result = factory.get();
      result.setCache(this);
    } else {
      result = (T) this.buffer[pointer];
      this.head = pointer;
    }
    return result;
  }

  @SuppressWarnings("unchecked")
  public boolean tryPush(final SoftCacheItem item) {
    boolean result = false;
    if (this.head < this.limit) {
      item.reset();
      this.buffer[this.head++] = item;
      result = true;
    }
    return result;
  }
}
