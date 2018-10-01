/*
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.igormaznitsa.prologparser.utils.ringbuffer;

import java.util.function.Supplier;

import static java.util.stream.IntStream.range;

/**
 * The class implements a ring buffer allows to cache some items. But also the
 * buffer creates new items if there is not any free one for a request. It is
 * soft one because it can lost items if it is full and generate new ones if it
 * is empty.
 * NB! NOT THREAD SAFE!
 *
 * @param <T> the type of cached element
 */
public class SoftCache<T extends SoftCacheItem> implements Supplier<T> {

  private final Supplier<T> factory;
  private final SoftCacheItem[] buffer;
  private final int limit;
  private int head;

  /**
   * The constructor.
   *
   * @param factory factory to create new items, it must not be null.
   * @param limit   max allowed number of cached elements
   */
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

  /**
   * Get an item.
   *
   * @return a cached item if it is detected in the cache or a new one.
   */
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

  /**
   * Try push element into cach if there is place
   *
   * @param item item to be pushed.
   * @return true if element is pushed or false otherwise
   */
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
