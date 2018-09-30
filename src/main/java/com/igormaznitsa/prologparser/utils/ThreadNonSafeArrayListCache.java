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

package com.igormaznitsa.prologparser.utils;


import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

/**
 * An Auxiliary Thread-NONsafe class allows to cache ArrayLists
 *
 * @param <T> class type to be processed by cache
 */
public final class ThreadNonSafeArrayListCache<T> {
  /**
   * The initial cached ArrayList size
   */
  private static final int INITIAL_ARRAY_LIST_SIZE = 32;

  /**
   * The number of cached ArrayList objects
   */
  private static final int MAX_CACHED_NUMBER = 64;

  /**
   * Inside blocking queue to keep cached lists
   */
  private final Object[] insideList = new Object[MAX_CACHED_NUMBER];

  private int firstFreeElementPointer = 0;

  public ThreadNonSafeArrayListCache() {
    // init cached items
    IntStream.range(0, MAX_CACHED_NUMBER)
        .forEach(x -> this.insideList[x] = new ArrayList<T>(INITIAL_ARRAY_LIST_SIZE));
  }

  @SuppressWarnings("unchecked")
  public List<T> getListFromCache() {
    if (this.firstFreeElementPointer == 0) {
      return new ArrayList<>(INITIAL_ARRAY_LIST_SIZE);
    } else {
      return (List<T>) this.insideList[--this.firstFreeElementPointer];
    }
  }

  public void putListToCache(final List<T> list) {
    if (this.firstFreeElementPointer < MAX_CACHED_NUMBER) {
      list.clear();
      this.insideList[this.firstFreeElementPointer++] = list;
    }
  }
}

