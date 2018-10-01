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
public final class ArrayListCache<T> {
  private static final int INITIAL_ARRAY_LIST_SIZE = 32;
  private static final int MAX_CACHED_NUMBER = 64;
  private final List<T>[] insideList;

  private int pointer = 0;

  @SuppressWarnings("unchecked")
  public ArrayListCache() {
    insideList = (List<T>[]) new List<?>[MAX_CACHED_NUMBER];
    IntStream.range(0, MAX_CACHED_NUMBER)
        .forEach(x -> this.insideList[x] = new ArrayList<T>(INITIAL_ARRAY_LIST_SIZE));
  }

  @SuppressWarnings("unchecked")
  public List<T> getListFromCache() {
    if (this.pointer == 0) {
      return new ArrayList<>(INITIAL_ARRAY_LIST_SIZE);
    } else {
      return this.insideList[--this.pointer];
    }
  }

  public void putListToCache(final List<T> list) {
    if (this.pointer < MAX_CACHED_NUMBER) {
      list.clear();
      this.insideList[this.pointer++] = list;
    }
  }
}

