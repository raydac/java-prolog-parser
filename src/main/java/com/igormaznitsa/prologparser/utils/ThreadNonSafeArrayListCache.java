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
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

/**
 * An Auxiliary Thread-NONsafe class allows to cache ArrayLists
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public final class ThreadNonSafeArrayListCache <T> {
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
    for (int i = 0; i < MAX_CACHED_NUMBER; i++) {
      insideList[i] = new ArrayList<T>(INITIAL_ARRAY_LIST_SIZE);
    }
  }

  @SuppressWarnings("unchecked")
  public List<T> getListFromCache() {
    if (firstFreeElementPointer == 0){
      return new ArrayList<T>(INITIAL_ARRAY_LIST_SIZE);
    }else{
      return (List<T>)insideList[--firstFreeElementPointer];
    }
  }
  
  public void putListToCache(final List<T> list){
    if (firstFreeElementPointer < MAX_CACHED_NUMBER){
      list.clear();
      insideList[firstFreeElementPointer++] = list;
    }
  }
}
