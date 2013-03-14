/*
 * Copyright 2011-2013 Igor Maznitsa (http://www.igormaznitsa.com)
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of version 3 of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307  USA
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
