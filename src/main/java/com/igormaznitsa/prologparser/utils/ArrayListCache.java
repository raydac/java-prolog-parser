package com.igormaznitsa.prologparser.utils;


import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

public final class ArrayListCache<T> {
  private static final int INITIAL_ARRAY_LIST_SIZE = 32;
  private static final int MAX_CACHED_NUMBER = 64;
  private final List<T>[] insideList;

  private int pointer = 0;

  @SuppressWarnings("unchecked")
  public ArrayListCache() {
    insideList = (List<T>[]) new List<?>[MAX_CACHED_NUMBER];
    IntStream.range(0, MAX_CACHED_NUMBER)
        .forEach(x -> this.insideList[x] = new ArrayList<>(INITIAL_ARRAY_LIST_SIZE));
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

