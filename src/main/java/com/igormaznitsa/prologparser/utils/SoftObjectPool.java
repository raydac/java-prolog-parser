/*
 * Copyright (c) 2011-2018 Igor Maznitsa. All rights reserved.
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package com.igormaznitsa.prologparser.utils;

import java.util.function.Supplier;

/**
 * Auxiliary class to implement cache some objects, but if there is not any cached one then new one will be generated,
 * @param <T> type of objects.
 */
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

  /**
   * Fill the cache by pre-generated instances.
   */
  public void fill() {
    while (this.size < this.maxSize) {
      this.push(this.get());
    }
  }

  public T find() {
    if (this.size == 0) {
      return this.get();
    } else {
      return this.pool[--this.size];
    }
  }

  public void push(final T object) {
    if (this.size < this.maxSize) {
      this.pool[this.size++] = object;
    }
  }
}
