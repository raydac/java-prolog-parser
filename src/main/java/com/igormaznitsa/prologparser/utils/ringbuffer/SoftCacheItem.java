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

/**
 * The interface describes an item to be saved in a SoftCache.
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @see SoftCache
 */
public interface SoftCacheItem {

  /**
   * Set the SoftCache owns the item.
   *
   * @param softCache a SoftCache to be the owner, it must not be null.
   */
  void setSoftCache(SoftCache<? extends SoftCacheItem> softCache);

  /**
   * Reset data in the item.
   */
  void reset();

  /**
   * Dispose the item.
   */
  void dispose();
}
