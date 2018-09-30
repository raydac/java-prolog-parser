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

package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.operators.OperatorContainer;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * A wrapper to keep single char mapped operator containers.
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 */
final class SingleCharOperatorContainerMap {

  /**
   * Inside wrapped map.
   */
  private final Map<String, OperatorContainer> insideMap = new HashMap<String, OperatorContainer>();
  /**
   * Array keeping operator containers for chars with codes lesser than 256.
   */
  private final OperatorContainer[] charMap = new OperatorContainer[0x100];

  /**
   * A Constructor.
   */
  SingleCharOperatorContainerMap() {
  }

  /**
   * A Constructor.
   *
   * @param containers containers to be added into the map.
   */
  SingleCharOperatorContainerMap(final OperatorContainer... containers) {
    for (final OperatorContainer c : containers) {
      put(c.getText(), c);
    }
  }

  /**
   * Check that there is a mapped operator container for the key.
   *
   * @param key a single char string.
   * @return true if there is a mapped operator container for the key, false otherwise.
   */
  public boolean containsKey(final String key) {
    if (key.length() != 1) {
      return false;
    }
    final int chr = key.charAt(0);
    return chr <= 0xFF && charMap[chr] != null;
  }

  /**
   * Put a mapped operator container for its key.
   *
   * @param key       a single char string to be used as the key for the container, it must not be null.
   * @param container a container to be mapped by the key, it must not be null.
   */
  public void put(final String key, final OperatorContainer container) {
    if (key.length() != 1) {
      throw new IllegalArgumentException("A Wrong key [" + key + ']');
    }

    final int chr = key.charAt(0);
    if (chr > 0xFF) {
      throw new IllegalArgumentException("The char code is greater than 0xFF");
    }

    charMap[chr] = container;
    insideMap.put(key, container);
  }

  /**
   * Get a mapped operator container for its key.
   *
   * @param key the key for a desired operator container, it must not be null.
   * @return null if there is not any mapped operator container for the key, a mapped container otherwise
   */
  public OperatorContainer get(final String key) {
    if (key.length() != 1) {
      return null;
    }

    final int code = key.charAt(0);

    if (code > 0xFF) {
      return null;
    }
    return charMap[code];
  }

  public OperatorContainer get(final char c) {
    return c > 0xFF ? null : charMap[c];
  }

  /**
   * Get mapped containers as a map.
   *
   * @return a map contains all mapped containers and their keys.
   */
  public Map<String, OperatorContainer> getMap() {
    return Collections.unmodifiableMap(insideMap);
  }

  /**
   * Clear the map.
   */
  public void clear() {
    insideMap.clear();
    Arrays.fill(charMap, null);
  }
}
