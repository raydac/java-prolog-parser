package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.operators.OperatorContainer;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static java.util.Arrays.stream;

final class SingleCharOperatorContainerMap {

  private final Map<String, OperatorContainer> insideMap = new HashMap<>();
  private final OperatorContainer[] charMap = new OperatorContainer[0x80];

  SingleCharOperatorContainerMap() {
  }

  SingleCharOperatorContainerMap(final OperatorContainer... containers) {
    stream(containers).forEach(x -> put(x.getText(), x));
  }

  public boolean containsKey(final String key) {
    if (key.length() != 1) {
      return false;
    }
    final int chr = key.charAt(0);
    return chr < 0x80 && charMap[chr] != null;
  }

  public void put(final String key, final OperatorContainer container) {
    if (key.length() != 1) {
      throw new IllegalArgumentException("A Wrong key [" + key + ']');
    }

    final int chr = key.charAt(0);
    if (chr > 0x7F) {
      throw new IllegalArgumentException("The char code is greater than 0x7F");
    }

    charMap[chr] = container;
    insideMap.put(key, container);
  }

  public OperatorContainer get(final String key) {
    if (key.length() != 1) {
      return null;
    }

    final int code = key.charAt(0);

    if (code > 0x7F) {
      return null;
    }
    return charMap[code];
  }

  public OperatorContainer get(final char c) {
    return c > 0x7F ? null : charMap[c];
  }

  public Map<String, OperatorContainer> getMap() {
    return Collections.unmodifiableMap(insideMap);
  }

  public void clear() {
    insideMap.clear();
    Arrays.fill(charMap, null);
  }
}
