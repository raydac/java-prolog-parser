package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.operators.OperatorContainer;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

class SingleCharOperatorContainerMap {

    private final Map<String, OperatorContainer> insideMap = new HashMap<String, OperatorContainer>();
    private final OperatorContainer[] charMap = new OperatorContainer[0x100];

    SingleCharOperatorContainerMap() {
    }

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

    public Map<String, OperatorContainer> getMap() {
        return Collections.unmodifiableMap(insideMap);
    }

    public void clear() {
        insideMap.clear();
        Arrays.fill(charMap, null);
    }
}
