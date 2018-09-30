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

public final class SingleCharString {

  private static final int MAX_CODE = 0xFF;
  private static final String[] map = new String[MAX_CODE + 1];

  static {
    for (int i = 0; i <= MAX_CODE; i++) {
      map[i] = String.valueOf((char) i);
    }
  }

  public static String valueOf(final char chr) {
    return chr > MAX_CODE ? String.valueOf(chr) : map[chr];
  }
}
