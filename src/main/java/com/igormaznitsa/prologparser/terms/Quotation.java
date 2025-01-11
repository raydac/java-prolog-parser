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

package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.utils.StringUtils;
import java.util.List;

/**
 * Type of quotation for prolog term.
 */
public enum Quotation {
  /**
   * Term doesn't have any quotation.
   */
  NONE("%s", true),
  /**
   * Term is single quotation
   * example: 'hello'
   */
  SINGLE("'%s'", true),
  /**
   * Term is double quotation
   * example: "hello"
   */
  DOUBLE("\"%s\"", true),
  /**
   * Term is back tick quotation
   * example: `hello`
   */
  BACK_TICK("`%s`", true),
  /**
   * Special variant shows that content is line comment
   *
   * @since 2.2.0
   */
  COMMENT_LINE("%%%s", false),
  /**
   * Special variant shows that content is block comment
   *
   * @since 2.2.0
   */
  COMMENT_BLOCK("/*%s*/", false);

  private final String formatPattern;
  private final boolean escapeString;
  public static final List<Quotation> VALUES = List.of(Quotation.values());

  Quotation(final String formatPattern, final boolean escapeString) {
    this.formatPattern = formatPattern;
    this.escapeString = escapeString;
  }

  /**
   * Check that string should be escaped.
   *
   * @return true if escape string required, false otherwise
   * @since 2.2.0
   */
  public boolean isEscapeString() {
    return this.escapeString;
  }

  /**
   * Get quotation mark.
   *
   * @return the quotation mark as string
   * @since 2.2.0
   */
  public String getFormatPattern() {
    return this.formatPattern;
  }

  /**
   * Quote string.
   *
   * @param str string to be quoted, can be null
   * @return quoted string
   */
  public String formatString(String str) {
    final String nonNullStr = str == null ? "" : str;
    return String.format(this.formatPattern,
        this.escapeString ? StringUtils.escapeString(nonNullStr, this) : nonNullStr);
  }
}
