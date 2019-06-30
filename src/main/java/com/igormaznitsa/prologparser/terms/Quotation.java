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

import static com.igormaznitsa.prologparser.utils.StringUtils.escapeString;

/**
 * Type of quotation for prolog term.
 */
public enum Quotation {
  /**
   * Term doesn't have any quotation.
   */
  NONE(""),
  /**
   * Term is single quotation
   * example: 'hello'
   */
  SINGLE("\'"),
  /**
   * Term is double quotation
   * example: "hello"
   */
  DOUBLE("\""),
  /**
   * Term is back tick quotation
   * example: `hello`
   */
  BACK_TICK("`");

  private final String quotationMark;

  Quotation(final String quotationMark) {
    this.quotationMark = quotationMark;
  }

  /**
   * Get quotation mark.
   *
   * @return the quotation mark as string
   */
  public String getQuotationMark() {
    return this.quotationMark;
  }

  /**
   * Quote string.
   *
   * @param str string to be quoted, can be null
   * @return quoted string
   */
  public String quoteString(final String str) {
    return this.quotationMark + escapeString(str == null ? "" : str, this) + this.quotationMark;
  }
}
