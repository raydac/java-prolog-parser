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

package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.terms.OpContainer;

/**
 * Context which provides flags and operator storage for a prolog parser.
 *
 * @see PrologParser
 */
@SuppressWarnings("GrazieInspection")
public interface ParserContext {
  /**
   * Empty flags, no any flag defined.
   */
  int FLAG_NONE = 0;
  /**
   * Flag allows block comments.
   */
  int FLAG_BLOCK_COMMENTS = 1;
  /**
   * Flag allows construction 0'{char} to get char code.
   * Example: 0'a, 0'\r
   */
  int FLAG_ZERO_QUOTATION_CHARCODE = 2;
  /**
   * Flag allows variable as structure functor.
   * Example: Var(a,b,c).
   */
  int FLAG_VAR_AS_FUNCTOR = 4;
  /**
   * Flag allows zero-arity structures.
   * Example: a().
   */
  int FLAG_ZERO_STRUCT = 8;
  /**
   * Flag allows blocks bounded by curly brackets.
   * Example: {a,{b,!,c}}
   */
  int FLAG_CURLY_BRACKETS = 16;
  /**
   * Recognize '.'(_,_) as a list term.
   */
  int FLAG_DOT2_AS_LIST = 32;
  /**
   * White-space char is allowed in zero quotation mode.
   *
   * @since 2.0.2
   */
  int FLAG_ZERO_QUOTATION_ALLOWS_WHITESPACE_CHAR = 64;
  /**
   * Make notifications about parsed commentaries and provide them as atoms.
   *
   * @see TokenizedCommentListener
   * @since 2.2.0
   */
  int FLAG_COMMENTS_AWARENESS = 128;

  /**
   * Check that the context contains an operator starts with some string.
   *
   * @param source     source prolog parser making request, must not be null
   * @param namePrefix string to be used to look for operator starts with it, must not be null
   * @return true if there is such operator, false otherwise
   */
  boolean hasOpStartsWith(PrologParser source, String namePrefix);

  /**
   * Find operators for their name.
   *
   * @param source source prolog parser making request, must not be null
   * @param name   name of operators, must not be null
   * @return operator container if such one is found, null otherwise
   */
  OpContainer findOpForName(PrologParser source, String name);

  /**
   * Get maximum allowed length value for internal parser text buffers.
   *
   * @return maximum allowed text parser buffers length in chars
   */
  default int getMaxTokenizerBufferLength() {
    return Integer.MAX_VALUE;
  }

  /**
   * get parser flags for the parser context.
   *
   * @return flags as bit field
   * @see ParserContext#FLAG_NONE
   * @see ParserContext#FLAG_VAR_AS_FUNCTOR
   * @see ParserContext#FLAG_BLOCK_COMMENTS
   * @see ParserContext#FLAG_CURLY_BRACKETS
   * @see ParserContext#FLAG_ZERO_QUOTATION_CHARCODE
   * @see ParserContext#FLAG_ZERO_STRUCT
   */
  int getFlags();
}
