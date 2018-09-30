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

/**
 * This enumeration contains all inside states for the prolog tokenizer
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @see PrologTokenizer
 */
public enum TokenizerState {

  /**
   * Looking for next token.
   */
  LOOKFOR,
  /**
   * Reading atom.
   */
  ATOM,
  /**
   * Reading string.
   */
  STRING,
  /**
   * Reading operator.
   */
  OPERATOR,
  /**
   * Reading variable.
   */
  VARIABLE,
  /**
   * Reading integer number.
   */
  INTEGER,
  /**
   * Reading float number.
   */
  FLOAT
}
