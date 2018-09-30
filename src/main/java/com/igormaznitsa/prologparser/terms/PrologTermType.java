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

package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.annotations.PrologOperator;
import com.igormaznitsa.prologparser.annotations.PrologOperators;

/**
 * The enumeration contains all prolog term types being used by the prolog
 * parser
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 */
public enum PrologTermType {

  /**
   * The constant describes a prolog atom
   *
   * @see PrologAtom
   */
  ATOM,
  /**
   * The constant describes a prolog structure
   *
   * @see PrologStructure
   */
  STRUCT,
  /**
   * The constant describes a prolog variable
   *
   * @see PrologVariable
   */
  VAR,
  /**
   * The constant describes a prolog operator
   *
   * @see PrologOperator
   */
  OPERATOR,
  /**
   * The constant describes an operator container, it is an auxiliary object
   * to save similar named operators on the intermediate processing level
   *
   * @see PrologOperators
   */
  OPERATORS,
  /**
   * The constant describes a prolog list
   *
   * @see PrologList
   */
  LIST
}
