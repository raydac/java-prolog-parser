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
import com.igormaznitsa.prologparser.terms.PrologStructure;

/**
 * The interface describes a parser context which is used by a parser to
 * recognize non-system operators and some special information, also the context
 * will be notified about new structure creation
 */
public interface ParserContext {

    /**
     * Check that there is any operator in the context which starts with a
     * string (or even whole name equals a string)
     * @param source the calling prolog parser
     * @param operatorNameStartSubstring
     *            the string which should be checked to be as the start as an
     *            operator name (or whole operator name too)
     * @return true is there is an operator starts with the string, else false
     */
    boolean hasOperatorStartsWith(AbstractPrologParser source, String operatorNameStartSubstring);

    /**
     * Find an operator container in the context which name equals a string
     * 
     * @param source the calling prolog parser
     * @param operatorName
     *            a string to be used as whole operator name
     * @return an operator container if there is one for the name, else null
     */
    OperatorContainer findOperatorForName(AbstractPrologParser source, String operatorName);

    /**
     * The method is being called when a parser wants to check that there is a
     * zero-arity predicate for a name, if there is one then the parser will
     * create a zero-arity structure for it instead just an atom
     * @param source the calling prolog parser
     * @param predicateName
     *            a string contains the predicate name
     * @return true if there is such predicate, else false
     */
    boolean hasZeroArityPredicate(AbstractPrologParser source, String predicateName);

    /**
     * It will be called by parser every time as it has created a structure and
     * the context can postprocess it
     * @param source the calling prolog parser
     * @param structure
     *            the structure just created by the parser
     */
    void processNewStructure(AbstractPrologParser source, PrologStructure structure);
}
