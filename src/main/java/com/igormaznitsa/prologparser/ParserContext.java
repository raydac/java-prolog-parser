/*
 * Copyright 2011 Igor Maznitsa (http://www.igormaznitsa.com)
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of version 3 of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307  USA
 */
package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.terms.PrologStructure;

/**
 * The interface describes a parser context which is used by a parser to
 * recognize non-system operators and some special information, also the context
 * will be notified about new structure creation
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.00
 */
public interface ParserContext {
	/**
	 * Check that there is any operator in the context which starts with a
	 * string (or even whole name equals a string)
	 * 
	 * @param operatorNameStartSubstring
	 *            the string which should be checked to be as the start as an
	 *            operator name (or whole operator name too)
	 * @return true is there is an operator starts with the string, else false
	 * @since 1.00
	 */
	boolean hasOperatorStartsWith(String operatorNameStartSubstring);

	/**
	 * Find an operator container in the context which name equals a string
	 * 
	 * @param operatorName
	 *            a string to be used as whole operator name
	 * @return an operator container if there is one for the name, else null
	 * @since 1.00
	 */
	OperatorContainer findOperatorForName(String operatorName);

	/**
	 * The method is being called when a parser wants to check that there is a
	 * zero-arity predicate for a name, if there is one then the parser will
	 * create a zero-arity structure for it instead just an atom
	 * 
	 * @param predicateName
	 *            a string contains the predicate name
	 * @return true if there is such predicate, else false
	 * @since 1.00
	 */
	boolean hasZeroArityPredicate(String predicateName);

	/**
	 * It will be called by parser every time as it has created a structure and
	 * the context can postprocess it
	 * 
	 * @param structure
	 *            the structure just created by the parser
	 * @since 1.00
	 */
	void processNewStructure(PrologStructure structure);
}
