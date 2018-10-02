package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.terms.PrologStructure;

public interface ParserContext {

  boolean hasOperatorStartsWith(GenericPrologParser source, String operatorNameStartSubstring);

  OperatorContainer findOperatorForName(GenericPrologParser source, String operatorName);

  boolean hasZeroArityPredicate(GenericPrologParser source, String predicateName);

  void processNewStructure(GenericPrologParser source, PrologStructure structure);
}
