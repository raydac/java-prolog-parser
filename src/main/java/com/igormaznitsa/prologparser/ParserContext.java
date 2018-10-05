package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.terms.PrologStructure;
import com.igormaznitsa.prologparser.tokenizer.PrologParser;

public interface ParserContext {

  boolean hasOperatorStartsWith(PrologParser source, String operatorNameStartSubstring);

  OperatorContainer findOperatorForName(PrologParser source, String operatorName);

  boolean hasZeroArityPredicate(PrologParser source, String predicateName);

  void processNewStructure(PrologParser source, PrologStructure structure);
}
