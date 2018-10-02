package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.terms.PrologStructure;
import com.igormaznitsa.prologparser.tokenizer.AbstractPrologParser;

public interface ParserContext {

  boolean hasOperatorStartsWith(AbstractPrologParser source, String operatorNameStartSubstring);

  OperatorContainer findOperatorForName(AbstractPrologParser source, String operatorName);

  boolean hasZeroArityPredicate(AbstractPrologParser source, String predicateName);

  void processNewStructure(AbstractPrologParser source, PrologStructure structure);
}
