package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.operators.OpContainer;
import com.igormaznitsa.prologparser.terms.PrologStructure;
import com.igormaznitsa.prologparser.tokenizer.PrologParser;

public interface ParserContext {

  boolean hasOperatorStartsWith(PrologParser source, String namePrefix);

  OpContainer findOperatorForName(PrologParser source, String name);

  boolean hasZeroArityPredicate(PrologParser source, String name);

  void onStructureCreated(PrologParser source, PrologStructure struct);
}
