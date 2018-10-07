package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.operators.OpContainer;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.tokenizer.PrologParser;

public interface ParserContext {

  boolean hasOperatorStartsWith(PrologParser source, String namePrefix);

  OpContainer findOperatorForName(PrologParser source, String name);

  boolean hasZeroArityStruct(PrologParser source, String atomName);

  void onStructureCreated(PrologParser source, PrologStruct struct);
}
