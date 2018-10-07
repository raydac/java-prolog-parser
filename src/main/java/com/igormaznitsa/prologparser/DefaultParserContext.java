package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.operators.Op;
import com.igormaznitsa.prologparser.operators.OpContainer;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.tokenizer.PrologParser;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class DefaultParserContext implements ParserContext {

  protected final Set<String> opPrefixes = new HashSet<>();
  protected final Map<String, OpContainer> opContainers = new HashMap<>();
  protected final Set<String> zeroArityStructs = new HashSet<>();

  public DefaultParserContext() {
  }

  protected void fillPrefixes(final String name) {
    for (int i = 1; i < name.length(); i++) {
      this.opPrefixes.add(name.substring(0, i));
    }
  }

  public DefaultParserContext addZeroArityStructs(final String... names) {
    for (final String name : names) {
      this.zeroArityStructs.add(name);
    }
    return this;
  }

  public DefaultParserContext addOperators(final Op... operators) {
    for (final Op op : operators) {
      fillPrefixes(op.getText());
      OpContainer container = this.opContainers.get(op.getText());
      if (container == null) {
        container = OpContainer.newOpCont(op);
        this.opContainers.put(op.getText(), container);
      } else {
        container.addOp(op);
      }
    }
    return this;
  }

  @Override
  public void onStructureCreated(final PrologParser source, final PrologStruct struct) {
  }

  @Override
  public boolean hasZeroArityStruct(final PrologParser source, String atomName) {
    return this.zeroArityStructs.contains(atomName);
  }

  @Override
  public boolean hasOperatorStartsWith(final PrologParser source, String operatorNameStartSubstring) {
    return this.opPrefixes.contains(operatorNameStartSubstring);
  }

  @Override
  public OpContainer findOperatorForName(final PrologParser source, String operatorName) {
    return this.opContainers.get(operatorName);
  }
}
