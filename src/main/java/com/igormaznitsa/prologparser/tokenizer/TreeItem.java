package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.OpType;
import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.terms.AbstractPrologNumericTerm;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologStructure;
import com.igormaznitsa.prologparser.terms.PrologTermType;

final class TreeItem {

  private final AbstractPrologTerm savedTerm;
  private final PrologParser parser;
  private TreeItem leftBranch;
  private TreeItem rightBranch;
  private TreeItem parentItem;
  private boolean insideBrakes;

  TreeItem(final PrologParser parser, final AbstractPrologTerm term, final boolean insideBrakes, final int lineNum, final int strPos) {
    this.parser = parser;

    if (term == null) {
      this.savedTerm = null;
    } else {
      final PrologTermType termType = term.getType();
      if (termType == PrologTermType.OPERATOR || termType == PrologTermType.OPERATORS) {
        final TermWrapper termWrapper = new TermWrapper(term);
        savedTerm = termWrapper;
      } else {
        savedTerm = term;
      }
      savedTerm.setStrPosition(strPos);
      savedTerm.setLineNumber(lineNum);
    }
    this.insideBrakes = insideBrakes;
  }

  int getPriority() {
    int result = 0;
    if (!insideBrakes) {
      result = savedTerm.getPrecedence();
    }
    return result;
  }

  TreeItem makeAsRightBranch(final TreeItem item) {
    final TreeItem currentSubbranch = rightBranch;
    setRightBranch(item);
    item.setLeftBranch(currentSubbranch);
    TreeItem result = this;
    if (item.getType() == PrologTermType.OPERATOR && item.getPriority() != 0) {
      result = item;
    }
    return result;
  }

  TreeItem makeAsOwnerWithLeftBranch(final TreeItem item) {
    this.replaceForOwner(item);
    item.setLeftBranch(this);
    return item;
  }

  TreeItem getRightBranch() {
    return rightBranch;
  }

  private void setRightBranch(final TreeItem item) {
    rightBranch = item;
    if (item != null) {
      item.parentItem = this;
    }
  }

  private TreeItem getLeftBranch() {
    return leftBranch;
  }

  private void setLeftBranch(final TreeItem item) {
    leftBranch = item;
    if (item != null) {
      item.parentItem = this;
    }
  }

  PrologTermType getType() {
    return savedTerm.getType();
  }

  TreeItem findRoot() {
    TreeItem result = this;
    while (true) {
      final TreeItem theParent = result.parentItem;
      if (theParent == null) {
        break;
      } else {
        result = theParent;
      }
    }
    return result;
  }

  TreeItem findFirstNodeWithSuchOrLowerPriority(final int priority) {
    TreeItem result = this;

    while (true) {
      final TreeItem itsparent = result.parentItem;
      if (itsparent == null || result.getPriority() >= priority) {
        break;
      } else {
        result = itsparent;
      }
    }

    return result;
  }

  private void replaceForOwner(final TreeItem newItem) {
    if (parentItem == null) {
      newItem.parentItem = null;
      return;
    }
    if (this == parentItem.getLeftBranch()) {
      parentItem.setLeftBranch(newItem);
    } else {
      parentItem.setRightBranch(newItem);
    }
  }

  OpType getOperatorType() {
    return ((Operator) ((TermWrapper) savedTerm).getTerm()).getOperatorType();
  }

  private boolean validate() {
    if (savedTerm.getType() == PrologTermType.OPERATOR) {
      final int priority = getPriority();
      final Operator wrappedOperator = (Operator) ((TermWrapper) savedTerm).getTerm();
      switch (wrappedOperator.getOperatorType()) {
        case FX:
          return leftBranch == null && (rightBranch != null && rightBranch.getPriority() < priority);
        case FY:
          return leftBranch == null && (rightBranch != null && rightBranch.getPriority() <= priority);
        case YF:
          return (leftBranch != null && leftBranch.getPriority() <= priority) && rightBranch == null;
        case XF:
          return (leftBranch != null && leftBranch.getPriority() < priority) && rightBranch == null;
        case XFX:
          return (leftBranch != null && leftBranch.getPriority() < priority) && (rightBranch != null && rightBranch.getPriority() < priority);
        case XFY:
          return (leftBranch != null && leftBranch.getPriority() < priority) && (rightBranch != null && rightBranch.getPriority() <= priority);
        case YFX:
          return (leftBranch != null && leftBranch.getPriority() <= priority) && (rightBranch != null && rightBranch.getPriority() < priority);
        default:
          throw new CriticalUnexpectedError();
      }
    } else {
      return leftBranch == null && rightBranch == null;
    }
  }

  @Override
  public String toString() {
    return savedTerm.toString();
  }

  AbstractPrologTerm convertTreeItemIntoTerm() throws PrologParserException {
    AbstractPrologTerm result;
    final ParserContext ctx = parser.getContext();
    final boolean ctxNotNull = ctx != null;
    switch (savedTerm.getType()) {
      case OPERATOR: {
        final TermWrapper wrapper = (TermWrapper) savedTerm;
        PrologStructure operatorStruct;
        if (leftBranch == null && rightBranch == null) {
          // it is an atom because it has not any arguments
          return new PrologAtom(wrapper.getTerm().getText(), wrapper.getLineNumber(), wrapper.getStrPosition());
        }

        if (!validate()) {
          throw new PrologParserException("Wrong operator [" + wrapper.getText() + ']', wrapper.getLineNumber(), wrapper.getStrPosition());
        }

        final AbstractPrologTerm left = leftBranch == null ? null : leftBranch.convertTreeItemIntoTerm();
        final AbstractPrologTerm right = rightBranch == null ? null : rightBranch.convertTreeItemIntoTerm();
        if (left == null && right == null) {
          throw new PrologParserException("Operator without operands", wrapper.getLineNumber(), wrapper.getStrPosition());
        }
        // this code replaces '-'(number) to '-number'
        if (right instanceof AbstractPrologNumericTerm && "-".equals(wrapper.getText()) && left == null && right.getType() == PrologTermType.ATOM) {
          result = ((AbstractPrologNumericTerm) right).neg();
          break;
        }
        if (left == null) {
          operatorStruct = new PrologStructure(wrapper.getTerm(), new AbstractPrologTerm[] {right});
        } else {
          operatorStruct = new PrologStructure(wrapper.getTerm(), (right == null ? new AbstractPrologTerm[] {left} : new AbstractPrologTerm[] {left, right}));
        }
        operatorStruct.setStrPosition(wrapper.getStrPosition());
        operatorStruct.setLineNumber(wrapper.getLineNumber());
        if (ctxNotNull) {
          ctx.processNewStructure(parser, operatorStruct);
        }
        result = operatorStruct;

      }
      break;
      case STRUCT: {
        if (ctxNotNull) {
          ctx.processNewStructure(parser, (PrologStructure) savedTerm);
        }
        result = savedTerm;
      }
      break;
      default: {
        result = savedTerm;
      }
      break;
    }

    return result;
  }

}