package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.Op;
import com.igormaznitsa.prologparser.operators.OpType;
import com.igormaznitsa.prologparser.terms.PrologNumeric;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.terms.TermType;

final class TreeItem {

  private final PrologTerm savedTerm;
  private final PrologParser parser;
  private TreeItem leftBranch;
  private TreeItem rightBranch;
  private TreeItem parentItem;
  private boolean insideBrakes;

  TreeItem(final PrologParser parser, final PrologTerm term, final boolean inBrakes, final int line, final int pos) {
    this.parser = parser;

    if (term == null) {
      this.savedTerm = null;
    } else {
      final TermType termType = term.getType();
      if (termType == TermType.OPERATOR || termType == TermType.OPERATORS) {
        final TermWrapper termWrapper = new TermWrapper(term);
        savedTerm = termWrapper;
      } else {
        savedTerm = term;
      }
      savedTerm.setPos(pos);
      savedTerm.setLine(line);
    }
    this.insideBrakes = inBrakes;
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
    if (item.getType() == TermType.OPERATOR && item.getPriority() != 0) {
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

  TermType getType() {
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
    return ((Op) ((TermWrapper) savedTerm).getTerm()).getOpType();
  }

  private boolean validate() {
    if (savedTerm.getType() == TermType.OPERATOR) {
      final int priority = getPriority();
      final Op wrappedOperator = (Op) ((TermWrapper) savedTerm).getTerm();
      switch (wrappedOperator.getOpType()) {
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

  PrologTerm convertTreeItemIntoTerm() {
    PrologTerm result;

    switch (savedTerm.getType()) {
      case OPERATOR: {
        final TermWrapper wrapper = (TermWrapper) this.savedTerm;
        if (this.leftBranch == null && this.rightBranch == null) {
          // it is an atom because it has not any arguments
          return new PrologAtom(wrapper.getTerm().getText(), wrapper.getPos(), wrapper.getLine());
        }

        if (!validate()) {
          throw new PrologParserException("Wrong operator [" + wrapper.getText() + ']', wrapper.getLine(), wrapper.getPos());
        }

        final PrologTerm left = this.leftBranch == null ? null : this.leftBranch.convertTreeItemIntoTerm();
        final PrologTerm right = this.rightBranch == null ? null : this.rightBranch.convertTreeItemIntoTerm();
        if (left == null && right == null) {
          throw new PrologParserException("Op without operands", wrapper.getLine(), wrapper.getPos());
        }
        // this code replaces '-'(number) to '-number'
        if (right instanceof PrologNumeric && "-".equals(wrapper.getText()) && left == null && right.getType() == TermType.ATOM) {
          result = ((PrologNumeric) right).neg();
          break;
        }

        final PrologStruct operatorStruct;
        if (left == null) {
          operatorStruct = new PrologStruct(wrapper.getTerm(), new PrologTerm[] {right}, wrapper.getLine(), wrapper.getPos());
        } else {
          operatorStruct = new PrologStruct(wrapper.getTerm(), right == null ? new PrologTerm[] {left} : new PrologTerm[] {left, right},wrapper.getLine(), wrapper.getPos());
        }
        result = operatorStruct;

        if (this.parser.getContext()!=null) {
          this.parser.getContext().onStructureCreated(this.parser, operatorStruct);
        }

      }
      break;
      case STRUCT: {
        if (this.parser.getContext()!=null) {
          this.parser.getContext().onStructureCreated(parser, (PrologStruct) this.savedTerm);
        }
        result = this.savedTerm;
      }
      break;
      default: {
        result = this.savedTerm;
      }
      break;
    }

    return result;
  }

}
