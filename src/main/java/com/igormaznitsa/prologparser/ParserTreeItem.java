package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.operators.OperatorType;
import com.igormaznitsa.prologparser.terms.AbstractPrologNumericTerm;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologStructure;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import com.igormaznitsa.prologparser.utils.ringbuffer.SoftCache;
import com.igormaznitsa.prologparser.utils.ringbuffer.SoftCacheItem;

public final class ParserTreeItem implements SoftCacheItem {

  private final GenericPrologParser parser;
  private SoftCache<ParserTreeItem> ringBuffer;
  private AbstractPrologTerm savedTerm;
  private ParserTreeItem leftBranch;
  private ParserTreeItem rightBranch;
  private ParserTreeItem parentItem;
  private boolean insideBrakes;

  ParserTreeItem(final GenericPrologParser parser) {
    this.parser = parser;
    this.setData(null, null, false, -1, -1);
  }

  @Override
  public void reset() {
    this.savedTerm = null;
    this.insideBrakes = false;
    this.leftBranch = null;
    this.rightBranch = null;
    this.parentItem = null;
  }

  void setData(final SoftCache<PrologTermWrapper> prologTermWrapperCache, final AbstractPrologTerm term, final boolean insideBrakes, final int lineNum, final int strPos) {
    if (term == null) {
      this.savedTerm = null;
    } else {
      final PrologTermType termType = term.getType();
      if (termType == PrologTermType.OPERATOR || termType == PrologTermType.OPERATORS) {
        final PrologTermWrapper termWrapper = prologTermWrapperCache.get();
        termWrapper.setWrappedTerm(term);
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

  ParserTreeItem makeAsRightBranch(final ParserTreeItem item) {
    final ParserTreeItem currentSubbranch = rightBranch;
    setRightBranch(item);
    item.setLeftBranch(currentSubbranch);
    ParserTreeItem result = this;
    if (item.getType() == PrologTermType.OPERATOR && item.getPriority() != 0) {
      result = item;
    }
    return result;
  }

  ParserTreeItem makeAsOwnerWithLeftBranch(final ParserTreeItem item) {
    this.replaceForOwner(item);
    item.setLeftBranch(this);
    return item;
  }

  ParserTreeItem getRightBranch() {
    return rightBranch;
  }

  private void setRightBranch(final ParserTreeItem item) {
    rightBranch = item;
    if (item != null) {
      item.parentItem = this;
    }
  }

  private ParserTreeItem getLeftBranch() {
    return leftBranch;
  }

  private void setLeftBranch(final ParserTreeItem item) {
    leftBranch = item;
    if (item != null) {
      item.parentItem = this;
    }
  }

  PrologTermType getType() {
    return savedTerm.getType();
  }

  ParserTreeItem findRoot() {
    ParserTreeItem result = this;
    while (true) {
      final ParserTreeItem theParent = result.parentItem;
      if (theParent == null) {
        break;
      } else {
        result = theParent;
      }
    }
    return result;
  }

  ParserTreeItem findFirstNodeWithSuchOrLowerPriority(final int priority) {
    ParserTreeItem result = this;

    while (true) {
      final ParserTreeItem itsparent = result.parentItem;
      if (itsparent == null || result.getPriority() >= priority) {
        break;
      } else {
        result = itsparent;
      }
    }

    return result;
  }

  private void replaceForOwner(final ParserTreeItem newItem) {
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

  OperatorType getOperatorType() {
    return ((Operator) ((PrologTermWrapper) savedTerm).getWrappedTerm()).getOperatorType();
  }

  private boolean validate() {
    if (savedTerm.getType() == PrologTermType.OPERATOR) {
      final int priority = getPriority();
      final Operator wrappedOperator = (Operator) ((PrologTermWrapper) savedTerm).getWrappedTerm();
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
        final PrologTermWrapper wrapper = (PrologTermWrapper) savedTerm;
        PrologStructure operatorStruct;
        try {
          if (leftBranch == null && rightBranch == null) {
            // it is an atom because it has not any arguments
            return new PrologAtom(wrapper.getWrappedTerm().getText(), wrapper.getLineNumber(), wrapper.getStrPosition());
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
            operatorStruct = new PrologStructure(wrapper.getWrappedTerm(), new AbstractPrologTerm[] {right});
          } else {
            operatorStruct = new PrologStructure(wrapper.getWrappedTerm(), (right == null ? new AbstractPrologTerm[] {left} : new AbstractPrologTerm[] {left, right}));
          }
          operatorStruct.setStrPosition(wrapper.getStrPosition());
          operatorStruct.setLineNumber(wrapper.getLineNumber());
          if (ctxNotNull) {
            ctx.processNewStructure(parser, operatorStruct);
          }
          result = operatorStruct;
        } finally {
          wrapper.release();
        }
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

    release();

    return result;
  }

  @Override
  @SuppressWarnings("unchecked")
  public void setCache(final SoftCache<?> owner) {
    this.ringBuffer = (SoftCache<ParserTreeItem>) owner;
  }

  @Override
  public void release() {
    if (this.ringBuffer != null) {
      this.ringBuffer.tryPush(this);
    }
  }
}
