/* 
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.exceptions.CriticalSoftwareDefectError;
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

/**
 * Inside auxiliary class represents a tree item
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 */
public final class ParserTreeItem implements SoftCacheItem {

  private SoftCache<ParserTreeItem> ringBuffer;
  /**
   * The term saved by the item.
   */
  private AbstractPrologTerm savedTerm;
  /**
   * The left branch of the tree item.
   */
  private ParserTreeItem leftBranch;
  /**
   * The right branch of the item.
   */
  private ParserTreeItem rightBranch;
  /**
   * The link to the owner of the item.
   */
  private ParserTreeItem parentItem;
  /**
   * The term has been placed into brakes.
   */
  private boolean insideBrakes;
  /**
   * The link to the owner parser for the item.
   */
  private final AbstractPrologParser parser;

  /**
   * A Constructor.
   *
   * @param parser the parser processes the item, it must not be null
   */
  ParserTreeItem(final AbstractPrologParser parser) {
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

  /**
   * Inside method to set data to the item.
   *
   * @param prologTermWrapperCache the cache of prolog term wrappers, it can be
   * null only if the term is null
   * @param term the term
   * @param insideBrakes the flag shows that it is into inside brakes
   * @param lineNum the line number
   * @param strPos the string position
   */
  void setData(final SoftCache<PrologTermWrapper> prologTermWrapperCache, final AbstractPrologTerm term, final boolean insideBrakes, final int lineNum, final int strPos) {
    if (term == null) {
      this.savedTerm = null;
    }
    else {
      final PrologTermType termType = term.getType();
      if (termType == PrologTermType.OPERATOR || termType == PrologTermType.OPERATORS) {
        final PrologTermWrapper termWrapper = prologTermWrapperCache.get();
        termWrapper.setWrappedTerm(term);
        savedTerm = termWrapper;
      }
      else {
        savedTerm = term;
      }
      savedTerm.setStrPosition(strPos);
      savedTerm.setLineNumber(lineNum);
    }
    this.insideBrakes = insideBrakes;
  }

  /**
   * Get the priority of the term.
   *
   * @return the priority value of the term
   */
  int getPriority() {
    int result = 0;
    if (!insideBrakes) {
      result = savedTerm.getPriority();
    }
    return result;
  }

  /**
   * Set the right branch.
   *
   * @param item the right branch for the term
   */
  private void setRightBranch(final ParserTreeItem item) {
    rightBranch = item;
    if (item != null) {
      item.parentItem = this;
    }
  }

  /**
   * Make another item as the right branch.
   *
   * @param item the item which will be used as the right branch
   * @return the item which will be used as the root, it can be this item or the
   * set item (it depends on priorities)
   */
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

  /**
   * Make an other item as the left branch.
   *
   * @param item the item which will be used as the left branch
   * @return the item which will be used as the root, it can be this item or the
   * set item (it depends on priorities)
   */
  ParserTreeItem makeAsOwnerWithLeftBranch(final ParserTreeItem item) {
    this.replaceForOwner(item);
    item.setLeftBranch(this);
    return item;
  }

  /**
   * Get the right branch of the item.
   *
   * @return the right branch or null if there is not the right one
   */
  ParserTreeItem getRightBranch() {
    return rightBranch;
  }

  /**
   * Set the left branch of the item.
   *
   * @param item the left branch for the item
   */
  private void setLeftBranch(final ParserTreeItem item) {
    leftBranch = item;
    if (item != null) {
      item.parentItem = this;
    }
  }

  /**
   * Get the left branch of the item.
   *
   * @return the left branch of the item or null if there is not left one
   */
  private ParserTreeItem getLeftBranch() {
    return leftBranch;
  }

  /**
   * Get the type of the saved term in the item.
   *
   * @return the prolog term type of the term which is being saved by the item
   */
  PrologTermType getType() {
    return savedTerm.getType();
  }

  /**
   * Find the tree root of the tree where the item has been presented.
   *
   * @return the root item for the tree where the item is being, if the item is
   * the root then it will be returned
   */
  ParserTreeItem findRoot() {
    ParserTreeItem result = this;
    while (true) {
      final ParserTreeItem theParent = result.parentItem;
      if (theParent == null) {
        break;
      }
      else {
        result = theParent;
      }
    }
    return result;
  }

  /**
   * Find the first node among the item's owners which has the same or lower
   * priority.
   *
   * @param priority the priority for the desired item
   * @return found root item which has the equal or less priority than the value
   */
  ParserTreeItem findFirstNodeWithSuchOrLowerPriority(final int priority) {
    ParserTreeItem result = this;

    while (true) {
      final ParserTreeItem itsparent = result.parentItem;
      if (itsparent == null || result.getPriority() >= priority) {
        break;
      }
      else {
        result = itsparent;
      }
    }

    return result;
  }

  /**
   * Replace the owner for the item by another item
   *
   * @param newItem the item to replace the current owner, it can be null
   */
  private void replaceForOwner(final ParserTreeItem newItem) {
    if (parentItem == null) {
      newItem.parentItem = null;
      return;
    }
    if (this == parentItem.getLeftBranch()) {
      parentItem.setLeftBranch(newItem);
    }
    else {
      parentItem.setRightBranch(newItem);
    }
  }

  /**
   * Get an operator type for the saved term which must be an operator!
   *
   * @return the operator type as a value frome the OperatorType enumeration
   */
  OperatorType getOperatorType() {
    return ((Operator) ((PrologTermWrapper) savedTerm).getWrappedTerm()).getOperatorType();
  }

  /**
   * Check the validity of the tree item for its saved term
   *
   * @return true if the tree item is valid and false if it's not
   */
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
          throw new CriticalSoftwareDefectError();
      }
    }
    else {
      return leftBranch == null && rightBranch == null;
    }
  }

  @Override
  public String toString() {
    return savedTerm.toString();
  }

  /**
   * Convert the tree item into a term.
   *
   * @return the tree item represented as a prolog term
   */
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
            operatorStruct = new PrologStructure(wrapper.getWrappedTerm(), new AbstractPrologTerm[]{right});
          }
          else {
            operatorStruct = new PrologStructure(wrapper.getWrappedTerm(), (right == null ? new AbstractPrologTerm[]{left} : new AbstractPrologTerm[]{left, right}));
          }
          operatorStruct.setStrPosition(wrapper.getStrPosition());
          operatorStruct.setLineNumber(wrapper.getLineNumber());
          if (ctxNotNull) {
            ctx.processNewStructure(parser, operatorStruct);
          }
          result = operatorStruct;
        }
        finally {
          wrapper.dispose();
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

    dispose();

    return result;
  }

  @Override
  @SuppressWarnings("unchecked")
  public void setSoftCache(final SoftCache<?> owner) {
    this.ringBuffer = (SoftCache<ParserTreeItem>) owner;
  }

  @Override
  public void dispose() {
    if (this.ringBuffer != null) {
      this.ringBuffer.dispose(this);
    }
  }
}
