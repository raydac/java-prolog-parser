/*
 * Copyright (c) 2011-2018 Igor Maznitsa. All rights reserved.
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologNumeric;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.Quotation;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.tokenizer.Op;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import com.igormaznitsa.prologparser.tokenizer.TermWrapper;
import com.igormaznitsa.prologparser.utils.SoftObjectPool;

import java.util.ArrayList;

final class AstItem {

  private final SoftObjectPool<AstItem> astItemPool;

  private final PrologParser parser;
  private final SoftObjectPool<TermWrapper> termWrapperPool;
  private AstItem leftItem;
  private AstItem rightItem;
  private AstItem parentItem;
  private PrologTerm savedTerm;

  public AstItem(final PrologParser parser, final SoftObjectPool<AstItem> pool, final SoftObjectPool<TermWrapper> termWrapperPool) {
    this.parser = parser;
    this.astItemPool = pool;
    this.termWrapperPool = termWrapperPool;
  }

  public AstItem setData(final PrologTerm term, final int line, final int pos) {
    if (term == null) {
      this.replaceSavedTerm(null);
    } else {
      final TermType termType = term.getType();
      if (termType == TermType.OPERATOR || termType == TermType.SPEC_TERM_OPERATOR_CONTAINER) {
        final TermWrapper termWrapper = this.termWrapperPool.find().setWrappedTerm(term);
        this.replaceSavedTerm(termWrapper);
      } else {
        this.replaceSavedTerm(term);
      }
      this.savedTerm.setPos(pos);
      this.savedTerm.setLine(line);
    }
    return this;
  }

  private void replaceSavedTerm(final PrologTerm newValue) {
    if (this.savedTerm instanceof TermWrapper) {
      ((TermWrapper) this.savedTerm).release();
    }
    this.savedTerm = newValue;
  }

  public void release() {
    this.leftItem = null;
    this.rightItem = null;
    this.parentItem = null;

    this.replaceSavedTerm(null);

    this.astItemPool.push(this);
  }

  public int getPrecedence() {
    return this.savedTerm.getPrecedence();
  }

  public AstItem makeAsRightBranch(final AstItem item) {
    final AstItem currentSubbranch = rightItem;
    setRightBranch(item);
    item.setLeftBranch(currentSubbranch);
    AstItem result = this;
    if (item.getType() == TermType.OPERATOR && item.getPrecedence() != 0) {
      result = item;
    }
    return result;
  }

  public AstItem makeAsOwnerWithLeftBranch(final AstItem item) {
    this.replaceForOwner(item);
    item.setLeftBranch(this);
    return item;
  }

  public AstItem getRightBranch() {
    return rightItem;
  }

  private void setRightBranch(final AstItem item) {
    rightItem = item;
    if (item != null) {
      item.parentItem = this;
    }
  }

  private AstItem getLeftBranch() {
    return leftItem;
  }

  private void setLeftBranch(final AstItem item) {
    leftItem = item;
    if (item != null) {
      item.parentItem = this;
    }
  }

  public TermType getType() {
    return savedTerm.getType();
  }

  public AstItem findRoot() {
    AstItem result = this;
    while (!Thread.currentThread().isInterrupted()) {
      final AstItem theParent = result.parentItem;
      if (theParent == null) {
        break;
      } else {
        result = theParent;
      }
    }
    return result;
  }

  public AstItem findFirstNodeWithSuchOrLowerPrecedence(final int precedence) {
    AstItem result = this;

    while (!Thread.currentThread().isInterrupted()) {
      final AstItem itsParent = result.parentItem;
      if (itsParent == null || result.getPrecedence() >= precedence) {
        break;
      } else {
        result = itsParent;
      }
    }

    return result;
  }

  private void replaceForOwner(final AstItem newItem) {
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

  public OpAssoc getOpAssoc() {
    return ((Op) ((TermWrapper) savedTerm).getWrappedTerm()).getAssoc();
  }

  private boolean isOperandsOk() {
    if (this.savedTerm.getType() == TermType.OPERATOR) {
      final Op wrappedOperator = (Op) ((TermWrapper) this.savedTerm).getWrappedTerm();
      switch (wrappedOperator.getAssoc()) {
        case FX:
        case FY:
          return this.leftItem == null && this.rightItem != null;
        case YF:
        case XF:
          return this.leftItem != null && this.rightItem == null;
        case XFX:
        case XFY:
        case YFX:
          return this.leftItem != null && this.rightItem != null;
        default:
          throw new CriticalUnexpectedError();
      }
    } else {
      return this.leftItem == null && this.rightItem == null;
    }
  }

  private boolean isPrecedenceOk() {
    if (this.savedTerm.getType() == TermType.OPERATOR) {
      final int thisPrecedence = this.getPrecedence();
      final Op wrappedOperator = (Op) ((TermWrapper) this.savedTerm).getWrappedTerm();
      switch (wrappedOperator.getAssoc()) {
        case FX:
          return this.leftItem == null && (this.rightItem != null && this.rightItem.getPrecedence() < thisPrecedence);
        case FY:
          return this.leftItem == null && (this.rightItem != null && this.rightItem.getPrecedence() <= thisPrecedence);
        case YF:
          return (this.leftItem != null && this.leftItem.getPrecedence() <= thisPrecedence) && this.rightItem == null;
        case XF:
          return (this.leftItem != null && this.leftItem.getPrecedence() < thisPrecedence) && this.rightItem == null;
        case XFX:
          return (this.leftItem != null && this.leftItem.getPrecedence() < thisPrecedence) && (this.rightItem != null && this.rightItem.getPrecedence() < thisPrecedence);
        case XFY:
          return (this.leftItem != null && this.leftItem.getPrecedence() < thisPrecedence) && (this.rightItem != null && this.rightItem.getPrecedence() <= thisPrecedence);
        case YFX:
          return (this.leftItem != null && this.leftItem.getPrecedence() <= thisPrecedence) && (this.rightItem != null && this.rightItem.getPrecedence() < thisPrecedence);
        default:
          throw new CriticalUnexpectedError();
      }
    } else {
      return this.leftItem == null && this.rightItem == null;
    }
  }

  private boolean isAnyBlock() {
    return this.savedTerm.getType() == TermType.STRUCT
            && (this.savedTerm.getFunctor() == Op.VIRTUAL_OPERATOR_BLOCK
            || this.savedTerm.getFunctor() == Op.VIRTUAL_OPERATOR_CURLY_BLOCK);
  }

  private boolean isBlock() {
    return this.savedTerm.getType() == TermType.STRUCT
            && this.savedTerm.getFunctor() == Op.VIRTUAL_OPERATOR_BLOCK;
  }

  private boolean isOperator() {
    return this.savedTerm.getType() == TermType.OPERATOR;
  }

  @Override
  public String toString() {
    return "TreeItem[" + (this.savedTerm == null ? "null" : this.savedTerm.toString()) + ']';
  }

  public PrologTerm convertToTermAndRelease() {
    try {
      PrologTerm result;

      switch (savedTerm.getType()) {
        case OPERATOR: {
          final TermWrapper wrapper = (TermWrapper) this.savedTerm;
          if (this.leftItem == null && this.rightItem == null) {
            // it is an atom because it has not any argument
            return new PrologAtom(wrapper.getWrappedTerm().getText(), wrapper.getQuotation(), wrapper.getPos(), wrapper.getLine());
          }

          if (this.leftItem == null) {
            if (this.rightItem.getType() == TermType.STRUCT && this.rightItem.savedTerm.isAnyBlock() && !((PrologStruct) this.rightItem.savedTerm).isEmpty()) {

              final PrologTerm rightTerm = this.rightItem.convertToTermAndRelease();
              Op operator = (Op) wrapper.getWrappedTerm();
              final PrologTerm blockContent = ((PrologStruct) rightTerm).getTermAt(0);

              if (blockContent.getType() == TermType.STRUCT) {
                final PrologTerm[] terms = blockContent.flatComma(new ArrayList<>()).toArray(PrologParser.EMPTY_TERM_ARRAY);

                if (operator.getArity() == terms.length) {
                  return new PrologStruct(operator, terms, wrapper.getLine(), wrapper.getPos());
                } else {
                    final Op appropriateOperator = this.parser.getContext().findOpForName(this.parser, operator.getText()).findForArity(terms.length);
                    
                    if (appropriateOperator == null) {
                        if (operator.getArity() == 1) {
                            return new PrologStruct(operator, new PrologTerm[]{blockContent}, wrapper.getLine(), wrapper.getPos());
                        } else {
                            return new PrologStruct(new PrologAtom(wrapper.getText(), Quotation.SINGLE, wrapper.getLine(), wrapper.getPos()), terms, wrapper.getLine(), wrapper.getPos());
                        }
                    } else {
                        return new PrologStruct(appropriateOperator, terms, wrapper.getLine(), wrapper.getPos());
                    }
                }

              } else {
                if (rightTerm.isCurlyBlock()) {
                  return new PrologStruct(operator, new PrologTerm[]{rightTerm}, wrapper.getLine(), wrapper.getPos());
                } else {
                  if (operator.getArity() == 1) {
                    return new PrologStruct(operator, new PrologTerm[]{blockContent}, wrapper.getLine(), wrapper.getPos());
                  } else {
                    operator = this.parser.getContext().findOpForName(this.parser, operator.getText()).findForArity(1);
                    return operator == null ? new PrologStruct(new PrologAtom(wrapper.getText(), Quotation.SINGLE, wrapper.getLine(), wrapper.getPos()), new PrologTerm[]{blockContent}, wrapper.getLine(), wrapper.getPos())
                            : new PrologStruct(operator, new PrologTerm[]{blockContent}, wrapper.getLine(), wrapper.getPos());
                  }
                }
              }
            }
          } else if (this.isAnyBlock() && this.leftItem.isAnyBlock()) {
            return new PrologStruct(wrapper.getWrappedTerm(), new PrologTerm[]{this.leftItem.convertToTermAndRelease()}, wrapper.getLine(), wrapper.getPos());
          }

          if (!isOperandsOk()) {
            throw new PrologParserException("No operands: [" + wrapper.getText() + ']', wrapper.getLine(), wrapper.getPos());
          }

          final PrologTerm left;
          final PrologTerm right;

          if (!isPrecedenceOk()) {
            if (this.rightItem != null && this.rightItem.isOperator() && this.rightItem.getOpAssoc().isPrefix()) {
              left = this.leftItem == null ? null : this.leftItem.convertToTermAndRelease();
              right = new PrologStruct(Op.VIRTUAL_OPERATOR_BLOCK, new PrologTerm[]{this.rightItem.convertToTermAndRelease()});
            } else {
              throw new PrologParserException("Operator precedence clash or missing operator: [" + wrapper.getText() + ']', wrapper.getLine(), wrapper.getPos());
            }
          } else {
            left = this.leftItem == null ? null : this.leftItem.convertToTermAndRelease();
            right = this.rightItem == null ? null : this.rightItem.convertToTermAndRelease();
          }

          // this code replaces '-'(number) to '-number' if number is not negative one
          if ("-".equals(wrapper.getText()) && left == null && right instanceof PrologNumeric) {
            final PrologNumeric numeric = (PrologNumeric) right;
            if (!numeric.isNegative()) {
              result = ((PrologNumeric) right).makeNeg();
              break;
            }
          }

          final PrologStruct operatorStruct;
          if (left == null) {
            operatorStruct = new PrologStruct(wrapper.getWrappedTerm(), new PrologTerm[]{right}, wrapper.getLine(), wrapper.getPos());
          } else {
            operatorStruct = new PrologStruct(wrapper.getWrappedTerm(), right == null ? new PrologTerm[]{left} : new PrologTerm[]{left, right}, wrapper.getLine(), wrapper.getPos());
          }
          result = operatorStruct;
        }
        break;
        case STRUCT: {
          final PrologStruct thisStruct = (PrologStruct) this.savedTerm;
          if ((thisStruct.getFunctor() == Op.VIRTUAL_OPERATOR_BLOCK || thisStruct.getFunctor() == Op.VIRTUAL_OPERATOR_CURLY_BLOCK)
                  && thisStruct.getArity() == 1) {

            final PrologTerm thatTerm = thisStruct.getTermAt(0);

            if (thatTerm.getType() == TermType.STRUCT && (thatTerm.getFunctor() == Op.VIRTUAL_OPERATOR_BLOCK || thatTerm.getFunctor() == Op.VIRTUAL_OPERATOR_CURLY_BLOCK)) {
                // rolling normal blocks
                result = thatTerm.isBlock() && (this.isBlock()&& (this.parentItem == null || (this.parentItem != null && this.parentItem.isBlock()))) ? thatTerm : thisStruct;
            } else {
              result = thisStruct;
            }
          } else {
            result = thisStruct;
          }
        }
        break;
        default: {
          result = this.savedTerm;
        }
        break;
      }

      return result;
    } finally {
      this.release();
    }
  }

}
