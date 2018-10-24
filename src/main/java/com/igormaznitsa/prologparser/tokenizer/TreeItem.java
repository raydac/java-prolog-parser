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

package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologNumeric;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.utils.SoftObjectPool;

import java.util.ArrayList;

final class TreeItem {

  private final SoftObjectPool<TreeItem> pool;

  private final PrologParser parser;
  private final SoftObjectPool<TermWrapper> termWrapperPool;
  private TreeItem leftBranch;
  private TreeItem rightBranch;
  private TreeItem parentItem;
  private PrologTerm savedTerm;

  TreeItem(final PrologParser parser, final SoftObjectPool<TreeItem> pool, final SoftObjectPool<TermWrapper> termWrapperPool) {
    this.parser = parser;
    this.pool = pool;
    this.termWrapperPool = termWrapperPool;
  }

  TreeItem setData(final PrologTerm term, final int line, final int pos) {
    if (term == null) {
      this.replaceSavedTerm(null);
    } else {
      final TermType termType = term.getTermType();
      if (termType == TermType.OPERATOR || termType == TermType.__OPERATOR_CONTAINER__) {
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

  void release() {
    this.leftBranch = null;
    this.rightBranch = null;
    this.parentItem = null;

    this.replaceSavedTerm(null);

    this.pool.push(this);
  }

  int getPrecedence() {
    return this.savedTerm.getPrecedence();
  }

  TreeItem makeAsRightBranch(final TreeItem item) {
    final TreeItem currentSubbranch = rightBranch;
    setRightBranch(item);
    item.setLeftBranch(currentSubbranch);
    TreeItem result = this;
    if (item.getType() == TermType.OPERATOR && item.getPrecedence() != 0) {
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
    return savedTerm.getTermType();
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
      final TreeItem itsParent = result.parentItem;
      if (itsParent == null || result.getPrecedence() >= priority) {
        break;
      } else {
        result = itsParent;
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
    return ((Op) ((TermWrapper) savedTerm).getWrappedTerm()).getOpType();
  }

  private boolean isPrecedenceOrderOk() {
    if (this.savedTerm.getTermType() == TermType.OPERATOR) {
      final int thisPrecedence = this.getPrecedence();
      final Op wrappedOperator = (Op) ((TermWrapper) this.savedTerm).getWrappedTerm();
      switch (wrappedOperator.getOpType()) {
        case FX:
          return this.leftBranch == null && (this.rightBranch != null && this.rightBranch.getPrecedence() < thisPrecedence);
        case FY:
          return this.leftBranch == null && (this.rightBranch != null && this.rightBranch.getPrecedence() <= thisPrecedence);
        case YF:
          return (this.leftBranch != null && this.leftBranch.getPrecedence() <= thisPrecedence) && this.rightBranch == null;
        case XF:
          return (this.leftBranch != null && this.leftBranch.getPrecedence() < thisPrecedence) && this.rightBranch == null;
        case XFX:
          return (this.leftBranch != null && this.leftBranch.getPrecedence() < thisPrecedence) && (this.rightBranch != null && this.rightBranch.getPrecedence() < thisPrecedence);
        case XFY:
          return (this.leftBranch != null && this.leftBranch.getPrecedence() < thisPrecedence) && (this.rightBranch != null && this.rightBranch.getPrecedence() <= thisPrecedence);
        case YFX:
          return (this.leftBranch != null && this.leftBranch.getPrecedence() <= thisPrecedence) && (this.rightBranch != null && this.rightBranch.getPrecedence() < thisPrecedence);
        default:
          throw new CriticalUnexpectedError();
      }
    } else {
      return this.leftBranch == null && this.rightBranch == null;
    }
  }

  private boolean isBlock() {
    return this.savedTerm.getTermType() == TermType.STRUCT && ((PrologStruct) this.savedTerm).getFunctor() == Op.VIRTUAL_OPERATOR_BLOCK;
  }

  @Override
  public String toString() {
    return savedTerm.toString();
  }

  PrologTerm convertToTermAndRelease() {
    try {
      PrologTerm result;

      switch (savedTerm.getTermType()) {
        case OPERATOR: {
          final TermWrapper wrapper = (TermWrapper) this.savedTerm;
          if (this.leftBranch == null && this.rightBranch == null) {
            // it is an atom because it has not any argument
            return new PrologAtom(wrapper.getWrappedTerm().getTermText(), wrapper.getQuotingType(), wrapper.getPos(), wrapper.getLine());
          }

          if (this.leftBranch == null) {
            if (this.rightBranch.getType() == TermType.STRUCT && this.rightBranch.savedTerm.isBlock()) {
              final PrologTerm rightTerm = this.rightBranch.convertToTermAndRelease();
              Op operator = (Op) wrapper.getWrappedTerm();
              final PrologTerm blockContent = ((PrologStruct) rightTerm).getElementAt(0);
              if (blockContent.getTermType() == TermType.STRUCT) {
                final PrologTerm[] terms = ((PrologStruct) blockContent).fillElementsForComma(new ArrayList<>()).toArray(PrologParser.EMPTY_TERM_ARRAY);
                if (operator.getArity() == terms.length) {
                  return new PrologStruct(operator, terms, wrapper.getLine(), wrapper.getPos());
                } else {
                  operator = this.parser.context.findOpForName(this.parser, operator.getTermText()).findForArity(terms.length);
                  return operator == null
                      ? new PrologStruct(
                      new PrologAtom(wrapper.getTermText(), PrologTerm.QuotingType.SINGLE_QUOTED, wrapper.getLine(), wrapper.getPos()),
                      terms, wrapper.getLine(), wrapper.getPos())
                      : new PrologStruct(operator, terms, wrapper.getLine(), wrapper.getPos());
                }
              } else {
                if (operator.getArity() == 1) {
                  return new PrologStruct(operator, new PrologTerm[] {blockContent}, wrapper.getLine(), wrapper.getPos());
                } else {
                  operator = this.parser.context.findOpForName(this.parser, operator.getTermText()).findForArity(1);
                  return operator == null ? new PrologStruct(new PrologAtom(wrapper.getTermText(), PrologTerm.QuotingType.SINGLE_QUOTED, wrapper.getLine(), wrapper.getPos()), new PrologTerm[] {blockContent}, wrapper.getLine(), wrapper.getPos())
                      : new PrologStruct(operator, new PrologTerm[] {blockContent}, wrapper.getLine(), wrapper.getPos());
                }
              }
            }
          } else if (this.isBlock() && this.leftBranch.isBlock()) {
            return this.leftBranch.convertToTermAndRelease();
          }

          if (!isPrecedenceOrderOk()) {
            if (this.rightBranch != null || this.leftBranch != null) {
              final Op operator = (Op) wrapper.getWrappedTerm();
              if (operator.getOpType() == OpType.XF || operator.getOpType() == OpType.YF || operator.getOpType() == OpType.FX || operator.getOpType() == OpType.FY) {
                final PrologTerm that = this.rightBranch != null
                    ? this.rightBranch.convertToTermAndRelease()
                    : this.leftBranch.convertToTermAndRelease();
                if (that.getTermType() != TermType.STRUCT && that.getTermType() != TermType.VAR) {
                  return new PrologStruct(that, new PrologTerm[] {
                      new PrologAtom(operator.getTermText())
                  }, operator.getLine(), operator.getPos());
                }
              }
            }

            throw new PrologParserException("Operator priority clash or missing operator: [" + wrapper.getTermText() + ']', wrapper.getLine(), wrapper.getPos());
          }

          final PrologTerm left = this.leftBranch == null ? null : this.leftBranch.convertToTermAndRelease();
          final PrologTerm right = this.rightBranch == null ? null : this.rightBranch.convertToTermAndRelease();

          if (left == null && right == null) {
            throw new PrologParserException("Op without operands", wrapper.getLine(), wrapper.getPos());
          }

          // this code replaces '-'(number) to '-number'
          if (right instanceof PrologNumeric && wrapper.getQuotingType() == PrologTerm.QuotingType.NO_QUOTED && left == null && right.getTermType() == TermType.ATOM) {
            if ("-".equals(wrapper.getTermText())) {
              result = ((PrologNumeric) right).neg();
              break;
            } else if ("+".equals(wrapper.getTermText())) {
              result = right;
              break;
            }
          }

          final PrologStruct operatorStruct;
          if (left == null) {
            operatorStruct = new PrologStruct(wrapper.getWrappedTerm(), new PrologTerm[] {right}, wrapper.getLine(), wrapper.getPos());
          } else {
            operatorStruct = new PrologStruct(wrapper.getWrappedTerm(), right == null ? new PrologTerm[] {left} : new PrologTerm[] {left, right}, wrapper.getLine(), wrapper.getPos());
          }
          result = operatorStruct;
        }
        break;
        case STRUCT: {
          final PrologStruct thisStruct = (PrologStruct) this.savedTerm;
          if (thisStruct.getFunctor() == Op.VIRTUAL_OPERATOR_BLOCK
              && thisStruct.getArity() == 1) {
            final PrologTerm thatTerm = thisStruct.getElementAt(0);
            if (thatTerm.getTermType() == TermType.STRUCT && ((PrologStruct) thatTerm).getFunctor() == Op.VIRTUAL_OPERATOR_BLOCK) {
              result = thatTerm;
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
