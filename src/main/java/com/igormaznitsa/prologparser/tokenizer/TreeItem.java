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

final class TreeItem {

  private final SoftObjectPool<TreeItem> pool;

  private final PrologParser parser;
  private final SoftObjectPool<TermWrapper> termWrapperPool;
  private TreeItem leftBranch;
  private TreeItem rightBranch;
  private TreeItem parentItem;
  private boolean insideBrakes;
  private PrologTerm savedTerm;

  TreeItem(final PrologParser parser, final SoftObjectPool<TreeItem> pool, final SoftObjectPool<TermWrapper> termWrapperPool) {
    this.parser = parser;
    this.pool = pool;
    this.termWrapperPool = termWrapperPool;
  }

  TreeItem setData(final PrologTerm term, final boolean inBrakes, final int line, final int pos) {
    if (term == null) {
      this.replaceSavedTerm(null);
    } else {
      final TermType termType = term.getTermType();
      if (termType == TermType.__OPERATOR__ || termType == TermType.__OPERATOR_CONTAINER__) {
        final TermWrapper termWrapper = this.termWrapperPool.find().setWrappedTerm(term);
        this.replaceSavedTerm(termWrapper);
      } else {
        this.replaceSavedTerm(term);
      }
      this.savedTerm.setPos(pos);
      this.savedTerm.setLine(line);
    }
    this.insideBrakes = inBrakes;
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
    this.insideBrakes = false;
    this.parentItem = null;

    this.replaceSavedTerm(null);

    this.pool.push(this);
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
    if (item.getType() == TermType.__OPERATOR__ && item.getPriority() != 0) {
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
    return ((Op) ((TermWrapper) savedTerm).getWrappedTerm()).getOpType();
  }

  private boolean validate() {
    if (savedTerm.getTermType() == TermType.__OPERATOR__) {
      final int priority = getPriority();
      final Op wrappedOperator = (Op) ((TermWrapper) savedTerm).getWrappedTerm();
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

  PrologTerm convertToTermAndRelease() {
    try {
      PrologTerm result;

      switch (savedTerm.getTermType()) {
        case __OPERATOR__: {
          final TermWrapper wrapper = (TermWrapper) this.savedTerm;
          if (this.leftBranch == null && this.rightBranch == null) {
            // it is an atom because it has not any arguments
            return new PrologAtom(wrapper.getWrappedTerm().getTermText(), wrapper.getQuotingType(), wrapper.getPos(), wrapper.getLine());
          }

          if (!validate()) {
            throw new PrologParserException("Wrong operator [" + wrapper.getTermText() + ']', wrapper.getLine(), wrapper.getPos());
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

          if (this.parser.getContext() != null) {
            this.parser.getContext().onStructureCreated(this.parser, operatorStruct);
          }

        }
        break;
        case STRUCT: {
          if (this.parser.getContext() != null) {
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
    } finally {
      this.release();
    }
  }

}
