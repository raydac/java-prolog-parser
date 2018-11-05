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
import com.igormaznitsa.prologparser.terms.OpContainer;
import com.igormaznitsa.prologparser.terms.PrologAtom;
import com.igormaznitsa.prologparser.terms.PrologList;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.Quotation;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.tokenizer.Op;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import com.igormaznitsa.prologparser.tokenizer.TermWrapper;
import com.igormaznitsa.prologparser.tokenizer.Tokenizer;
import com.igormaznitsa.prologparser.tokenizer.TokenizerResult;
import com.igormaznitsa.prologparser.tokenizer.TreeItem;
import com.igormaznitsa.prologparser.utils.AssertUtils;
import com.igormaznitsa.prologparser.utils.Koi7CharOpMap;
import com.igormaznitsa.prologparser.utils.SoftObjectPool;

import java.io.Closeable;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import static com.igormaznitsa.prologparser.DefaultParserContext.of;
import static com.igormaznitsa.prologparser.ParserContext.*;
import static com.igormaznitsa.prologparser.terms.TermType.__OPERATOR_CONTAINER__;
import static com.igormaznitsa.prologparser.utils.Koi7CharOpMap.ofOps;

/**
 * Abstract base Prolog parser.
 */
public abstract class PrologParser implements Iterator<PrologTerm>, Iterable<PrologTerm>, Closeable {

  public static final PrologTerm[] EMPTY_TERM_ARRAY = new PrologTerm[0];
  protected static final Koi7CharOpMap META_OP_MAP;
  private static final int MAX_INTERNAL_POOL_SIZE = 96;
  private static final OpContainer OPERATOR_COMMA;
  private static final OpContainer OPERATOR_LEFTBRACKET;
  private static final OpContainer OPERATOR_LEFTCURLYBRACKET;
  private static final OpContainer OPERATOR_RIGHTBRACKET;
  private static final OpContainer OPERATOR_RIGHTCURLYBRACKET;
  private static final OpContainer OPERATOR_RIGHTSQUAREBRACKET;
  private static final OpContainer OPERATOR_DOT;
  private static final OpContainer OPERATOR_VERTICALBAR;
  private static final Koi7CharOpMap OPERATORS_PHRASE;
  private static final Koi7CharOpMap OPERATORS_INSIDE_LIST;
  private static final Koi7CharOpMap OPERATORS_END_LIST;
  private static final Koi7CharOpMap OPERATORS_INSIDE_STRUCT;
  private static final Koi7CharOpMap OPERATORS_SUBBLOCK;
  private static final Koi7CharOpMap OPERATORS_SUBBLOCK_CURLY;

  static {
    META_OP_MAP = ofOps();

    OPERATOR_DOT = META_OP_MAP.add(Op.METAOPERATOR_DOT);
    OPERATOR_LEFTBRACKET = META_OP_MAP.add(Op.METAOPERATOR_LEFT_BRACKET);
    OPERATOR_LEFTCURLYBRACKET = META_OP_MAP.add(Op.METAOPERATOR_LEFT_CURLY_BRACKET);
    OPERATOR_RIGHTBRACKET = META_OP_MAP.add(Op.METAOPERATOR_RIGHT_BRACKET);
    OPERATOR_RIGHTCURLYBRACKET = META_OP_MAP.add(Op.METAOPERATOR_RIGHT_CURLY_BRACKET);
    META_OP_MAP.add(Op.METAOPERATOR_LEFT_SQUARE_BRACKET);
    OPERATOR_RIGHTSQUAREBRACKET = META_OP_MAP.add(Op.METAOPERATOR_RIGHT_SQUARE_BRACKET);
    OPERATOR_VERTICALBAR = META_OP_MAP.add(Op.METAOPERATOR_VERTICAL_BAR);
    OPERATOR_COMMA = META_OP_MAP.add(Op.METAOPERATOR_COMMA);

    OPERATORS_PHRASE = ofOps(OPERATOR_DOT);
    OPERATORS_INSIDE_LIST = ofOps(OPERATOR_COMMA, OPERATOR_RIGHTSQUAREBRACKET, OPERATOR_VERTICALBAR);
    OPERATORS_END_LIST = ofOps(OPERATOR_RIGHTSQUAREBRACKET);
    OPERATORS_INSIDE_STRUCT = ofOps(OPERATOR_COMMA, OPERATOR_RIGHTBRACKET);
    OPERATORS_SUBBLOCK = ofOps(OPERATOR_RIGHTBRACKET);
    OPERATORS_SUBBLOCK_CURLY = ofOps(OPERATOR_RIGHTCURLYBRACKET);
  }

  protected final ParserContext context;
  protected final int parserFlags;
  private final SoftObjectPool<TreeItem> treeItemPool;
  private final SoftObjectPool<TermWrapper> termWrapperPool;
  private final SoftObjectPool<List<PrologTerm>> termArrayListPool;
  private final Tokenizer tokenizer;
  private PrologTerm lastFoundTerm;

  public PrologParser(final Reader source, final ParserContext context) {
    this.context = context == null ? of(ParserContext.FLAG_NONE) : context;
    this.tokenizer = new Tokenizer(this, META_OP_MAP, AssertUtils.assertNotNull(source));
    this.parserFlags = context == null ? FLAG_NONE : context.getFlags();

    this.termArrayListPool = new SoftObjectPool<List<PrologTerm>>(MAX_INTERNAL_POOL_SIZE) {
      @Override
      public final List<PrologTerm> get() {
        return new ArrayList<>();
      }
    };

    this.termWrapperPool = new SoftObjectPool<TermWrapper>(MAX_INTERNAL_POOL_SIZE) {
      @Override
      public final TermWrapper get() {
        return new TermWrapper(this);
      }
    };

    this.treeItemPool = new SoftObjectPool<TreeItem>(MAX_INTERNAL_POOL_SIZE) {
      @Override
      public final TreeItem get() {
        return new TreeItem(PrologParser.this, this, termWrapperPool);
      }
    };
  }

  public static Op findBaseMetaOperator(final String text, final OpAssoc type) {
    if (text.length() != 1) {
      return null;
    }

    OpContainer container = META_OP_MAP.get(text);

    if (container == null) {
      container = META_OP_MAP.get(text);
    }

    Op result = null;

    if (container != null) {
      result = container.findForType(type);
    }

    return result;
  }

  private static int getOnlyCharCode(final String text) {
    if (text == null || text.length() != 1) {
      return -1;
    } else {
      return text.charAt(0);
    }
  }

  public static Koi7CharOpMap findMetaOps() {
    return Koi7CharOpMap.copyOf(META_OP_MAP);
  }

  private boolean isEndOperator(final PrologTerm operator, final Koi7CharOpMap endOperators) {
    if (operator == null) {
      return true;
    }

    if (endOperators == null) {
      return false;
    }

    return operator.getTermType() == __OPERATOR_CONTAINER__ && endOperators.contains(operator.getTermText());
  }

  public ParserContext getContext() {
    return context;
  }

  @Override
  public boolean hasNext() {
    if (this.lastFoundTerm == null) {
      final PrologTerm found = readBlock(OPERATORS_PHRASE);
      if (found != null) {
        final TokenizerResult endAtom = this.tokenizer.readNextToken();
        try {
          if (endAtom == null || !endAtom.getResult().getTermText().equals(OPERATOR_DOT.getTermText())) {
            throw new PrologParserException("End operator is not found", this.tokenizer.getLine(), this.tokenizer.getPos());
          }
        } finally {
          if (endAtom != null) {
            endAtom.release();
          }
        }
      }
      this.lastFoundTerm = found;
    }

    return this.lastFoundTerm != null;
  }

  @Override
  public PrologTerm next() {
    try {
      if (hasNext()) {
        return this.lastFoundTerm;
      } else {
        throw new NoSuchElementException("No terms in source");
      }
    } finally {
      this.lastFoundTerm = null;
    }
  }

  private PrologStruct readStruct(final PrologTerm functor) {
    final List<PrologTerm> listOfAtoms = this.termArrayListPool.find();
    try {
      PrologStruct result;
      boolean active = true;
      while (active && !Thread.currentThread().isInterrupted()) {
        final PrologTerm block = readBlock(OPERATORS_INSIDE_STRUCT);

        if (block == null) {
          return null;
        }

        final TokenizerResult nextAtom = this.tokenizer.readNextToken();
        if (nextAtom == null) {
          throw new PrologParserException("Can't read next token in block", this.tokenizer.getLine(), this.tokenizer.getPos());
        }

        try {
          final String nextText = nextAtom.getResult().getTermText();

          switch (getOnlyCharCode(nextText)) {
            case ',': {
              listOfAtoms.add(block);
            }
            break;
            case ')': {
              listOfAtoms.add(block);
              active = false;
            }
            break;
            default:
              throw new PrologParserException("Unexpected term in structure: " + nextText, nextAtom.getLine(), nextAtom.getPos());
          }
        } finally {
          nextAtom.release();
        }
      }

      result = new PrologStruct(functor, listOfAtoms.toArray(EMPTY_TERM_ARRAY));
      return result;
    } finally {
      listOfAtoms.clear();
      this.termArrayListPool.push(listOfAtoms);
    }
  }

  private PrologTerm readList(final TokenizerResult openingBracket) {
    PrologList leftPart = new PrologList();
    PrologList leftPartFirst = leftPart;
    PrologTerm rightPart = null;

    boolean hasSeparator = false;

    boolean doRead = true;

    while (doRead && !Thread.currentThread().isInterrupted()) {
      final PrologTerm block = readBlock(OPERATORS_INSIDE_LIST);

      final TokenizerResult nextAtom = this.tokenizer.readNextToken();
      if (nextAtom == null) {
        throw new PrologParserException("Can't read next token in list", this.tokenizer.getLine(), this.tokenizer.getPos());
      }

      try {
        final String text = nextAtom.getResult().getTermText();

        switch (getOnlyCharCode(text)) {
          case ']': {
            doRead = false;
            if (block == null) {
              continue;
            }
          }
          break;
          case '|': {
            // we have found the list tail, so we need read it as one block
            // until the ']' atom
            checkForNull(block, "There is not any list element", openingBracket);
            if (leftPartFirst.isEmpty()) {
              leftPartFirst = PrologList.setTermAsNewListTail(leftPart, block);
            } else {
              PrologList.setTermAsNewListTail(leftPart, block);
            }

            hasSeparator = true;

            rightPart = readBlock(OPERATORS_END_LIST);

            if (rightPart != null
                && rightPart.getTermType() == TermType.STRUCT
                && ((PrologStruct) rightPart).getFunctor().getTermText().equals(OPERATOR_VERTICALBAR.getTermText())) {
              throw new PrologParserException(
                  "Duplicated list tail definition",
                  tokenizer.getLastTokenLine(),
                  tokenizer.getLastTokenPos(), null);
            }

            final TokenizerResult nextAtomTwo = tokenizer.readNextToken();
            if (nextAtomTwo == null) {
              throw new PrologParserException("Can't find expected token in list", this.tokenizer.getLine(), this.tokenizer.getPos());
            }
            try {
              if (!nextAtomTwo.getResult().getTermText().equals(OPERATOR_RIGHTSQUAREBRACKET.getTermText())) {
                throw new PrologParserException("Wrong end of the list tail", this.tokenizer.getLastTokenLine(), this.tokenizer.getLastTokenPos());
              }
            } finally {
              nextAtomTwo.release();
            }
            doRead = false;
            continue;
          }
          case ',': {
            // all good and we read next block
            checkForNull(block, "List element not found", nextAtom);
          }
          break;
          default: {
            throw new CriticalUnexpectedError();
          }
        }

        if (leftPartFirst.isEmpty()) {
          leftPartFirst = PrologList.setTermAsNewListTail(leftPart, block);
          leftPart = leftPartFirst;
        } else {
          leftPart = PrologList.setTermAsNewListTail(leftPart, block);
        }
      } finally {
        nextAtom.release();
      }
    }

    if (hasSeparator) {
      // '|' separator was found at the list
      if (rightPart == null) {
        throw new PrologParserException("There is not any term as the tail at the list", this.tokenizer.getLastTokenLine(), this.tokenizer.getLastTokenPos());
      }
      if (rightPart.getTermType() == TermType.ATOM && rightPart.getQuotation() == Quotation.NONE && ",".equals(rightPart.getTermText())) {
        throw new PrologParserException("Comma operator in list tail", this.tokenizer.getLastTokenLine(), this.tokenizer.getLastTokenPos());
      }
      leftPartFirst.replaceTail(rightPart);
    }
    return leftPartFirst;
  }

  private void checkForNull(final Object obj, final String message, final TokenizerResult startTerm) {
    if (obj == null) {
      throw new PrologParserException(message, startTerm.getLine(), startTerm.getPos());
    }
  }

  private PrologTerm readBlock(final Koi7CharOpMap endOperators) {
    // the variable will contain last processed tree item contains either
    // atom or operator
    TreeItem currentTreeItem = null;

    while (!Thread.currentThread().isInterrupted()) {
      // read next atom from tokenizer
      TokenizerResult readAtomContainer = this.tokenizer.readNextToken();
      try {
        if (readAtomContainer == null) {
          if (currentTreeItem == null) {
            // end_of_file
            return null;
          } else {
            // non closed something
            throw new PrologParserException("Non-ended clause", this.tokenizer.getLastTokenLine(), this.tokenizer.getLastTokenPos());
          }
        }

        PrologTerm readAtom = readAtomContainer.getResult();

        // check the atom to be the end atom
        if (isEndOperator(readAtom, endOperators)) {
          // it's an end atom so we push it back and end the cycle
          tokenizer.push(readAtomContainer);
          readAtomContainer = null;
          break;
        }

        // the variable contains calculated item precedence (it can be not the
        // same as the natural precedence)
        int readAtomPrecedence = 0; // we make it as highest precedence

        if (readAtom.getTermType() == __OPERATOR_CONTAINER__) {
          // it is operator list
          // try to get the single operator from the list if the list
          // contains only one
          final Op readOperator = ((OpContainer) readAtom).getIfSingle();

          // check that the operator is single
          if (readOperator == null) {

            // there are a few operators in the list so we need to
            // select one
            final OpContainer readOperators = (OpContainer) readAtom;

            boolean leftPresented = false;

            if (currentTreeItem != null) {
              if (currentTreeItem.getType() == TermType.OPERATOR) {
                if (currentTreeItem.getRightBranch() != null) {
                  leftPresented = true;
                }
              } else {
                leftPresented = true;
              }
            }

            final TokenizerResult peekResult = this.tokenizer.peek();
            final boolean rightPresented = peekResult != null && !isEndOperator(peekResult.getResult(), endOperators);

            readAtom = readOperators.findSimilar(leftPresented, rightPresented);

            if (readAtom == null) {
              if (currentTreeItem == null && !(leftPresented || rightPresented)) {
                // alone operator, it is an atom
                return new PrologAtom(readOperators.getTermText(), Quotation.SINGLE, readOperators.getLine(), readOperators.getPos());
              }
              // we didn't get any operator for our criteria, so throw
              // an exception
              throw new PrologParserException("Operator clash detected [" + readAtomContainer.getResult().getTermText() + ']',
                  readAtomContainer.getLine(), readAtomContainer.getPos());
            }
            // we have found needed operator so get its precedence
            readAtomPrecedence = readAtom.getPrecedence();
          } else {
            readAtom = readOperator;
            final String operatorText = readOperator.getTermText();

            if (operatorText.length() == 1) {
              final int onlyCharCode = getOnlyCharCode(operatorText);
              switch (onlyCharCode) {
                case '[': {
                  // it's a list
                  readAtom = readList(readAtomContainer);
                  readAtom.setPos(readAtomContainer.getPos());
                  readAtom.setLine(readAtomContainer.getLine());
                  readAtomPrecedence = 0;
                }
                break;
                case '{':
                case '(': {
                  boolean processReadAtom = true;
                  if (onlyCharCode == '(') {
                    readAtom = readBlock(OPERATORS_SUBBLOCK);
                  } else {
                    if ((this.parserFlags & ParserContext.FLAG_CURLY_BRACKETS_ALLOWED) == 0) {
                      readAtomPrecedence = readOperator.getPrecedence();
                      processReadAtom = false;
                    } else {
                      readAtom = readBlock(OPERATORS_SUBBLOCK_CURLY);
                    }
                  }

                  if (processReadAtom) {
                    if (readAtom == null) {
                      throw new PrologParserException("Illegal start of term",
                          readAtomContainer.getLine(), readAtomContainer.getPos());
                    }

                    readAtom.setLine(readAtomContainer.getLine());
                    readAtom.setPos(readAtomContainer.getPos());

                    readAtom = new PrologStruct(onlyCharCode == '{' ? Op.VIRTUAL_OPERATOR_CURLY_BLOCK : Op.VIRTUAL_OPERATOR_BLOCK, new PrologTerm[] {readAtom}, readAtomContainer.getLine(), readAtomContainer.getPos());

                    final TokenizerResult token = this.tokenizer.readNextToken();

                    final PrologTerm closingAtom;
                    if (token == null) {
                      closingAtom = null;
                    } else {
                      closingAtom = token.getResult();
                      token.release();
                    }

                    if (closingAtom == null || !closingAtom.getTermText().equals((onlyCharCode == '{' ? OPERATOR_RIGHTCURLYBRACKET : OPERATOR_RIGHTBRACKET).getTermText())) {
                      throw new PrologParserException("Non-closed brackets: " + onlyCharCode, this.tokenizer.getLine(), this.tokenizer.getPos());
                    }
                  }
                }
                break;
                default: {
                  readAtomPrecedence = readOperator.getPrecedence();
                }
                break;
              }
            } else {
              readAtomPrecedence = readOperator.getPrecedence();
            }
          }
        } else {
          if (readAtom.getTermType() != TermType.VAR || (this.parserFlags & FLAG_VAR_AS_FUNCTOR) != 0) {
            TokenizerResult nextToken = this.tokenizer.readNextToken();

            if (nextToken == null) {
              throw new PrologParserException("Non-closed clause", this.tokenizer.getLastTokenLine(), this.tokenizer.getLastTokenPos());
            }

            try {
              if (nextToken.getResult().getTermText().equals(OPERATOR_LEFTBRACKET.getTermText())) {
                final int nextTokenLineNumber = nextToken.getLine();
                final int nextTokenStrPosition = nextToken.getPos();

                // it is a structure
                if (
                    readAtom.getTermType() == TermType.ATOM
                        || (readAtom.getTermType() == TermType.VAR
                        && (this.parserFlags & FLAG_VAR_AS_FUNCTOR) != 0)
                ) {

                  final PrologTerm prev = readAtom;
                  readAtom = readStruct(readAtom);
                  if (readAtom == null) {
                    // we have met the empty brackets
                    if ((this.parserFlags & FLAG_ZERO_STRUCT_ALLOWED) == 0) {
                      throw new PrologParserException("Empty structure is not allowed",
                          nextTokenLineNumber, nextTokenStrPosition);
                    } else {
                      final TokenizerResult pushed = this.tokenizer.pop();
                      if (pushed.getResult() == OPERATOR_RIGHTBRACKET) {
                        readAtom = new PrologStruct(prev);
                      } else {
                        throw new CriticalUnexpectedError();
                      }
                    }
                  }
                } else {
                  tokenizer.push(nextToken);
                  nextToken = null;
                  throw new PrologParserException("You must have an atom as the structure functor",
                      nextTokenLineNumber, nextTokenStrPosition);
                }
              } else {
                // push back the next atom
                tokenizer.push(nextToken);
                nextToken = null;
              }
            } finally {
              if (nextToken != null) {
                nextToken.release();
              }
            }
          }
        }

        final TreeItem readAtomTreeItem = this.treeItemPool.find().setData(readAtom,
            readAtomContainer.getLine(),
            readAtomContainer.getPos());

        if (currentTreeItem == null) {
          // it's first
          currentTreeItem = readAtomTreeItem;
        } else {
          // not first
          if (currentTreeItem.getType() == TermType.OPERATOR) {
            // it's not first operator
            if (currentTreeItem.getPrecedence() <= readAtomPrecedence) {
              if (readAtom.getTermType() == TermType.OPERATOR && ((Op) readAtom).getOpAssoc().isPrefix()) {
                // it is a prefix operator so that it can be there
                currentTreeItem = currentTreeItem.makeAsRightBranch(readAtomTreeItem);
              } else {
                // new has lower or equal precedence
                // make it as ascendant one
                final TreeItem foundItem = currentTreeItem.findFirstNodeWithSuchOrLowerPrecedence(readAtomPrecedence);
                if (foundItem.getPrecedence() < readAtomPrecedence) {
                  // make as parent
                  currentTreeItem = foundItem.makeAsOwnerWithLeftBranch(readAtomTreeItem);
                } else if (foundItem.getPrecedence() > readAtomPrecedence) {
                  // make new as right sub-branch
                  currentTreeItem = foundItem.makeAsRightBranch(readAtomTreeItem);
                } else {
                  // equal precedence
                  switch (foundItem.getOpAssoc()) {
                    case XF:
                    case YF:
                    case FX:
                    case XFX:
                    case YFX:
                      currentTreeItem = foundItem.makeAsOwnerWithLeftBranch(readAtomTreeItem);
                      break;
                    case FY:
                    case XFY:
                      currentTreeItem = foundItem.makeAsRightBranch(readAtomTreeItem);
                      break;
                    default:
                      throw new CriticalUnexpectedError();
                  }
                }
              }
            } else if (currentTreeItem.getPrecedence() > readAtomPrecedence) {
              // new has greater precedence
              if (readAtomTreeItem.getType() != TermType.OPERATOR && currentTreeItem.getRightBranch() != null) {
                // it's a ground atom and its right branch is not empty
                throw new PrologParserException(
                    "There is no any operator before the atom",
                    readAtomContainer.getLine(),
                    readAtomContainer.getPos());
              }
              // make it as right
              currentTreeItem = currentTreeItem.makeAsRightBranch(readAtomTreeItem);
            }
          } else {
            // check that it is an operator
            if (currentTreeItem.getType() != TermType.OPERATOR
                && readAtomTreeItem.getType() != TermType.OPERATOR) {
              throw new PrologParserException(
                  "There must be an operator between atoms or structures",
                  readAtomContainer.getLine(),
                  readAtomContainer.getPos());
            }

            // make it as left branch
            currentTreeItem = currentTreeItem.makeAsOwnerWithLeftBranch(readAtomTreeItem);
          }
        }
      } finally {
        if (readAtomContainer != null) {
          readAtomContainer.release();
        }
      }
    }
    if (currentTreeItem == null) {
      return null;
    } else {
      return currentTreeItem.findRoot().convertToTermAndRelease();
    }
  }

  @Override
  public Iterator<PrologTerm> iterator() {
    return new Iterator<PrologTerm>() {
      @Override
      public boolean hasNext() {
        return PrologParser.this.hasNext();
      }

      @Override
      public PrologTerm next() {
        return PrologParser.this.next();
      }
    };
  }

  public Stream<PrologTerm> stream() {
    return StreamSupport.stream(
        Spliterators.spliteratorUnknownSize(
            this,
            Spliterator.ORDERED | Spliterator.IMMUTABLE | Spliterator.NONNULL),
        false
    );
  }

  @Override
  public void close() throws IOException {
    this.tokenizer.close();
  }
}
