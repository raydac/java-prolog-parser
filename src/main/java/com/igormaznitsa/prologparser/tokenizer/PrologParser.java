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

import com.igormaznitsa.prologparser.DefaultParserContext;
import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.Op;
import com.igormaznitsa.prologparser.operators.OpContainer;
import com.igormaznitsa.prologparser.operators.OpType;
import com.igormaznitsa.prologparser.operators.Ops;
import com.igormaznitsa.prologparser.terms.PrologList;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.terms.TermType;
import com.igormaznitsa.prologparser.utils.AssertUtils;
import com.igormaznitsa.prologparser.utils.SoftObjectPool;
import com.igormaznitsa.prologparser.utils.StringBuilderEx;

import java.io.Closeable;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import static com.igormaznitsa.prologparser.operators.OpContainer.make;

/**
 * Abstract prolog parser.
 */
public abstract class PrologParser implements Iterator<PrologTerm>, Iterable<PrologTerm>, Closeable {

  protected static final OneCharOpMap META_SYSTEM_OPERATORS = new OneCharOpMap();
  protected static final Map<String, OpContainer> SYSTEM_OPERATORS = new HashMap<>();
  protected static final Set<String> SYSTEM_OPERATORS_PREFIXES = new HashSet<>();
  private static final int MAX_INTERNAL_POOL_SIZE = 96;
  private final static OpContainer OPERATOR_COMMA;
  private final static OpContainer OPERATOR_LEFTBRACKET;
  private final static OpContainer OPERATOR_RIGHTBRACKET;
  private final static OpContainer OPERATOR_LEFTSQUAREBRACKET;
  private final static OpContainer OPERATOR_RIGHTSQUAREBRACKET;
  private final static OpContainer OPERATOR_DOT;
  private final static OpContainer OPERATOR_VERTICALBAR;
  private static final OneCharOpMap OPERATORS_PHRASE;
  private static final OneCharOpMap OPERATORS_INSIDE_LIST;
  private static final OneCharOpMap OPERATORS_END_LIST;
  private static final OneCharOpMap OPERATORS_INSIDE_STRUCT;
  private static final OneCharOpMap OPERATORS_SUBBLOCK;
  private static final PrologTerm[] EMPTY = new PrologTerm[0];

  static {
    META_SYSTEM_OPERATORS.clear();

    OPERATOR_DOT = make(Op.METAOPERATOR_DOT);
    META_SYSTEM_OPERATORS.put(Op.METAOPERATOR_DOT.getTermText(), OPERATOR_DOT);

    OPERATOR_LEFTBRACKET = make(Op.METAOPERATOR_LEFT_BRACKET);
    META_SYSTEM_OPERATORS.put(Op.METAOPERATOR_LEFT_BRACKET.getTermText(), OPERATOR_LEFTBRACKET);

    OPERATOR_RIGHTBRACKET = make(Op.METAOPERATOR_RIGHT_BRACKET);
    META_SYSTEM_OPERATORS.put(Op.METAOPERATOR_RIGHT_BRACKET.getTermText(), OPERATOR_RIGHTBRACKET);

    OPERATOR_LEFTSQUAREBRACKET = make(Op.METAOPERATOR_LEFT_SQUARE_BRACKET);
    META_SYSTEM_OPERATORS.put(Op.METAOPERATOR_LEFT_SQUARE_BRACKET.getTermText(), OPERATOR_LEFTSQUAREBRACKET);

    OPERATOR_RIGHTSQUAREBRACKET = make(Op.METAOPERATOR_RIGHT_SQUARE_BRACKET);
    META_SYSTEM_OPERATORS.put(Op.METAOPERATOR_RIGHT_SQUARE_BRACKET.getTermText(), OPERATOR_RIGHTSQUAREBRACKET);

    OPERATOR_VERTICALBAR = make(Op.METAOPERATOR_VERTICAL_BAR);
    META_SYSTEM_OPERATORS.put(Op.METAOPERATOR_VERTICAL_BAR.getTermText(), OPERATOR_VERTICALBAR);

    registerSysOp(Ops.of(1000, OpType.XFY, ","));

    SYSTEM_OPERATORS_PREFIXES.add(Op.METAOPERATOR_DOT.getTermText());
    SYSTEM_OPERATORS_PREFIXES.add(Op.METAOPERATOR_LEFT_BRACKET.getTermText());
    SYSTEM_OPERATORS_PREFIXES.add(Op.METAOPERATOR_LEFT_SQUARE_BRACKET.getTermText());
    SYSTEM_OPERATORS_PREFIXES.add(Op.METAOPERATOR_RIGHT_BRACKET.getTermText());
    SYSTEM_OPERATORS_PREFIXES.add(Op.METAOPERATOR_RIGHT_SQUARE_BRACKET.getTermText());
    SYSTEM_OPERATORS_PREFIXES.add(Op.METAOPERATOR_VERTICAL_BAR.getTermText());

    OPERATOR_COMMA = SYSTEM_OPERATORS.get(",");

    OPERATORS_PHRASE = new OneCharOpMap(OPERATOR_DOT);
    OPERATORS_INSIDE_LIST = new OneCharOpMap(OPERATOR_COMMA, OPERATOR_RIGHTSQUAREBRACKET, OPERATOR_VERTICALBAR);
    OPERATORS_END_LIST = new OneCharOpMap(OPERATOR_RIGHTSQUAREBRACKET);
    OPERATORS_INSIDE_STRUCT = new OneCharOpMap(OPERATOR_COMMA, OPERATOR_RIGHTBRACKET);
    OPERATORS_SUBBLOCK = new OneCharOpMap(OPERATOR_RIGHTBRACKET);
  }

  protected final ParserContext context;
  private final SoftObjectPool<TreeItem> treeItemPool;
  private final SoftObjectPool<TermWrapper> termWrapperPool;
  private final SoftObjectPool<List<PrologTerm>> termArrayListPool;
  private final Tokenizer tokenizer;
  private PrologTerm lastFoundTerm;

  public PrologParser(final Reader source, final ParserContext context) {
    this.context = context == null ? new DefaultParserContext(ParserContext.FLAG_NONE) : context;
    this.tokenizer = new Tokenizer(this, AssertUtils.assertNotNull(source));

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

  private static void registerSysOp(final Ops... operators) {
    final StringBuilderEx buff = new StringBuilderEx(10);

    Stream.of(operators).flatMap(x -> Arrays.stream(x.makeOperators())).forEach(op -> {
      final String opText = op.getTermText();
      if (SYSTEM_OPERATORS.containsKey(opText)) {
        final OpContainer container = SYSTEM_OPERATORS.get(opText);
        container.add(op);
      } else {
        final OpContainer container = make(op);
        SYSTEM_OPERATORS.put(opText, container);
      }
      buff.clear();
      for (final char c : opText.toCharArray()) {
        buff.append(c);
        SYSTEM_OPERATORS_PREFIXES.add(buff.toString());
      }
    });
  }

  public static Map<String, OpContainer> findAllSystemOperators() {
    final Map<String, OpContainer> result = new HashMap<>(SYSTEM_OPERATORS);
    result.putAll(META_SYSTEM_OPERATORS.getMap());
    return result;
  }

  public static Op findSystemOperatorForNameAndType(final String text, final OpType type) {
    OpContainer container = META_SYSTEM_OPERATORS.get(text);

    if (container == null) {
      container = SYSTEM_OPERATORS.get(text);
    }

    Op result = null;

    if (container != null) {
      result = container.findForType(type);
    }

    return result;
  }

  private static int findFirstCharCode(final String text) {
    if (text == null || text.length() != 1) {
      return -1;
    } else {
      return text.charAt(0);
    }
  }

  private boolean isEndOperator(final PrologTerm operator, final OneCharOpMap endOperators) {
    if (operator == null) {
      return true;
    }

    if (endOperators == null) {
      return false;
    }

    return operator.getTermType() == TermType.__OPERATOR_CONTAINER__ && endOperators.containsKey(operator.getTermText());
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
        throw new NoSuchElementException("There are terms in the source.");
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
      while (active) {
        final PrologTerm block = readBlock(OPERATORS_INSIDE_STRUCT);

        if (block == null) {
          return null;
        }

        final TokenizerResult nextAtom = this.tokenizer.readNextToken();
        try {
          final String nextText = nextAtom.getResult().getTermText();

          switch (findFirstCharCode(nextText)) {
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

      result = new PrologStruct(functor, listOfAtoms.toArray(EMPTY));
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

    while (doRead) {
      final PrologTerm block = readBlock(OPERATORS_INSIDE_LIST);

      final TokenizerResult nextAtom = tokenizer.readNextToken();
      try {
        final String text = nextAtom.getResult().getTermText();

        switch (findFirstCharCode(text)) {
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
            try {
              if (!nextAtomTwo.getResult().getTermText().equals(OPERATOR_RIGHTSQUAREBRACKET.getTermText())) {
                throw new PrologParserException("Wrong end of the list tail", tokenizer.getLastTokenLine(), tokenizer.getLastTokenPos());
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
        throw new PrologParserException("There is not any term as the tail at the list", tokenizer.getLastTokenLine(), tokenizer.getLastTokenPos());
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

  private PrologTerm readBlock(final OneCharOpMap endOperators) {
    // the variable will contain last processed tree item contains either
    // atom or operator
    TreeItem currentTreeItem = null;

    while (!Thread.currentThread().isInterrupted()) {
      // read next atom from tokenizer
      TokenizerResult readAtomContainer = this.tokenizer.readNextToken();
      try {
        boolean atBrakes = false;

        if (readAtomContainer == null) {
          if (currentTreeItem == null) {
            // end_of_file
            return null;
          } else {
            // non closed something
            throw new PrologParserException("Not-ended sentence", this.tokenizer.getLastTokenLine(), this.tokenizer.getLastTokenPos());
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

        // the variable contains calculated atem priority (it can be not the
        // same as the nature priority)
        int readAtomPriority = 0; // we make it as zero (the highest
        // priority) default

        switch (readAtom.getTermType()) {
          case __OPERATOR_CONTAINER__: {
            // it is operator list
            // try to get the single operator from the list if the list
            // contains only one
            final Op readOperator = ((OpContainer) readAtom).getOperatorIfSingle();

            // check that the operator is single
            if (readOperator == null) {

              // there are a few operators in the list so we need to
              // select one
              final OpContainer readOperators = (OpContainer) readAtom;

              boolean leftPresented = false;

              if (currentTreeItem != null) {
                if (currentTreeItem.getType() == TermType.__OPERATOR__) {
                  if (currentTreeItem.getRightBranch() != null) {
                    leftPresented = true;
                  }
                } else {
                  leftPresented = true;
                }
              }

              final boolean rightPresented = !isEndOperator(tokenizer.peek().getResult(), endOperators);

              readAtom = readOperators.findSimilar(leftPresented, rightPresented);

              if (readAtom == null) {
                // we didn't get any operator for our criteria, so throw
                // an exception
                throw new PrologParserException("Incompatible operator type [" + readAtomContainer.getResult().getTermText() + ']',
                    readAtomContainer.getLine(), readAtomContainer.getPos());
              }
              // we have found needed operator so get its priority
              readAtomPriority = readAtom.getPrecedence();
            } else {
              readAtom = readOperator;
              final String operatorText = readOperator.getTermText();

              if (operatorText.length() == 1) {
                switch (findFirstCharCode(operatorText)) {
                  case '[': {
                    // it's a list
                    readAtom = readList(readAtomContainer);
                    readAtom.setPos(readAtomContainer.getPos());
                    readAtom.setLine(readAtomContainer.getLine());
                    readAtomPriority = 0;
                  }
                  break;
                  case '(': {
                    // read subblock
                    atBrakes = true;
                    readAtom = readBlock(OPERATORS_SUBBLOCK);
                    if (readAtom == null) {
                      throw new PrologParserException("Illegal start of term",
                          readAtomContainer.getLine(), readAtomContainer.getPos());
                    }
                    readAtom.setPos(readAtomContainer.getPos());
                    readAtom.setLine(readAtomContainer.getLine());
                    readAtomPriority = 0;

                    final TokenizerResult token = tokenizer.readNextToken();
                    final PrologTerm closingAtom = token.getResult();
                    token.release();

                    if (closingAtom == null || !closingAtom.getTermText().equals(OPERATOR_RIGHTBRACKET.getTermText())) {
                      throw new PrologParserException("Non-closed brakes", this.tokenizer.getLine(), this.tokenizer.getPos());
                    }
                  }
                  break;
                  default: {
                    readAtomPriority = readOperator.getPrecedence();
                  }
                  break;
                }
              } else {
                readAtomPriority = readOperator.getPrecedence();
              }
            }
          }
          break;
          case VAR: {
            // it's a variable
            // do nothing
          }
          break;
          default: {
            TokenizerResult nextToken = tokenizer.readNextToken();
            try {
              if (nextToken != null && nextToken.getResult().getTermText().equals(OPERATOR_LEFTBRACKET.getTermText())) {
                final int nextTokenLineNumber = nextToken.getLine();
                final int nextTokenStrPosition = nextToken.getPos();

                // it is a structure
                if (readAtom.getTermType() == TermType.ATOM) {
                  readAtom = readStruct(readAtom);
                  if (readAtom == null) {
                    // we have met the empty brackets, it disallowed by Prolog
                    throw new PrologParserException("Illegal start of term",
                        nextTokenLineNumber, nextTokenStrPosition);
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
                // check read atom to be zero-struct
                if (readAtomContainer.getResult().getTermType() == TermType.ATOM) {
                  if (readAtomContainer.getTokenizerState() == TokenizerState.ATOM && readAtom.getTermText().equals("!")) {
                    readAtom = new PrologStruct("!", readAtomContainer.getLine(), readAtomContainer.getPos());
                  } else if (context != null && context.hasZeroArityStruct(this, readAtom.getTermText())) {
                    readAtom = new PrologStruct(readAtom, readAtomContainer.getLine(), readAtomContainer.getPos());
                  }
                }
              }
            } finally {
              if (nextToken != null) {
                nextToken.release();
              }
            }
          }
          break;
        }

        final TreeItem readAtomTreeItem = this.treeItemPool.find().setData(readAtom,
            atBrakes,
            readAtomContainer.getLine(),
            readAtomContainer.getPos());

        if (currentTreeItem == null) {
          // it's first
          currentTreeItem = readAtomTreeItem;
        } else {
          // not first
          if (currentTreeItem.getType() == TermType.__OPERATOR__) {
            // it's an operator
            if (currentTreeItem.getPriority() <= readAtomPriority) {
              // new has low priority
              // make its as an ascendent
              final TreeItem foundItem = currentTreeItem.findFirstNodeWithSuchOrLowerPriority(readAtomPriority);
              if (foundItem.getPriority() < readAtomPriority) {
                // make as parent
                currentTreeItem = foundItem.makeAsOwnerWithLeftBranch(readAtomTreeItem);
              } else if (foundItem.getPriority() > readAtomPriority) {
                // make new as right subbranch
                currentTreeItem = foundItem.makeAsRightBranch(readAtomTreeItem);
              } else {
                // equals priority
                switch (foundItem.getOperatorType()) {
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

            } else if (currentTreeItem.getPriority() > readAtomPriority) {
              // new has great priority
              if (readAtomTreeItem.getType() != TermType.__OPERATOR__ && currentTreeItem.getRightBranch() != null) {
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
            if (currentTreeItem.getType() != TermType.__OPERATOR__
                && readAtomTreeItem.getType() != TermType.__OPERATOR__) {
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
    return this;
  }

  public Stream<PrologTerm> stream() {
    return StreamSupport.stream(Spliterators.spliteratorUnknownSize(this, Spliterator.ORDERED | Spliterator.IMMUTABLE | Spliterator.NONNULL), false);
  }

  @Override
  public void close() throws IOException {
    this.tokenizer.close();
  }
}
