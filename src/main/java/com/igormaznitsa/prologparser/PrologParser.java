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

import static com.igormaznitsa.prologparser.ParserContext.FLAG_DOT2_AS_LIST;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_NONE;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_VAR_AS_FUNCTOR;
import static com.igormaznitsa.prologparser.ParserContext.FLAG_ZERO_STRUCT;
import static com.igormaznitsa.prologparser.tokenizer.Op.METAOPERATOR_COMMA;
import static com.igormaznitsa.prologparser.tokenizer.Op.METAOPERATOR_VERTICAL_BAR;
import static com.igormaznitsa.prologparser.utils.Koi7CharOpMap.ofOps;
import static java.util.Objects.requireNonNull;

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
import com.igormaznitsa.prologparser.tokenizer.Tokenizer;
import com.igormaznitsa.prologparser.tokenizer.TokenizerResult;
import com.igormaznitsa.prologparser.utils.Koi7CharOpMap;
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

/**
 * Abstract base Prolog parser.
 */
@SuppressWarnings({"GrazieInspection", "unused"})
public abstract class PrologParser implements Iterable<PrologTerm>, AutoCloseable {

  public static final PrologTerm[] EMPTY_TERM_ARRAY = new PrologTerm[0];
  protected static final Koi7CharOpMap META_OP_MAP;
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
    OPERATOR_COMMA = META_OP_MAP.add(METAOPERATOR_COMMA);

    OPERATORS_PHRASE = ofOps(OPERATOR_DOT);
    OPERATORS_INSIDE_LIST =
        ofOps(OPERATOR_COMMA, OPERATOR_RIGHTSQUAREBRACKET, OPERATOR_VERTICALBAR);
    OPERATORS_END_LIST = ofOps(OPERATOR_RIGHTSQUAREBRACKET);
    OPERATORS_INSIDE_STRUCT = ofOps(OPERATOR_COMMA, OPERATOR_RIGHTBRACKET);
    OPERATORS_SUBBLOCK = ofOps(OPERATOR_RIGHTBRACKET);
    OPERATORS_SUBBLOCK_CURLY = ofOps(OPERATOR_RIGHTCURLYBRACKET);
  }

  protected final ParserContext context;
  protected final int parserFlags;
  private final Tokenizer tokenizer;
  private final List<TokenizedCommentListener> commentTokenListeners;
  private PrologTermReadResult deferredReadTerm;

  protected PrologParser(
      final Reader source,
      final ParserContext context,
      final List<TokenizedCommentListener> tokenizedCommentListeners
  ) {
    this.context = context == null ? DefaultParserContext.of(ParserContext.FLAG_NONE) : context;
    this.parserFlags = context == null ? FLAG_NONE : context.getFlags();
    this.tokenizer = new Tokenizer(this, META_OP_MAP, requireNonNull(source));
    this.commentTokenListeners = List.copyOf(tokenizedCommentListeners);
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

  private static boolean isComment(final TokenizerResult tokenizerResult) {
    return tokenizerResult != null
        && tokenizerResult.getResult().getType() == TermType.ATOM
        && (tokenizerResult.getResult().getQuotation() == Quotation.COMMENT_LINE
        || tokenizerResult.getResult().getQuotation() == Quotation.COMMENT_BLOCK);
  }

  /**
   * Access to the internal tokenizer, it is mainly for test purposes because parser makes reading into internal buffers.
   *
   * @return the internal tokenizer in use by the parser
   */
  public Tokenizer getInternalTokenizer() {
    return this.tokenizer;
  }

  private boolean isEndOperator(final PrologTerm operator, final Koi7CharOpMap endOperators) {
    if (operator == null) {
      return true;
    }

    if (endOperators == null) {
      return false;
    }

    return operator.getType() == TermType.OPERATOR && endOperators.contains(operator.getText());
  }

  public ParserContext getContext() {
    return context;
  }

  private void notifyCommentTokenListeners(final TokenizerResult commentToken) {
    for (final TokenizedCommentListener listener : this.commentTokenListeners) {
      listener.onCommentToken(this, commentToken);
    }
  }

  private TokenizerResult extractNextTokenCommentAware() {
    TokenizerResult result;
    while (true) {
      result = this.tokenizer.readNextToken();
      if (isComment(result)) {
        this.notifyCommentTokenListeners(result);
      } else {
        break;
      }
    }
    return result;
  }

  private PrologTermReadResult extractNextBlockAndWrapError() {
    try {
      final PrologTerm found = this.readBlock(OPERATORS_PHRASE);
      if (found == null) {
        throw new NoSuchElementException("No terms in source");
      } else {
        final TokenizerResult endAtom = this.extractNextTokenCommentAware();
        if (endAtom == null || !endAtom.getResult().getText().equals(OPERATOR_DOT.getText())) {
          throw new PrologParserException("End operator is not found",
              this.tokenizer.getLine(),
              this.tokenizer.getPos());
        }
      }
      return new PrologTermReadResult(found, null);
    } catch (RuntimeException ex) {
      return new PrologTermReadResult(null, ex);
    }
  }

  public boolean hasNext() {
    if (this.deferredReadTerm == null) {
      this.deferredReadTerm = this.extractNextBlockAndWrapError();
    }
    return this.deferredReadTerm.isPresented();
  }

  public PrologTerm next() {
    if (this.deferredReadTerm == null) {
      this.deferredReadTerm = this.extractNextBlockAndWrapError();
    }
    final PrologTerm result;
    try {
      result = this.deferredReadTerm.getResult();
    } finally {
      this.deferredReadTerm = null;
    }
    return result;
  }

  private PrologStruct readStruct(final PrologTerm functor) {
    final List<PrologTerm> listOfAtoms = new ArrayList<>();
    PrologStruct result;
    boolean active = true;
    while (active) {
      final PrologTerm block = this.readBlock(OPERATORS_INSIDE_STRUCT);
      if (block == null) {
        return null;
      }

      final TokenizerResult nextAtom = this.extractNextTokenCommentAware();
      if (nextAtom == null) {
        throw new PrologParserException("Can't read next token in block", this.tokenizer.getLine(),
            this.tokenizer.getPos());
      }

      final String nextText = nextAtom.getResult().getText();

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
          throw new PrologParserException("Unexpected term in structure: " + nextText,
              nextAtom.getLine(), nextAtom.getPos());
      }
    }

    result = new PrologStruct(functor, listOfAtoms.toArray(EMPTY_TERM_ARRAY));
    return result;
  }

  private PrologTerm readList(final TokenizerResult openingBracket) {
    PrologList leftPart = new PrologList();
    PrologList leftPartFirst = leftPart;
    PrologTerm rightPart = null;

    boolean hasSeparator = false;
    boolean continueReading = true;

    while (continueReading) {
      final PrologTerm block = readBlock(OPERATORS_INSIDE_LIST);

      final TokenizerResult nextAtom = this.extractNextTokenCommentAware();
      if (nextAtom == null) {
        throw new PrologParserException("Can't read next token in list", this.tokenizer.getLine(),
            this.tokenizer.getPos());
      }

      final String text = nextAtom.getResult().getText();

      switch (getOnlyCharCode(text)) {
        case ']': {
          continueReading = false;
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
              && rightPart.getType() == TermType.STRUCT
              && rightPart.getFunctor().getText().equals(OPERATOR_VERTICALBAR.getText())) {
            throw new PrologParserException(
                "Duplicated list tail definition",
                tokenizer.getLastTokenLine(),
                tokenizer.getLastTokenPos(), null);
          }

          final TokenizerResult nextAtomTwo = this.extractNextTokenCommentAware();
          if (nextAtomTwo == null) {
            throw new PrologParserException("Can't find expected token in list",
                this.tokenizer.getLine(), this.tokenizer.getPos());
          }
          if (!nextAtomTwo.getResult().getText().equals(OPERATOR_RIGHTSQUAREBRACKET.getText())) {
            throw new PrologParserException("Wrong end of the list tail",
                this.tokenizer.getLastTokenLine(), this.tokenizer.getLastTokenPos());
          }
          continueReading = false;
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
    }

    if (hasSeparator) {
      // '|' separator was found at the list
      if (rightPart == null) {
        throw new PrologParserException("There is not any term as the tail at the list",
            this.tokenizer.getLastTokenLine(), this.tokenizer.getLastTokenPos());
      }

      if (rightPart.getType() == TermType.STRUCT
          && (rightPart.getFunctor() == METAOPERATOR_COMMA
          || rightPart.getFunctor() == METAOPERATOR_VERTICAL_BAR)
      ) {
        throw new PrologParserException("Unexpected comma or bar in rest of list",
            this.tokenizer.getLastTokenLine(), this.tokenizer.getLastTokenPos());
      }

      if (rightPart.getType() == TermType.ATOM
          && rightPart.getQuotation() == Quotation.NONE
          && ",".equals(rightPart.getText())
      ) {
        throw new PrologParserException("Comma operator in list tail",
            this.tokenizer.getLastTokenLine(), this.tokenizer.getLastTokenPos());
      }

      leftPartFirst.replaceEndListElement(rightPart);
    }
    return leftPartFirst;
  }

  private PrologTerm readBlock(final Koi7CharOpMap endOperators) {
    // the variable will contain last processed tree item contains either
    // atom or operator
    AstItem currentTreeItem = null;

    while (true) {
      // read next atom from tokenizer
      TokenizerResult readAtomContainer = this.extractNextTokenCommentAware();

      if (readAtomContainer == null) {
        if (currentTreeItem == null) {
          // end_of_file
          return null;
        } else {
          // non closed something
          throw new PrologParserException("Non-ended clause", this.tokenizer.getLastTokenLine(),
              this.tokenizer.getLastTokenPos());
        }
      }

      PrologTerm readAtom = readAtomContainer.getResult();

      // check the atom to be the end atom
      if (isEndOperator(readAtom, endOperators)) {
        // it's an end atom so we push it back and end the cycle
        this.tokenizer.push(readAtomContainer);
        break;
      }

      // the variable contains calculated item precedence (it can be not the
      // same as the natural precedence)
      int readAtomPrecedence = 0; // we make it as highest precedence

      if (readAtom instanceof OpContainer) {
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
          final boolean rightPresented =
              peekResult != null && !isEndOperator(peekResult.getResult(), endOperators);

          readAtom = readOperators.findSimilar(leftPresented, rightPresented);

          if (readAtom == null) {
            if (currentTreeItem == null && !rightPresented) {
              // alone operator, it is an atom
              return new PrologAtom(readOperators.getText(), Quotation.SINGLE,
                  readOperators.getLine(), readOperators.getPos());
            }
            // we didn't get any operator for our criteria, so throw
            // an exception
            throw new PrologParserException(
                "Operator clash detected [" + readAtomContainer.getResult().getText() + ']',
                readAtomContainer.getLine(), readAtomContainer.getPos());
          }
          // we have found needed operator so get its precedence
          readAtomPrecedence = readAtom.getPrecedence();
        } else {
          readAtom = readOperator;
          final String operatorText = readOperator.getText();

          if (operatorText.length() == 1) {
            final int onlyCharCode = getOnlyCharCode(operatorText);
            switch (onlyCharCode) {
              case '[': {
                // it's a list
                readAtom = readList(readAtomContainer);
                readAtom.setPos(readAtomContainer.getPos());
                readAtom.setLine(readAtomContainer.getLine());
              }
              break;
              case '{':
              case '(': {
                boolean processReadAtom = true;
                if (onlyCharCode == '(') {
                  readAtom = readBlock(OPERATORS_SUBBLOCK);
                } else {
                  if ((this.parserFlags & ParserContext.FLAG_CURLY_BRACKETS) == 0) {
                    readAtomPrecedence = readOperator.getPrecedence();
                    processReadAtom = false;
                  } else {
                    readAtom = readBlock(OPERATORS_SUBBLOCK_CURLY);
                  }
                }

                if (processReadAtom) {
                  if (readAtom == null) {
                    if (onlyCharCode == '{') {
                      readAtom = new PrologStruct(Op.VIRTUAL_OPERATOR_CURLY_BLOCK, EMPTY_TERM_ARRAY,
                          readAtomContainer.getLine(), readAtomContainer.getPos());
                    } else {
                      throw new PrologParserException("Illegal start of term",
                          readAtomContainer.getLine(), readAtomContainer.getPos());
                    }
                  } else {
                    readAtom.setLine(readAtomContainer.getLine());
                    readAtom.setPos(readAtomContainer.getPos());
                    readAtom = new PrologStruct(
                        onlyCharCode == '{' ? Op.VIRTUAL_OPERATOR_CURLY_BLOCK :
                            Op.VIRTUAL_OPERATOR_BLOCK, new PrologTerm[] {readAtom},
                        readAtomContainer.getLine(), readAtomContainer.getPos());
                  }

                  final TokenizerResult token = this.extractNextTokenCommentAware();

                  final PrologTerm closingAtom;
                  if (token == null) {
                    closingAtom = null;
                  } else {
                    closingAtom = token.getResult();
                  }

                  if (closingAtom == null || !closingAtom.getText().equals(
                      (onlyCharCode == '{' ? OPERATOR_RIGHTCURLYBRACKET :
                          OPERATOR_RIGHTBRACKET).getText())) {
                    throw new PrologParserException("Non-closed brackets: " + onlyCharCode,
                        this.tokenizer.getLine(), this.tokenizer.getPos());
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
        if (readAtom.getType() != TermType.VAR || (this.parserFlags & FLAG_VAR_AS_FUNCTOR) != 0) {
          TokenizerResult nextToken = this.extractNextTokenCommentAware();

          if (nextToken == null) {
            throw new PrologParserException("Non-closed clause", this.tokenizer.getLastTokenLine(),
                this.tokenizer.getLastTokenPos());
          }

          if (nextToken.getResult().getText().equals(OPERATOR_LEFTBRACKET.getText())) {
            final int nextTokenLineNumber = nextToken.getLine();
            final int nextTokenStrPosition = nextToken.getPos();

            // it is a structure
            if (readAtom.getType() == TermType.ATOM
                || (readAtom.getType() == TermType.VAR
                && (this.parserFlags & FLAG_VAR_AS_FUNCTOR) != 0)) {

              final PrologTerm prev = readAtom;
              readAtom = readStruct(readAtom);
              if (readAtom == null) {
                // we have met the empty brackets
                if ((this.parserFlags & FLAG_ZERO_STRUCT) == 0) {
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
              throw new PrologParserException("You must have an atom as the structure functor",
                  nextTokenLineNumber, nextTokenStrPosition);
            }
          } else {
            // push back the next atom
            tokenizer.push(nextToken);
          }
        }
      }

      final AstItem readAtomTreeItem = new AstItem(readAtom,
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
            if (readAtom.getType() == TermType.OPERATOR && ((Op) readAtom).getAssoc().isPrefix()) {
              // it is a prefix operator so that it can be there
              currentTreeItem = currentTreeItem.makeAsRightBranch(readAtomTreeItem);
            } else {
              // new has lower or equal precedence
              // make it as ascendant one
              final AstItem foundItem =
                  currentTreeItem.findFirstNodeWithSuchOrLowerPrecedence(readAtomPrecedence);
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
            if (readAtomTreeItem.getType() != TermType.OPERATOR
                && currentTreeItem.getRightBranch() != null) {
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
    }
    if (currentTreeItem == null) {
      return null;
    } else {
      PrologTerm result = currentTreeItem.findRoot().convertToTermAndRelease(this);
      if ((this.parserFlags & FLAG_DOT2_AS_LIST) != 0
          && result.getType() == TermType.STRUCT
          && result.getText().equals(".")
          && result.getArity() == 2) {
        final PrologStruct asStruct = (PrologStruct) result;
        result = new PrologList(asStruct.getTermAt(0), asStruct.getTermAt(1));
      }
      return result;
    }
  }

  private void checkForNull(final Object obj, final String message,
                            final TokenizerResult startTerm) {
    if (obj == null) {
      throw new PrologParserException(message, startTerm.getLine(), startTerm.getPos());
    }
  }

  @Override
  public void close() throws IOException {
    this.deferredReadTerm = null;
    this.tokenizer.close(this.isCloseReader());
  }

  /**
   * Returns flag to close the base reader. By default the reader will be closed.
   *
   * @return true if close base reader during its close, false to not close reader
   * @see PrologParser#close()
   * @since 2.2.0
   */
  protected boolean isCloseReader() {
    return true;
  }

  @Override
  public Iterator<PrologTerm> iterator() {
    return new Iterator<>() {
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
            this.iterator(),
            Spliterator.ORDERED | Spliterator.IMMUTABLE | Spliterator.NONNULL),
        false
    );
  }

  private static final class PrologTermReadResult {
    private final PrologTerm result;
    private final RuntimeException exception;

    private PrologTermReadResult(final PrologTerm result, final RuntimeException error) {
      this.result = result;
      this.exception = error;
    }

    boolean isPresented() {
      return this.exception == null || !(this.exception instanceof NoSuchElementException);
    }

    PrologTerm getResult() {
      if (this.exception == null) {
        return this.result;
      } else {
        throw this.exception;
      }
    }
  }

}
