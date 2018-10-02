package com.igormaznitsa.prologparser.tokenizer;

import com.igormaznitsa.prologparser.CharSource;
import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.OpType;
import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.operators.OperatorDef;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologList;
import com.igormaznitsa.prologparser.terms.PrologStructure;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import com.igormaznitsa.prologparser.utils.ArrayListCache;
import com.igormaznitsa.prologparser.utils.StrBuffer;
import com.igormaznitsa.prologparser.utils.ringbuffer.SoftCache;

import java.io.Closeable;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import static java.util.Arrays.stream;

public abstract class AbstractPrologParser implements Closeable {
  protected static final SingleCharOperatorContainerMap META_SYSTEM_OPERATORS = new SingleCharOperatorContainerMap();
  protected static final Map<String, OperatorContainer> SYSTEM_OPERATORS = new HashMap<>();
  protected static final Set<String> SYSTEM_OPERATORS_PREFIXES = new HashSet<>();

  private final static OperatorContainer OPERATOR_COMMA;
  private final static OperatorContainer OPERATOR_LEFTBRACKET;
  private final static OperatorContainer OPERATOR_RIGHTBRACKET;
  private final static OperatorContainer OPERATOR_LEFTSQUAREBRACKET;
  private final static OperatorContainer OPERATOR_RIGHTSQUAREBRACKET;
  private final static OperatorContainer OPERATOR_DOT;
  private final static OperatorContainer OPERATOR_VERTICALBAR;
  private static final SingleCharOperatorContainerMap OPERATORS_PHRASE;
  private static final SingleCharOperatorContainerMap OPERATORS_INSIDE_LIST;
  private static final SingleCharOperatorContainerMap OPERATORS_END_LIST;
  private static final SingleCharOperatorContainerMap OPERATORS_INSIDE_STRUCT;
  private static final SingleCharOperatorContainerMap OPERATORS_SUBBLOCK;

  static {
    META_SYSTEM_OPERATORS.clear();

    OPERATOR_DOT = new OperatorContainer(Operator.METAOPERATOR_DOT);
    META_SYSTEM_OPERATORS.put(Operator.METAOPERATOR_DOT.getText(), OPERATOR_DOT);

    OPERATOR_LEFTBRACKET = new OperatorContainer(Operator.METAOPERATOR_LEFT_BRACKET);
    META_SYSTEM_OPERATORS.put(Operator.METAOPERATOR_LEFT_BRACKET.getText(), OPERATOR_LEFTBRACKET);

    OPERATOR_RIGHTBRACKET = new OperatorContainer(Operator.METAOPERATOR_RIGHT_BRACKET);
    META_SYSTEM_OPERATORS.put(Operator.METAOPERATOR_RIGHT_BRACKET.getText(), OPERATOR_RIGHTBRACKET);

    OPERATOR_LEFTSQUAREBRACKET = new OperatorContainer(Operator.METAOPERATOR_LEFT_SQUARE_BRACKET);
    META_SYSTEM_OPERATORS.put(Operator.METAOPERATOR_LEFT_SQUARE_BRACKET.getText(), OPERATOR_LEFTSQUAREBRACKET);

    OPERATOR_RIGHTSQUAREBRACKET = new OperatorContainer(Operator.METAOPERATOR_RIGHT_SQUARE_BRACKET);
    META_SYSTEM_OPERATORS.put(Operator.METAOPERATOR_RIGHT_SQUARE_BRACKET.getText(), OPERATOR_RIGHTSQUAREBRACKET);

    OPERATOR_VERTICALBAR = new OperatorContainer(Operator.METAOPERATOR_VERTICAL_BAR);
    META_SYSTEM_OPERATORS.put(Operator.METAOPERATOR_VERTICAL_BAR.getText(), OPERATOR_VERTICALBAR);

    registerAsSystemOperators(OperatorDef.of(1000, OpType.XFY, ","));

    SYSTEM_OPERATORS_PREFIXES.add(Operator.METAOPERATOR_DOT.getText());
    SYSTEM_OPERATORS_PREFIXES.add(Operator.METAOPERATOR_LEFT_BRACKET.getText());
    SYSTEM_OPERATORS_PREFIXES.add(Operator.METAOPERATOR_LEFT_SQUARE_BRACKET.getText());
    SYSTEM_OPERATORS_PREFIXES.add(Operator.METAOPERATOR_RIGHT_BRACKET.getText());
    SYSTEM_OPERATORS_PREFIXES.add(Operator.METAOPERATOR_RIGHT_SQUARE_BRACKET.getText());
    SYSTEM_OPERATORS_PREFIXES.add(Operator.METAOPERATOR_VERTICAL_BAR.getText());

    OPERATOR_COMMA = SYSTEM_OPERATORS.get(",");

    OPERATORS_PHRASE = new SingleCharOperatorContainerMap(OPERATOR_DOT);
    OPERATORS_INSIDE_LIST = new SingleCharOperatorContainerMap(OPERATOR_COMMA, OPERATOR_RIGHTSQUAREBRACKET, OPERATOR_VERTICALBAR);
    OPERATORS_END_LIST = new SingleCharOperatorContainerMap(OPERATOR_RIGHTSQUAREBRACKET);
    OPERATORS_INSIDE_STRUCT = new SingleCharOperatorContainerMap(OPERATOR_COMMA, OPERATOR_RIGHTBRACKET);
    OPERATORS_SUBBLOCK = new SingleCharOperatorContainerMap(OPERATOR_RIGHTBRACKET);
  }

  private final SoftCache<TermWrapper> cacheForTermWrappers = new SoftCache<>(TermWrapper::new, 256);
  private final ArrayListCache<AbstractPrologTerm> abstractPrologTermListCache = new ArrayListCache<>();
  private final SoftCache<TreeItem> itemCache = new SoftCache<>(() -> new TreeItem(this), 128);
  private final Tokenizer tokenizer = new Tokenizer();
  private final ParserContext context;
  private CharSource charSource;

  public AbstractPrologParser(final ParserContext nullableContext) {
    this.context = nullableContext;
  }

  protected static void registerAsSystemOperators(final OperatorDef... operators) {
    final StrBuffer buff = new StrBuffer(10);
    stream(operators).filter(Objects::nonNull).forEach(x -> stream(x.getNames()).forEach(n -> {
      if (SYSTEM_OPERATORS.containsKey(n)) {
        final OperatorContainer container = SYSTEM_OPERATORS.get(n);
        container.addOperator(Operator.makeOperator(x.getPrecedence(), x.getType(), n));
      } else {
        final OperatorContainer container = new OperatorContainer(Operator.makeOperator(x.getPrecedence(), x.getType(), n));
        SYSTEM_OPERATORS.put(n, container);
      }
      buff.clear();
      for (final char c : n.toCharArray()) {
        buff.append(c);
        SYSTEM_OPERATORS_PREFIXES.add(buff.toString());
      }
    }));
  }

  public static Map<String, OperatorContainer> collectSystemOperators() {
    final Map<String, OperatorContainer> result = new HashMap<>(SYSTEM_OPERATORS);
    result.putAll(META_SYSTEM_OPERATORS.getMap());
    return result;
  }

  public static Operator findSystemOperatorForNameAndType(final String text, final OpType type) {
    OperatorContainer container = META_SYSTEM_OPERATORS.get(text);
    if (container == null) {
      container = SYSTEM_OPERATORS.get(text);
    }

    Operator result = null;

    if (container != null) {
      result = container.getOperatorForType(type);
    }

    return result;
  }

  private static int getFirstCharIfItIsSingle(final String text) {
    if (text == null || text.length() != 1) {
      return 0x1FFFF;
    } else {
      return text.charAt(0);
    }
  }

  private boolean isEndOperator(final AbstractPrologTerm operator, final SingleCharOperatorContainerMap endOperators) {
    if (operator == null) {
      return true;
    }

    if (endOperators == null) {
      return false;
    }

    return operator.getType() == PrologTermType.OPERATORS && endOperators.containsKey(operator.getText());
  }

  public ParserContext getContext() {
    return context;
  }

  public CharSource getCharSource() {
    return this.charSource;
  }

  public AbstractPrologTerm nextSentence(final String str) throws IOException {
    return this.nextSentence(CharSource.of(str));
  }

  public AbstractPrologTerm nextSentence(final CharSource reader) throws IOException {
    charSource = reader;
    return this.nextSentence();
  }

  public AbstractPrologTerm nextSentence() throws IOException {

    final AbstractPrologTerm result = readBlock(OPERATORS_PHRASE);
    if (result == null) {
      return null; // end_of_file
    }
    final TokenizerResult endAtom = tokenizer.nextToken(this.charSource, this);
    if (endAtom == null || !endAtom.getResult().getText().equals(OPERATOR_DOT.getText())) {
      throw new PrologParserException("End operator is not found", this.charSource.getLineNum(),
          this.charSource.getStrPos());
    }
    endAtom.release();
    return result;
  }

  private PrologStructure readStruct(final AbstractPrologTerm functor) throws IOException {
    final List<AbstractPrologTerm> listOfAtoms = abstractPrologTermListCache.getListFromCache();
    PrologStructure result;
    try {
      while (true) {
        final AbstractPrologTerm block = readBlock(OPERATORS_INSIDE_STRUCT);

        if (block == null) {
          return null;
        }

        final TokenizerResult nextAtom = tokenizer.nextToken(charSource, this);
        final String nextText = nextAtom.getResult().getText();
        nextAtom.release();

        final int firstCharCode = getFirstCharIfItIsSingle(nextText);

        if ((int) ',' == firstCharCode) {
          // next item
          listOfAtoms.add(block);
        } else if ((int) ')' == firstCharCode) {
          // end of the structure
          listOfAtoms.add(block);
          break;
        }
      }

      result = new PrologStructure(functor,
          listOfAtoms.toArray(new AbstractPrologTerm[listOfAtoms.size()]));
    } finally {
      abstractPrologTermListCache.putListToCache(listOfAtoms);
    }
    return result;
  }

  private AbstractPrologTerm readList(final TokenizerResult openingBracket) throws IOException {
    PrologList leftPart = new PrologList();
    PrologList leftPartFirst = leftPart;
    AbstractPrologTerm rightPart = null;

    boolean hasSeparator = false;

    boolean doRead = true;

    TokenizerResult nextAtom = null;
    while (doRead) {
      final AbstractPrologTerm block = readBlock(OPERATORS_INSIDE_LIST);

      if (nextAtom != null) {
        nextAtom.release();
      }
      nextAtom = tokenizer.nextToken(charSource, this);
      final String text = nextAtom.getResult().getText();

      final int singleCharCode = getFirstCharIfItIsSingle(text);

      if ((int) ']' == singleCharCode) {
        // end
        doRead = false;
        if (block == null) {
          continue;
        }
      } else if ((int) '|' == singleCharCode) {
        // we have found the list tail, so we need read it as one block
        // until the ']' atom
        checkForNull(block, "There is not any list element", openingBracket);
        if (leftPartFirst.isNullList()) {
          leftPartFirst = PrologList.setTermAsNewListTail(leftPart, block);
        } else {
          PrologList.setTermAsNewListTail(leftPart, block);
        }

        hasSeparator = true;

        rightPart = readBlock(OPERATORS_END_LIST);

        if (rightPart != null
            && rightPart.getType() == PrologTermType.STRUCT
            && ((PrologStructure) rightPart).getFunctor().getText().equals(OPERATOR_VERTICALBAR.getText())) {
          throw new PrologParserException(
              "Duplicated list tail definition",
              tokenizer.getLastTokenLineNum(),
              tokenizer.getLastTokenStrPos());
        }

        nextAtom.release();
        nextAtom = tokenizer.nextToken(charSource, this);
        if (!nextAtom.getResult().getText().equals(OPERATOR_RIGHTSQUAREBRACKET.getText())) {
          throw new PrologParserException(
              "Wrong end of the list tail",
              tokenizer.getLastTokenLineNum(),
              tokenizer.getLastTokenStrPos());
        }
        break;
      } else if ((int) ',' == singleCharCode) {
        // all good and we read next block
        checkForNull(block, "List element not found", nextAtom);
      } else {
        throw new CriticalUnexpectedError();
      }

      if (leftPartFirst.isNullList()) {
        leftPartFirst = PrologList.setTermAsNewListTail(leftPart, block);
        leftPart = leftPartFirst;
      } else {
        leftPart = PrologList.setTermAsNewListTail(leftPart, block);
      }
    }

    if (hasSeparator) {
      // '|' separator was found at the list
      if (rightPart == null) {
        throw new PrologParserException(
            "There is not any term as the tail at the list",
            tokenizer.getLastTokenLineNum(),
            tokenizer.getLastTokenStrPos());
      }
      leftPartFirst.replaceLastElement(rightPart);
    }
    return leftPartFirst;
  }

  private void checkForNull(final Object obj, final String message, final TokenizerResult startTerm) throws PrologParserException {
    if (obj == null) {
      throw new PrologParserException(message,
          startTerm.getLineNumber(),
          startTerm.getStringPosition());
    }
  }

  private AbstractPrologTerm readBlock(final SingleCharOperatorContainerMap endOperators) throws IOException {
    // the variable will contain last processed tree item contains either
    // atom or operator
    TreeItem currentTreeItem = null;

    TokenizerResult readAtomContainer = null;
    while (true) {
      // read next atom from tokenizer
      if (readAtomContainer != null) {
        readAtomContainer.release();
      }
      readAtomContainer = tokenizer.nextToken(
          charSource, this);
      boolean atBrakes = false;

      if (readAtomContainer == null) {
        if (currentTreeItem == null) {
          // end_of_file
          return null;
        } else {
          // non closed something
          throw new PrologParserException("Not-ended phrase",
              tokenizer.getLastTokenLineNum(),
              tokenizer.getLastTokenStrPos());
        }
      }

      AbstractPrologTerm readAtom = readAtomContainer.getResult();

      // check the atom to be the end atom
      if (isEndOperator(readAtom, endOperators)) {
        // it's an end atom so we push it back and end the cycle
        tokenizer.push(readAtomContainer);
        break;
      }

      // the variable contains calculated atem priority (it can be not the
      // same as the nature priority)
      int readAtomPriority = 0; // we make it as zero (the highest
      // priority) default

      switch (readAtom.getType()) {
        case OPERATORS: {
          // it is operator list
          // try to get the single operator from the list if the list
          // contains only one
          final Operator readOperator = ((OperatorContainer) readAtom).getOperatorIfSingle();

          // check that the operator is single
          if (readOperator == null) {

            // there are a few operators in the list so we need to
            // select one
            final OperatorContainer readOperators = (OperatorContainer) readAtom;

            boolean leftPresented = false;

            if (currentTreeItem != null) {
              if (currentTreeItem.getType() == PrologTermType.OPERATOR) {
                if (currentTreeItem.getRightBranch() != null) {
                  leftPresented = true;
                }
              } else {
                leftPresented = true;
              }
            }

            final boolean rightPresented = !isEndOperator(tokenizer.peek(charSource, this).getResult(), endOperators);

            readAtom = readOperators.findCompatibleOperator(leftPresented, rightPresented);

            if (readAtom == null) {
              // we didn't get any operator for our criteria, so throw
              // an exception

              throw new PrologParserException("Incompatible operator type",
                  readAtomContainer.getLineNumber(), readAtomContainer.getStringPosition());
            }
            // we have found needed operator so get its priority
            readAtomPriority = readAtom.getPrecedence();
          } else {
            readAtom = readOperator;
            final String operatorText = readOperator.getText();
            if (operatorText.length() == 1) {
              final int firstSignleChar = getFirstCharIfItIsSingle(operatorText);
              if ((int) '[' == firstSignleChar) {
                // it's a list
                readAtom = readList(readAtomContainer);
                readAtom.setStrPosition(readAtomContainer.getStringPosition());
                readAtom.setLineNumber(readAtomContainer.getLineNumber());
                readAtomPriority = 0;
              } else if ((int) '(' == firstSignleChar) {
                // read subblock
                atBrakes = true;
                readAtom = readBlock(OPERATORS_SUBBLOCK);
                if (readAtom == null) {
                  throw new PrologParserException("Illegal start of term",
                      readAtomContainer.getLineNumber(), readAtomContainer.getStringPosition());
                }
                readAtom.setStrPosition(readAtomContainer.getStringPosition());
                readAtom.setLineNumber(readAtomContainer.getLineNumber());
                readAtomPriority = 0;

                final TokenizerResult token = tokenizer.nextToken(charSource, this);
                final AbstractPrologTerm closingAtom = token.getResult();
                token.release();

                if (closingAtom == null || !closingAtom.getText().equals(OPERATOR_RIGHTBRACKET.getText())) {
                  throw new PrologParserException("Non-closed brakes", charSource.getLineNum(),
                      charSource.getStrPos());
                }
              } else {
                readAtomPriority = readOperator.getPrecedence();
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
          final TokenizerResult nextToken = tokenizer.nextToken(charSource, this);
          if (nextToken != null && nextToken.getResult().getText().equals(OPERATOR_LEFTBRACKET.getText())) {
            final int nextTokenLineNumber = nextToken.getLineNumber();
            final int nextTokenStrPosition = nextToken.getStringPosition();

            // it is a structure
            if (readAtom.getType() == PrologTermType.ATOM) {
              nextToken.release();
              readAtom = readStruct(readAtom);
              if (readAtom == null) {
                // we have met the empty brackets, it disallowed by Prolog
                throw new PrologParserException("Illegal start of term",
                    nextTokenLineNumber, nextTokenStrPosition);
              }
            } else {
              tokenizer.push(nextToken);
              throw new PrologParserException("You must have an atom as the structure functor",
                  nextTokenLineNumber, nextTokenStrPosition);
            }
          } else {
            // push back the next atom
            tokenizer.push(nextToken);

            // check read atom to be zero-struct
            if (readAtomContainer.getResult().getType() == PrologTermType.ATOM) {
              if (readAtomContainer.getTokenizerState() == TokenizerState.ATOM && readAtom.getText().equals("!")) {
                readAtom = new PrologStructure("!", readAtomContainer.getStringPosition(), readAtomContainer.getLineNumber());
              } else if (context != null && context.hasZeroArityPredicate(this, readAtom.getText())) {
                readAtom = new PrologStructure(readAtom, readAtomContainer.getStringPosition(), readAtomContainer.getLineNumber());
              }
            }
          }

        }
        break;
      }

      final TreeItem readAtomTreeItem = itemCache.get();
      readAtomTreeItem.setData(cacheForTermWrappers, readAtom,
          atBrakes,
          readAtomContainer.getLineNumber(),
          readAtomContainer.getStringPosition());

      if (currentTreeItem == null) {
        // it's first
        currentTreeItem = readAtomTreeItem;
      } else {
        // not first
        if (currentTreeItem.getType() == PrologTermType.OPERATOR) {
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
            if (readAtomTreeItem.getType() != PrologTermType.OPERATOR && currentTreeItem.getRightBranch() != null) {
              // it's a ground atom and its right branch is not empty
              throw new PrologParserException(
                  "There is no any operator before the atom",
                  readAtomContainer.getLineNumber(),
                  readAtomContainer.getStringPosition());
            }
            // make it as right
            currentTreeItem = currentTreeItem.makeAsRightBranch(readAtomTreeItem);
          }
        } else {
          // check that it is an operator
          if (currentTreeItem.getType() != PrologTermType.OPERATOR
              && readAtomTreeItem.getType() != PrologTermType.OPERATOR) {
            throw new PrologParserException(
                "There must be an operator between atoms or structures",
                readAtomContainer.getLineNumber(),
                readAtomContainer.getStringPosition());
          }

          // make it as left branch
          currentTreeItem = currentTreeItem.makeAsOwnerWithLeftBranch(readAtomTreeItem);
        }
      }
    }
    if (currentTreeItem == null) {
      return null;
    } else {
      return currentTreeItem.findRoot().convertTreeItemIntoTerm();
    }
  }

  @Override
  public void close() throws IOException {
    if (this.charSource != null) {
      this.charSource.close();
      this.charSource = null;
    }
  }

}
