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

import com.igormaznitsa.prologparser.annotations.PrologOperator;
import com.igormaznitsa.prologparser.annotations.PrologOperators;
import com.igormaznitsa.prologparser.exceptions.CriticalSoftwareDefectError;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.operators.OperatorType;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologList;
import com.igormaznitsa.prologparser.terms.PrologStructure;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import com.igormaznitsa.prologparser.utils.Assert;
import com.igormaznitsa.prologparser.utils.FastStringBuilder;
import com.igormaznitsa.prologparser.utils.ThreadNonSafeArrayListCache;
import com.igormaznitsa.prologparser.utils.ringbuffer.SoftCache;
import com.igormaznitsa.prologparser.utils.ringbuffer.SoftCacheItemFactory;

import java.io.IOException;
import java.util.*;

/**
 * The class is a minimal hand-written prolog parser allows to parse incoming
 * char stream and make prolog structures. The Class process only minimal set of
 * base system operators - '(',')','|','[',']','.',','. All Extra operators can
 * be defined either through context or class successor. NB! Base system
 * operators are saved in static structures, use only parser class per JVM!
 * Don't try to make several successors of the class with different system
 * operators defined through annotations!
 * <p>
 * The class is thread safe.
 *
 * @since 1.3.3
 */
@SuppressWarnings("serial")
@PrologOperators(Operators = {
        @PrologOperator(Priority = 1000, Type = OperatorType.XFY, Name = ",")})
public abstract class AbstractPrologParser implements SoftCacheItemFactory<ParserTreeItem> {

    /**
     * The Static map contains all system meta-operators for the parser (brackets,
     * dot, vertical bar).
     */
    static final SingleCharOperatorContainerMap META_SYSTEM_OPERATORS = new SingleCharOperatorContainerMap();
    private static final int INSIDE_TABLE_CAPACITY = 0x300;
    private static final float LOAD_FACTOR = 0.75f;
    /**
     * The Static map contains all system operators are being used by the parser.
     */
    static final Map<String, OperatorContainer> SYSTEM_OPERATORS = new HashMap<String, OperatorContainer>(INSIDE_TABLE_CAPACITY, LOAD_FACTOR);
    /**
     * The set contains all possible prefixes of system operators to expedite
     * parsing.
     */
    static final Set<String> SYSTEM_OPERATORS_PREFIXES = new HashSet<String>(INSIDE_TABLE_CAPACITY, LOAD_FACTOR);
    /**
     * Inside link to the system ',' operator.
     */
    private final static OperatorContainer OPERATOR_COMMA;
    /**
     * Inside link to the system '(' operator.
     */
    private final static OperatorContainer OPERATOR_LEFTBRACKET;
    /**
     * Inside link to the system ')' operator.
     */
    private final static OperatorContainer OPERATOR_RIGHTBRACKET;
    /**
     * Inside link to the system '[' operator.
     */
    private final static OperatorContainer OPERATOR_LEFTSQUAREBRACKET;
    /**
     * Inside link to the system ']' operator.
     */
    private final static OperatorContainer OPERATOR_RIGHTSQUAREBRACKET;
    /**
     * Inside link to the system '.' operator.
     */
    private final static OperatorContainer OPERATOR_DOT;
    /**
     * Inside link to the '|' operator.
     */
    private final static OperatorContainer OPERATOR_VERTICALBAR;

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

        SYSTEM_OPERATORS.clear();
        SYSTEM_OPERATORS_PREFIXES.clear();

        SYSTEM_OPERATORS_PREFIXES.add(Operator.METAOPERATOR_DOT.getText());
        SYSTEM_OPERATORS_PREFIXES.add(Operator.METAOPERATOR_LEFT_BRACKET.getText());
        SYSTEM_OPERATORS_PREFIXES.add(Operator.METAOPERATOR_LEFT_SQUARE_BRACKET.getText());
        SYSTEM_OPERATORS_PREFIXES.add(Operator.METAOPERATOR_RIGHT_BRACKET.getText());
        SYSTEM_OPERATORS_PREFIXES.add(Operator.METAOPERATOR_RIGHT_SQUARE_BRACKET.getText());
        SYSTEM_OPERATORS_PREFIXES.add(Operator.METAOPERATOR_VERTICAL_BAR.getText());

        readSystemOperators(AbstractPrologParser.class);
        OPERATOR_COMMA = SYSTEM_OPERATORS.get(",");
    }

    /**
     * Inside array of operators which will be used to read a prolog phrase.
     */
    private static final SingleCharOperatorContainerMap OPERATORS_PHRASE = new SingleCharOperatorContainerMap(OPERATOR_DOT);
    /**
     * Inside array of operators which will be used inside a prolog list.
     */
    private static final SingleCharOperatorContainerMap OPERATORS_INSIDE_LIST = new SingleCharOperatorContainerMap(
            OPERATOR_COMMA, OPERATOR_RIGHTSQUAREBRACKET, OPERATOR_VERTICALBAR);
    /**
     * Inside array of operators which contains the end list operator.
     */
    private static final SingleCharOperatorContainerMap OPERATORS_END_LIST = new SingleCharOperatorContainerMap(OPERATOR_RIGHTSQUAREBRACKET);
    /**
     * Inside array of operators which will be used to read a structure.
     */
    private static final SingleCharOperatorContainerMap OPERATORS_INSIDE_STRUCT = new SingleCharOperatorContainerMap(
            OPERATOR_COMMA, OPERATOR_RIGHTBRACKET);
    /**
     * Inside array of operators which will be used to read a sub-block inside of
     * a block.
     */
    private static final SingleCharOperatorContainerMap OPERATORS_SUBBLOCK = new SingleCharOperatorContainerMap(OPERATOR_RIGHTBRACKET);

    /**
     * Inside cache for term wrappers.
     */
    private final SoftCache<PrologTermWrapper> cacheForTermWrappers = new TermWrapperCache(256);
    /**
     * Inside cache of array lists to accumulate AbstractPrologTerms
     */
    private final ThreadNonSafeArrayListCache<AbstractPrologTerm> abstractPrologTermListCache = new ThreadNonSafeArrayListCache<AbstractPrologTerm>();
    private final SoftCache<ParserTreeItem> itemCache = new SoftCache<ParserTreeItem>(this, 128);
    /**
     * The tokenizer which is being used to tokenize the data stream.
     */
    private final PrologTokenizer tokenizer = new PrologTokenizer();
    /**
     * The engine context which owns the tree builder.
     */
    private final ParserContext context;
    /**
     * The last reader which was used to read a prolog sentence.
     */
    private PrologCharDataSource prologReader;
    /**
     * The constructor
     *
     * @param context the context for the parser, it can be null.
     */
    public AbstractPrologParser(final ParserContext context) {
        this.context = context;
    }

    protected static void readSystemOperators(final Class<?> src) {

        final PrologOperators operators = src.getAnnotation(PrologOperators.class);
        if (operators != null) {
            final FastStringBuilder accum = new FastStringBuilder(10);
            for (final PrologOperator operator : operators.Operators()) {
                if (SYSTEM_OPERATORS.containsKey(operator.Name())) {
                    final OperatorContainer container = SYSTEM_OPERATORS.get(operator.Name());
                    container.addOperator(Operator.makeOperator(operator.Priority(),
                            operator.Type(), operator.Name()));
                } else {
                    final OperatorContainer container = new OperatorContainer(
                            Operator.makeOperator(operator.Priority(), operator.Type(),
                                    operator.Name()));
                    SYSTEM_OPERATORS.put(operator.Name(), container);
                }

                accum.clear();
                for (char chr : operator.Name().toCharArray()) {
                    accum.append(chr);
                    SYSTEM_OPERATORS_PREFIXES.add(accum.toString());
                }
            }
        }
    }

    /**
     * It allows to get the system operator map (it includes both system operators
     * and meta-operators)
     *
     * @return the operator map contains system operators
     * @see OperatorContainer
     */
    public static Map<String, OperatorContainer> getSystemOperators() {
        final Map<String, OperatorContainer> result = new HashMap<String, OperatorContainer>(SYSTEM_OPERATORS);
        result.putAll(META_SYSTEM_OPERATORS.getMap());
        return result;
    }

    /**
     * Get a system operator for its name and type. Mainly the method is used for
     * deserialization.
     *
     * @param text the operator name, it must not be null.
     * @param type the type of the operator, it must not be null.
     * @return the found operator if it is found, null otherwise
     */
    public static Operator findSystemOperatorForNameAndType(final String text, final OperatorType type) {
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

    /**
     * Get a char code of a sngle char text string.
     *
     * @param text a single char text string.
     * @return if the text contains the single char then its code point will be
     * returned, 0x1FFFF otherwise
     */
    private static int getFirstCharIfItIsSingle(final String text) {
        if (text == null || text.length() != 1) {
            return 0x1FFFF;
        } else {
            return text.charAt(0);
        }
    }

    /**
     * Check that an operator is presented within an operator map
     *
     * @param operator     the operator to be examined, it can be null so the result
     *                     will be true
     * @param endOperators the map contains OperatorContainer objects to be
     *                     checked that it contains the operator, it can be null then the function
     *                     will return false
     * @return true if the array contains the operator else false
     */
    private boolean isEndOperator(final AbstractPrologTerm operator,
                                  final SingleCharOperatorContainerMap endOperators) {
        if (operator == null) {
            return true;
        }

        if (endOperators == null) {
            return false;
        }

        return operator.getType() == PrologTermType.OPERATORS && endOperators.containsKey(operator.getText());
    }

    /**
     * Get the current parser context
     *
     * @return the current context, it can be null.
     */
    public ParserContext getContext() {
        return context;
    }

    /**
     * Get last used reader
     *
     * @return the last used reader, it can be null
     */
    public PrologCharDataSource getReader() {
        return prologReader;
    }

    /**
     * Make a string based reader and read the first sentence from it
     *
     * @param str the string to be a stream for a reader, must not be null
     * @return the first prolog sentence from the string
     * @throws IOException           it will be thrown for any transport level problem
     * @throws PrologParserException it will be thrown for wrong prolog syntax
     */
    public AbstractPrologTerm nextSentence(final String str)
            throws IOException, PrologParserException {
        return this.nextSentence(new PrologCharDataSource(str));
    }

    /**
     * Read next sentence from a reader, the reader will become the current reader
     * for the parser
     *
     * @param reader the reader to be used as the data source for the parser, it
     *               must not be null and it will become as the current reader for the parser
     * @return the first prolog sentence met in the data stream from the reader,
     * if there is not any more sentences then it will return null
     * @throws PrologParserException it will be thrown if there is any prolog
     *                               syntax error
     * @throws IOException           it will be thrown if there is any transport error
     */
    public AbstractPrologTerm nextSentence(
            final PrologCharDataSource reader) throws PrologParserException,
            IOException {
        Assert.assertNotNull("The reader is null", reader);
        prologReader = reader;
        return this.nextSentence();
    }

    /**
     * Read the next sentence from the current reader
     *
     * @return the first sentence met in the data stream from the reader, if there
     * is not any data, then it will return null
     * @throws PrologParserException it will be thrown if there is any prolog
     *                               syntax error
     * @throws IOException           it will be thrown if there is any transport error
     */
    public AbstractPrologTerm nextSentence()
            throws PrologParserException, IOException {
        Assert.assertNotNull("The Current prolog reader is null", prologReader);

        final AbstractPrologTerm result = readBlock(OPERATORS_PHRASE);
        if (result == null) {
            return null; // end_of_file
        }
        final TokenizerResult endAtom = tokenizer.nextToken(this.prologReader, this);
        if (endAtom == null || !endAtom.getResult().getText().equals(OPERATOR_DOT.getText())) {
            throw new PrologParserException("End operator is not found", this.prologReader.getLineNumber(),
                    this.prologReader.getNextCharStringPosition());
        }
        endAtom.dispose();
        return result;
    }

    /**
     * Inside method to read a whole prolog structure from the input stream
     *
     * @param functor the functor for the read structure, must not be null
     * @return the read structure or null if the stream end was reached
     * @throws IOException           it will be thrown if there will be any transport error
     * @throws PrologParserException it will be thrown if there is an prolog
     *                               syntax error
     */
    private PrologStructure readStruct(final AbstractPrologTerm functor)
            throws PrologParserException, IOException {
        final List<AbstractPrologTerm> listOfAtoms = abstractPrologTermListCache.getListFromCache();
        PrologStructure result;
        try {
            while (true) {
                final AbstractPrologTerm block = readBlock(OPERATORS_INSIDE_STRUCT);

                if (block == null) {
                    return null;
                }

                final TokenizerResult nextAtom = tokenizer.nextToken(prologReader, this);
                final String nextText = nextAtom.getResult().getText();
                nextAtom.dispose();

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

    /**
     * Inside function to read a prolog list from the reader
     *
     * @param openingBracket the tokenizer result for the list opening bracket,
     *                       must not be null
     * @return the read list or null if the stream end was reached
     * @throws IOException           it will be thrown if there is any transport error
     * @throws PrologParserException it will be thrown if there is a prolog syntax
     *                               error
     */
    private AbstractPrologTerm readList(final TokenizerResult openingBracket) throws PrologParserException,
            IOException {
        Assert.assertNotNull("The Opening bracket info is null", openingBracket);

        PrologList leftPart = new PrologList();
        PrologList leftPartFirst = leftPart;
        AbstractPrologTerm rightPart = null;

        boolean hasSeparator = false;

        boolean doRead = true;

        TokenizerResult nextAtom = null;
        while (doRead) {
            final AbstractPrologTerm block = readBlock(OPERATORS_INSIDE_LIST);

            if (nextAtom != null) {
                nextAtom.dispose();
            }
            nextAtom = tokenizer.nextToken(prologReader, this);
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

                nextAtom.dispose();
                nextAtom = tokenizer.nextToken(prologReader, this);
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
                throw new CriticalSoftwareDefectError();
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

    /**
     * Inside function to throw PrologParserException if an object is null
     *
     * @param obj       the object to be asserted
     * @param message   the message to be wrapped by an exception if the object is
     *                  null
     * @param startTerm if the parameter is not null then its first char string
     *                  parameters will be used to make exception
     * @throws PrologParserException it will be thrown if the object is null
     */
    private void checkForNull(final Object obj, final String message, final TokenizerResult startTerm)
            throws PrologParserException {
        if (obj == null) {
            throw new PrologParserException(message,
                    startTerm.getLineNumber(),
                    startTerm.getStringPosition());
        }
    }

    /**
     * Inside function to read a block from the reader
     *
     * @param endOperators the map contains end operators which will bound the
     *                     block
     * @return a read block as Term or null if the end of the stream has been
     * reached
     * @throws IOException it will be thrown if there is any transport error
     */
    private AbstractPrologTerm readBlock(
            final SingleCharOperatorContainerMap endOperators)
            throws PrologParserException, IOException {
        // the variable will contain last processed tree item contains either
        // atom or operator
        ParserTreeItem currentTreeItem = null;

        TokenizerResult readAtomContainer = null;
        while (true) {
            // read next atom from tokenizer
            if (readAtomContainer != null) {
                readAtomContainer.dispose();
            }
            readAtomContainer = tokenizer.nextToken(
                    prologReader, this);
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
                tokenizer.pushTermBack(readAtomContainer);
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

                        final boolean rightPresented = !isEndOperator(tokenizer.peekToken(prologReader, this).getResult(), endOperators);

                        readAtom = readOperators.findCompatibleOperator(leftPresented, rightPresented);

                        if (readAtom == null) {
                            // we didn't get any operator for our criteria, so throw
                            // an exception

                            throw new PrologParserException("Incompatible operator type",
                                    readAtomContainer.getLineNumber(), readAtomContainer.getStringPosition());
                        }
                        // we have found needed operator so get its priority
                        readAtomPriority = readAtom.getPriority();
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

                                final TokenizerResult token = tokenizer.nextToken(prologReader, this);
                                final AbstractPrologTerm closingAtom = token.getResult();
                                token.dispose();

                                if (closingAtom == null || !closingAtom.getText().equals(OPERATOR_RIGHTBRACKET.getText())) {
                                    throw new PrologParserException("Non-closed brakes", prologReader.getLineNumber(),
                                            prologReader.getNextCharStringPosition());
                                }
                            } else {
                                readAtomPriority = readOperator.getPriority();
                            }
                        } else {
                            readAtomPriority = readOperator.getPriority();
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
                    final TokenizerResult nextToken = tokenizer.nextToken(prologReader, this);
                    if (nextToken != null && nextToken.getResult().getText().equals(OPERATOR_LEFTBRACKET.getText())) {
                        final int nextTokenLineNumber = nextToken.getLineNumber();
                        final int nextTokenStrPosition = nextToken.getStringPosition();

                        // it is a structure
                        if (readAtom.getType() == PrologTermType.ATOM) {
                            nextToken.dispose();
                            readAtom = readStruct(readAtom);
                            if (readAtom == null) {
                                // we have met the empty brackets, it disallowed by Prolog
                                throw new PrologParserException("Illegal start of term",
                                        nextTokenLineNumber, nextTokenStrPosition);
                            }
                        } else {
                            tokenizer.pushTermBack(nextToken);
                            throw new PrologParserException("You must have an atom as the structure functor",
                                    nextTokenLineNumber, nextTokenStrPosition);
                        }
                    } else {
                        // push back the next atom
                        tokenizer.pushTermBack(nextToken);

                        // check read atom to be zero-struct
                        if (readAtomContainer.getResult().getType() == PrologTermType.ATOM) {
                            if (readAtomContainer.getTokenizerState() == TokenizerState.ATOM
                                    && readAtom.getText().equals("!")) {
                                readAtom = new PrologStructure("!", readAtomContainer.getStringPosition(),
                                        readAtomContainer.getLineNumber());
                            } else if (context != null && context.hasZeroArityPredicate(this, readAtom.getText())) {
                                readAtom = new PrologStructure(readAtom, readAtomContainer.getStringPosition(),
                                        readAtomContainer.getLineNumber());
                            }
                        }
                    }

                }
                break;
            }

            final ParserTreeItem readAtomTreeItem = itemCache.get();
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
                        final ParserTreeItem foundItem = currentTreeItem.findFirstNodeWithSuchOrLowerPriority(readAtomPriority);
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
                                    throw new CriticalSoftwareDefectError();
                            }
                        }

                    } else if (currentTreeItem.getPriority() > readAtomPriority) {
                        // new has great priority
                        if (readAtomTreeItem.getType() != PrologTermType.OPERATOR && currentTreeItem.getRightBranch() != null) {
                            // it's a ground atom and its right branch is not empty
                            throw new PrologParserException(
                                    "There is not any operator before the atom",
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

    /**
     * The method allows to close the read char stream being used by the parser
     * and clear inside variables of the parser. I don't recommend to use the
     * parser after the method call.
     *
     * @throws IOException thrown if any error
     */
    public void close() throws IOException {
        if (prologReader != null) {
            prologReader.close();
            prologReader = null;
        }
    }

    @Override
    public ParserTreeItem makeNew() {
        return new ParserTreeItem(this);
    }

    private static final class TermWrapperCacheFactory implements SoftCacheItemFactory<PrologTermWrapper> {

        @Override
        public PrologTermWrapper makeNew() {
            return new PrologTermWrapper();
        }
    }

    private static final class TermWrapperCache extends SoftCache<PrologTermWrapper> {
        private static final TermWrapperCacheFactory factory = new TermWrapperCacheFactory();

        public TermWrapperCache(int size) {
            super(factory, size);
        }
    }
}
