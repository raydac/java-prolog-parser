/*
 * Copyright 2011 Igor Maznitsa (http://www.igormaznitsa.com)
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of version 3 of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307  USA
 */
package com.igormaznitsa.prologparser;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;

import com.igormaznitsa.prologparser.annotations.PrologOperators;
import com.igormaznitsa.prologparser.annotations.PrologOperator;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.operators.OperatorContainer;
import com.igormaznitsa.prologparser.operators.OperatorType;
import com.igormaznitsa.prologparser.terms.AbstractPrologNumericTerm;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologList;
import com.igormaznitsa.prologparser.terms.PrologStructure;
import com.igormaznitsa.prologparser.terms.PrologTermType;
import com.igormaznitsa.prologparser.terms.PrologVariable;
import com.igormaznitsa.prologparser.utils.AssertionUtils;

/**
 * The class is a hand-written prolog parser allows to parse incoming char
 * stream and make prolog structures.
 * The class is thread safe.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.03
 */
@PrologOperators(Operators = {
    @PrologOperator(Priority = 1200, Type = OperatorType.XFX, Name = ":-"),
    @PrologOperator(Priority = 1200, Type = OperatorType.XFX, Name = "-->"),

    @PrologOperator(Priority = 1200, Type = OperatorType.FX, Name = "?-"),
    @PrologOperator(Priority = 1200, Type = OperatorType.FX, Name = ":-"),

    @PrologOperator(Priority = 1100, Type = OperatorType.XFY, Name = ";"),

    @PrologOperator(Priority = 1050, Type = OperatorType.XFY, Name = "->"),
    
    @PrologOperator(Priority = 1000, Type = OperatorType.XFY, Name = ","),

    @PrologOperator(Priority = 900, Type = OperatorType.FY, Name = "\\+"),

    @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "="),
    @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "\\="),
    @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "=="),
    @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "\\=="),
    @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "@<"),
    @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "@=<"),
    @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "@>"),
    @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "@>="),
    @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "=.."),
    @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "is"),
    @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "=:="),
    @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "=\\="),
    @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "<"),
    @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "=<"),
    @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = ">"),
    @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = ">="),
    
    @PrologOperator(Priority = 500, Type = OperatorType.YFX, Name = "+"),
    @PrologOperator(Priority = 500, Type = OperatorType.YFX, Name = "-"),
    @PrologOperator(Priority = 500, Type = OperatorType.YFX, Name = "/\\"),
    @PrologOperator(Priority = 500, Type = OperatorType.YFX, Name = "\\/"),
    
    @PrologOperator(Priority = 400, Type = OperatorType.YFX, Name = "*"),
    @PrologOperator(Priority = 400, Type = OperatorType.YFX, Name = "/"),
    @PrologOperator(Priority = 400, Type = OperatorType.YFX, Name = "//"),
    @PrologOperator(Priority = 400, Type = OperatorType.YFX, Name = "<<"),
    @PrologOperator(Priority = 400, Type = OperatorType.YFX, Name = ">>"),
    @PrologOperator(Priority = 400, Type = OperatorType.YFX, Name = "rem"),
    @PrologOperator(Priority = 400, Type = OperatorType.YFX, Name = "mod"),
    
    @PrologOperator(Priority = 200, Type = OperatorType.XFX, Name = "**"),
    @PrologOperator(Priority = 200, Type = OperatorType.XFY, Name = "^"),
    @PrologOperator(Priority = 200, Type = OperatorType.FY, Name = "-"),
    @PrologOperator(Priority = 200, Type = OperatorType.FY, Name = "\\")})
public class PrologParser {

    /**
     * The Static map contains all system operators are being used by the parser
     * 
     * @since 1.00
     */
    static final Map<String, OperatorContainer> SYSTEM_OPERATORS = new HashMap<String, OperatorContainer>();
    
    /**
     * The Static map contains all system meta-operators for the parser (brackets, dot, vertical bar)
     * 
     * @since 1.03
     */
    static final Map<String, OperatorContainer> META_SYSTEM_OPERATORS = new HashMap<String, OperatorContainer>();
    
    /**
     * The set contains all possible prefixes of system operators to expedite
     * parsing
     * 
     * @since 1.00
     */
    static final Set<String> SYSTEM_OPERATORS_PREFIXES = new HashSet<String>();
    /**
     * Inside link to the system ',' operator
     * 
     * @since 1.00
     */
    static OperatorContainer OPERATOR_COMMA;
    /**
     * Inside link to the system '(' operator
     * 
     * @since 1.00
     */
    static OperatorContainer OPERATOR_LEFTBRACKET;
    /**
     * Inside link to the system ')' operator
     * 
     * @since 1.00
     */
    static OperatorContainer OPERATOR_RIGHTBRACKET;
    /**
     * Inside link to the system '[' operator
     * 
     * @since 1.00
     */
    static OperatorContainer OPERATOR_LEFTSQUAREBRACKET;
    /**
     * Inside link to the system ']' operator
     * 
     * @since 1.00
     */
    static OperatorContainer OPERATOR_RIGHTSQUAREBRACKET;
    /**
     * Inside link to the system '.' operator
     * 
     * @since 1.00
     */
    static OperatorContainer OPERATOR_DOT;
    /**
     * Inside link to the '|' operator
     * 
     * @since 1.00
     */
    static OperatorContainer OPERATOR_VERTICALBAR;

    static {
        initSystemOperatorsFromClassAnnotations();
        OPERATOR_COMMA = SYSTEM_OPERATORS.get(",");
        OPERATOR_LEFTBRACKET = META_SYSTEM_OPERATORS.get("(");
        OPERATOR_RIGHTBRACKET = META_SYSTEM_OPERATORS.get(")");
        OPERATOR_LEFTSQUAREBRACKET = META_SYSTEM_OPERATORS.get("[");
        OPERATOR_RIGHTSQUAREBRACKET = META_SYSTEM_OPERATORS.get("]");
        OPERATOR_DOT = META_SYSTEM_OPERATORS.get(".");
        OPERATOR_VERTICALBAR = META_SYSTEM_OPERATORS.get("|");
    }

    /**
     * It allows to get the system operator map (it includes both system operators and meta-operators)
     * 
     * @return the operator map contains system operators
     * @since 1.00
     * @see OperatorContainer
     */
    public static Map<String, OperatorContainer> getSystemOperators() {
        final Map<String,OperatorContainer> result = new HashMap<String,OperatorContainer>(SYSTEM_OPERATORS);
        result.putAll(META_SYSTEM_OPERATORS);
        return result;
    }
    /**
     * Inside array of operators which will be used to read a prolog phrase
     * @since 1.00
     */
    private static final Map<String, OperatorContainer> OPERATORS_PHRASE = makeMapFromOperatorContainers(OPERATOR_DOT);
    /**
     * Inside array of operators which will be used inside a prolog list
     * @since 1.00
     */
    private static final Map<String, OperatorContainer> OPERATORS_INSIDE_LIST = makeMapFromOperatorContainers(
            OPERATOR_COMMA, OPERATOR_RIGHTSQUAREBRACKET, OPERATOR_VERTICALBAR);
    /**
     * Inside array of operators which contains the end list operator
     * @since 1.00
     */
    private static final Map<String, OperatorContainer> OPERATORS_END_LIST = makeMapFromOperatorContainers(OPERATOR_RIGHTSQUAREBRACKET);
    /**
     * Inside array of operators which will be used to read a structure
     * @since 1.00
     */
    private static final Map<String, OperatorContainer> OPERATORS_INSIDE_STRUCT = makeMapFromOperatorContainers(
            OPERATOR_COMMA, OPERATOR_RIGHTBRACKET);
    /**
     * Inside array of operators which will be used to read a sub-block inside
     * of a block
     * @since 1.00
     */
    private static final Map<String, OperatorContainer> OPERATORS_SUBBLOCK = makeMapFromOperatorContainers(OPERATOR_RIGHTBRACKET);
    /**
     * The locker allows to avoid problems when the object is called simultaneously from different threads 
     * @since 1.01
     */
    private final ReentrantLock multithreadOperationsLocker = new ReentrantLock();

    /**
     * Inside auxiliary class represents a tree item
     * 
     * @author Igor Maznitsa (http://www.igormaznitsa.com)
     * @version 1.00
     */
    private static final class TreeItem {

        /**
         * The term saved by the item
         * @since 1.00
         */
        private final AbstractPrologTerm savedTerm;
        /**
         * The left branch of the tree item
         * @since 1.00
         */
        private TreeItem leftBranch;
        /**
         * The right branch of the item
         * @since 1.00
         */
        private TreeItem rightBranch;
        /**
         * The link to the owner of the item
         * @since 1.00
         */
        private TreeItem parentItem;
        /**
         * The term has been placed into brakes
         * @since 1.00
         */
        private final boolean insideBrakes;
        /**
         * The link to the owner parser for the item
         * @since 1.00
         */
        private final PrologParser parser;

        /**
         * The constructor
         * 
         * @param builder
         *            the builder which has the item, must not be null
         * @param term
         *            the term has been read from the input stream, must not be
         *            null
         * @param insideBrakes
         *            the flag shows that the term was in the brakes so it has
         *            the max priority
         * @param lineNum
         *            the line number of the line where the term has been found
         * @param strPos
         *            the string position of the read stream
         * @since 1.00
         */
        private TreeItem(final PrologParser builder,
                final AbstractPrologTerm term, final boolean insideBrakes,
                final int lineNum, final int strPos) {
            this.parser = builder;

            if (term.getType() == PrologTermType.OPERATORS || term.getType() == PrologTermType.OPERATOR) {
                savedTerm = new PrologTermWrapper(term);
            } else {
                savedTerm = term;
            }

            savedTerm.setStrPosition(strPos);
            savedTerm.setLineNumber(lineNum);
            this.insideBrakes = insideBrakes;
        }

        /**
         * Get the priority of the term.
         * 
         * @return the priority value of the term
         * @since 1.00
         */
        private int getPriority() {
            if (insideBrakes) {
                return 0;
            }
            return savedTerm.getPriority();
        }

        /**
         * Set the right branch.
         * 
         * @param item
         *            the right branch for the term
         * @since 1.00
         */
        private void setRightBranch(final TreeItem item) {
            rightBranch = item;
            if (item != null) {
                item.parentItem = this;
            }
        }

        /**
         * Make another item as the right branch.
         * 
         * @param item
         *            the item which will be used as the right branch
         * @return the item which will be used as the root, it can be this item
         *         or the set item (it depends on priorities)
         * @since 1.00
         */
        private TreeItem makeAsRightBranch(final TreeItem item) {
            final TreeItem currentSubbranch = rightBranch;
            setRightBranch(item);
            item.setLeftBranch(currentSubbranch);
            
            TreeItem result = this;
            
            if (item.getType() == PrologTermType.OPERATOR && item.getPriority() != 0) {
                result = item;
            }
            return result;
        }

        /**
         * Make an other item as the left branch.
         * 
         * @param item
         *            the item which will be used as the left branch
         * @return the item which will be used as the root, it can be this item
         *         or the set item (it depends on priorities)
         * @since 1.00
         */
        private TreeItem makeAsOwnerWithLeftBranch(final TreeItem item) {
            this.replaceForOwner(item);
            item.setLeftBranch(this);
            return item;
        }

        /**
         * Get the right branch of the item.
         * 
         * @return the right branch or null if there is not the right one
         * @since 1.00
         */
        private TreeItem getRightBranch() {
            return rightBranch;
        }

        /**
         * Set the left branch of the item.
         * 
         * @param item
         *            the left branch for the item
         * @since 1.00
         */
        private void setLeftBranch(final TreeItem item) {
            leftBranch = item;
            if (item != null) {
                item.parentItem = this;
            }
        }

        /**
         * Get the left branch of the item.
         * 
         * @return the left branch of the item or null if there is not left one
         * @since 1.00
         */
        private TreeItem getLeftBranch() {
            return leftBranch;
        }

        /**
         * Get the type of the saved term in the item.
         * 
         * @return the prolog term type of the term which is being saved by the
         *         item
         * @since 1.00
         */
        private PrologTermType getType() {
            return savedTerm.getType();
        }

        /**
         * Find the tree root of the tree where the item has been presented.
         * 
         * @return the root item for the tree where the item is being, if the
         *         item is the root then it will be returned
         * @since 1.00
         */
        private TreeItem findRoot() {
            if (parentItem == null) {
                return this;
            }
            return parentItem.findRoot();
        }

        /**
         * Find the first node among the item's owners which has the same or
         * lower priority.
         * 
         * @param priority
         *            the priority for the desired item
         * @return found root item which has the equal or less priority than the
         *         value
         * @since 1.00
         */
        private TreeItem findFirstNodeWithSuchOrLowerPriority(final int priority) {
            TreeItem result = null;
            if (getPriority() >= priority || parentItem == null) {
                result = this;
            } else {
                result = parentItem.findFirstNodeWithSuchOrLowerPriority(priority);
            }
            return result;
        }

        /**
         * Replace the owner for the item by another item
         * 
         * @param newItem
         *            the item to replace the current owner, it can be null
         * @since 1.00
         */
        private void replaceForOwner(final TreeItem newItem) {
            if (parentItem == null) {
                newItem.parentItem = null;
                return;
            }

            if (this.equals(parentItem.getLeftBranch())) {
                parentItem.setLeftBranch(newItem);
            } else {
                parentItem.setRightBranch(newItem);
            }
        }

        /**
         * Get an operator type for the saved term which must be an operator!
         * 
         * @return the operator type as a value frome the OperatorType
         *         enumeration
         * @since 1.00
         */
        private OperatorType getOperatorType() {
            return ((Operator) ((PrologTermWrapper) savedTerm).getWrappedTerm()).getOperatorType();
        }

        /**
         * Check the validity of the tree item for its saved term
         * 
         * @return true if the tree item is valid and false if it's not
         * @since 1.00
         */
        private boolean validate() {
            if (savedTerm.getType() == PrologTermType.OPERATOR) {
                final int priority = getPriority();

                final Operator wrappedOperator = (Operator) ((PrologTermWrapper) savedTerm).getWrappedTerm();

                switch (wrappedOperator.getOperatorType()) {
                    case FX:
                        return leftBranch == null
                                && (rightBranch != null && rightBranch.getPriority() < priority);
                    case FY:
                        return leftBranch == null
                                && (rightBranch != null && rightBranch.getPriority() <= priority);
                    case YF:
                        return (leftBranch != null && leftBranch.getPriority() <= priority)
                                && rightBranch == null;
                    case XF:
                        return (leftBranch != null && leftBranch.getPriority() < priority)
                                && rightBranch == null;
                    case XFX:
                        return (leftBranch != null && leftBranch.getPriority() < priority)
                                && (rightBranch != null && rightBranch.getPriority() < priority);
                    case XFY:
                        return (leftBranch != null && leftBranch.getPriority() < priority)
                                && (rightBranch != null && rightBranch.getPriority() <= priority);
                    case YFX:
                        return (leftBranch != null && leftBranch.getPriority() <= priority)
                                && (rightBranch != null && rightBranch.getPriority() < priority);
                    default:
                        throw new Error(
                                "Unknown operator type, it's a conceptual programming error");
                }
            } else {
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
         * @since 1.00
         */
        private AbstractPrologTerm convertTreeItemIntoTerm()
                throws PrologParserException {
            AbstractPrologTerm result = null;
            switch (savedTerm.getType()) {
                case OPERATOR:
                    final PrologTermWrapper wrapper = (PrologTermWrapper) savedTerm;

                    PrologStructure operatorStruct = null;
                    if (!validate()) {
                        throw new PrologParserException("Wrong operator ["+wrapper.getText()+']', wrapper.getLineNumber(),
                                wrapper.getStrPosition());
                    }

                    final AbstractPrologTerm left = leftBranch != null ? leftBranch.convertTreeItemIntoTerm() : null;
                    final AbstractPrologTerm right = rightBranch != null ? rightBranch.convertTreeItemIntoTerm() : null;
                    if (left == null && right == null) {
                        throw new PrologParserException(
                                "Operator without operands", wrapper.getLineNumber(), wrapper.getStrPosition());
                    }

                    // this code replaces '-'(number) to '-number'
                    if ("-".equals(wrapper.getText()) && left == null) {
                        if (right.getType() == PrologTermType.ATOM
                                && right instanceof AbstractPrologNumericTerm) {
                            result = ((AbstractPrologNumericTerm) right).neg();
                            break;
                        }
                    }

                    if (left != null) {
                        if (right == null) {
                            operatorStruct = new PrologStructure(
                                    (Operator) wrapper.getWrappedTerm(),
                                    new AbstractPrologTerm[]{left});
                        } else {
                            operatorStruct = new PrologStructure(
                                    (Operator) wrapper.getWrappedTerm(), new AbstractPrologTerm[]{
                                        left, right});
                        }
                    } else {
                        operatorStruct = new PrologStructure((Operator) wrapper.getWrappedTerm(),
                                new AbstractPrologTerm[]{right});
                    }

                    operatorStruct.setStrPosition(wrapper.getStrPosition());
                    operatorStruct.setLineNumber(wrapper.getLineNumber());

                    if (parser.context != null) {
                        parser.context.processNewStructure(parser, operatorStruct);
                    }

                    result = operatorStruct;
                    break;
                case STRUCT:
                    if (parser.context != null) {
                        parser.context.processNewStructure(parser, (PrologStructure) savedTerm);
                    }
                    result = savedTerm;
                    break;
                default:
                    result = savedTerm;
                    break;
            }
            return result;
        }
    }

    /**
     * Check that an operator is presented within an operator map
     * 
     * @param operator
     *            the operator to be examined, it can be null so the result will
     *            be true
     * @param endOperators
     *            the map contains OperatorContainer objects to be checked that
     *            it contains the operator, it can be null then the function
     *            will return false
     * @return true if the array contains the operator else false
     * @since 1.00
     */
    private boolean isEndOperator(final AbstractPrologTerm operator,
            final Map<String, OperatorContainer> endOperators) {
        if (operator == null) {
            return true;
        }

        if (endOperators == null) {
            return false;
        }

        if (operator.getType() == PrologTermType.OPERATORS) {
            return endOperators.containsKey(operator.getText());
        }
        return false;
    }
    /**
     * The map contains variable map which is being used to map variables inside
     * a tree
     * @since 1.00
     */
    private final Map<String, PrologVariable> variableSet;
    /**
     * The tokenizer which is being used to tokenize the data stream
     * @since 1.00
     */
    private final PrologTokenizer tokenizer = new PrologTokenizer();
    /**
     * The last reader which was used to read a prolog sentence
     * @since 1.00
     */
    private PrologCharDataSource prologReader;
    /**
     * The engine context which owns the tree builder
     * @since 1.00
     */
    private final ParserContext context;

    /**
     * Get the current parser context
     * 
     * @return the current context, it can be null
     * @since 1.00
     */
    public ParserContext getContext() {
        return context;
    }

    /**
     * Get last used reader
     * 
     * @return the last used reader, it can be null
     * @since 1.00
     */
    public PrologCharDataSource getReader() {
        return prologReader;
    }

    /**
     * The constructor
     * 
     * @param context
     *            the context for the parser, it can be null
     * @since 1.00
     */
    public PrologParser(final ParserContext context) {
        variableSet = new HashMap<String, PrologVariable>();
        this.context = context;
    }

    /**
     * Make a string based reader and read the first sentence from it
     * 
     * @param str
     *            the string to be a stream for a reader, must not be null
     * @return the first prolog sentence from the string
     * @throws IOException
     *             it will be thrown for any transport level problem
     * @throws PrologParserException
     *             it will be thrown for wrong prolog syntax
     * @since 1.00
     */
    public AbstractPrologTerm nextSentence(final String str)
            throws IOException, PrologParserException {
        return this.nextSentence(new PrologCharDataSource(str));
    }

    /**
     * Read next sentence from a reader, the reader will become the current
     * reader for the parser
     * 
     * @param reader
     *            the reader to be used as the data source for the parser, it
     *            must not be null and it will become as the current reader for
     *            the parser
     * @return the first prolog sentence met in the data stream from the reader,
     *         if there is not any more sentences then it will return null
     * @throws PrologParserException
     *             it will be thrown if there is any prolog syntax error
     * @throws IOException
     *             it will be thrown if there is any transport error
     * @since 1.00
     */
    public AbstractPrologTerm nextSentence(
            final PrologCharDataSource reader) throws PrologParserException,
            IOException {
        AssertionUtils.checkNotNull("The reader is null", reader);
        multithreadOperationsLocker.lock();
        try {
            prologReader = reader;
            return this.nextSentence();
        } finally {
            multithreadOperationsLocker.unlock();
        }
    }

    /**
     * Read the next sentence from the current reader
     * 
     * @return the first sentence met in the data stream from the reader, if
     *         there is not any data, then it will return null
     * @throws PrologParserException
     *             it will be thrown if there is any prolog syntax error
     * @throws IOException
     *             it will be thrown if there is any transport error
     * @since 1.00
     */
    public AbstractPrologTerm nextSentence()
            throws PrologParserException, IOException {
        multithreadOperationsLocker.lock();
        try {
            AssertionUtils.checkNotNull("The Current prolog reader is null", prologReader);

            variableSet.clear();
            final AbstractPrologTerm result = readBlock(OPERATORS_PHRASE);
            if (result == null) {
                return null; // end_of_file
            }
            final TokenizerResult endAtom = tokenizer.nextToken(this.prologReader, this);
            if (endAtom == null || !endAtom.getResult().getText().equals(OPERATOR_DOT.getText())) {
                throw new PrologParserException("End operator is not found", this.prologReader.getLineNumber(),
                        this.prologReader.getNextCharStringPosition());
            }
            return result;
        } finally {
            multithreadOperationsLocker.unlock();
            variableSet.clear();
        }
    }

    /**
     * Inside method to read a whole prolog structure from the input stream
     * 
     * @param functor
     *            the functor for the read structure, must not be null
     * @return the read structure or null if the stream end was reached
     * @throws IOException
     *             it will be thrown if there will be any transport error
     * @throws PrologParserException
     *             it will be thrown if there is an prolog syntax error
     * @since 1.00
     */
    private PrologStructure readStruct(final AbstractPrologTerm functor)
            throws PrologParserException, IOException {
        final ArrayList<AbstractPrologTerm> listOfAtoms = new ArrayList<AbstractPrologTerm>();

        while (true) {
            final AbstractPrologTerm block = readBlock(OPERATORS_INSIDE_STRUCT);

            final TokenizerResult nextAtom = tokenizer.nextToken(prologReader,
                    this);
            final String nextText = nextAtom.getResult().getText();

            if (OPERATOR_COMMA.getText().equals(nextText)) {
                // next item
                if (block == null) {
                    throw new PrologParserException("Empty structure element",
                            tokenizer.getLastTokenLineNum(),
                            tokenizer.getLastTokenStrPos());
                } else {
                    listOfAtoms.add(block);
                }
                continue;
            } else if (OPERATOR_RIGHTBRACKET.getText().equals(nextText)) {
                // end of the structure
                if (block != null) {
                    listOfAtoms.add(block);
                }
                break;
            }
        }

        final PrologStructure result = new PrologStructure(functor,
                listOfAtoms.toArray(new AbstractPrologTerm[listOfAtoms.size()]));

        return result;
    }

    /**
     * Inside function to read a prolog list from the reader
     * 
     * @param openingBracket
     *            the tokenizer result for the list opening bracket, must not be null
     * @return the read list or null if the stream end was reached
     * @throws IOException
     *             it will be thrown if there is any transport error
     * @throws PrologParserException
     *             it will be thrown if there is a prolog syntax error
     * @since 1.00
     */
    private AbstractPrologTerm readList(final TokenizerResult openingBracket) throws PrologParserException,
            IOException {
        AssertionUtils.checkNotNull("The Opening bracket info is null", openingBracket);

        PrologList leftPart = new PrologList();
        PrologList leftPartFirst = leftPart;
        AbstractPrologTerm rightPart = null;

        boolean hasSeparator = false;

        boolean doRead = true;

        while (doRead) {
            final AbstractPrologTerm block = readBlock(OPERATORS_INSIDE_LIST);

            TokenizerResult nextAtom = tokenizer.nextToken(prologReader,
                    this);

            final String text = nextAtom.getResult().getText();
            if (OPERATOR_RIGHTSQUAREBRACKET.getText().equals(text)) {
                // end
                doRead = false;
                if (block == null) {
                    continue;
                }
            } else if (OPERATOR_VERTICALBAR.getText().equals(text)) {
                // we have found the list tail, so we need read it as one block
                // until the ']' atom
                checkForNull(block, "There is not any list element", openingBracket);
                if (leftPartFirst.isNullList()) {
                    leftPartFirst = PrologList.setTermAsNewListTail(leftPart,
                            block);
                    leftPart = leftPartFirst;
                } else {
                    leftPart = PrologList.setTermAsNewListTail(leftPart, block);
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

                nextAtom = tokenizer.nextToken(prologReader, this);
                if (!nextAtom.getResult().getText().equals(OPERATOR_RIGHTSQUAREBRACKET.getText())) {
                    throw new PrologParserException(
                            "Wrong end of the list tail",
                            tokenizer.getLastTokenLineNum(),
                            tokenizer.getLastTokenStrPos());
                }

                break;
            } else if (OPERATOR_COMMA.getText().equals(text)) {
                // all good and we read next block
                checkForNull(block, "List element not found", nextAtom);
            } else {
                throw new Error("Nonprocessd state at list definition");
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
     * @param obj
     *            the object to be asserted
     * @param message
     *            the message to be wrapped by an exception if the object is
     *            null
     * @param startTerm
     * 			if the parameter is not null then its first char string parameters will be used to make exception
     * @throws PrologParserException
     *             it will be thrown if the object is null
     * @since 1.00
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
     * @param endOperators
     *            the map contains end operators which will bound the block
     * @return a read block as Term or null if the end of the stream has been
     *         reached
     * @throws IOException
     *             it will be thrown if there is any transport error
     * @since 1.00
     */
    private AbstractPrologTerm readBlock(
            final Map<String, OperatorContainer> endOperators)
            throws PrologParserException, IOException {
        // the variable will contain last processed tree item contains either
        // atom or operator
        TreeItem currentTreeItem = null;

        while (true) {
            // read next atom from tokenizer
            final TokenizerResult readAtomContainer = tokenizer.nextToken(
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
                            if (currentTreeItem.getType() != PrologTermType.OPERATOR) {
                                leftPresented = true;
                            } else {
                                if (currentTreeItem.getRightBranch() != null) {
                                    leftPresented = true;
                                }
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
                            if (OPERATOR_LEFTSQUAREBRACKET.getText().equals(operatorText)) {
                                // it's a list
                                readAtom = readList(readAtomContainer);
                                readAtom.setStrPosition(readAtomContainer.getStringPosition());
                                readAtom.setLineNumber(readAtomContainer.getLineNumber());
                                readAtomPriority = 0;
                            } else if (OPERATOR_LEFTBRACKET.getText().equals(operatorText)) {
                                // read subblock
                                atBrakes = true;
                                readAtom = readBlock(OPERATORS_SUBBLOCK);
                                readAtom.setStrPosition(readAtomContainer.getStringPosition());
                                readAtom.setLineNumber(readAtomContainer.getLineNumber());
                                readAtomPriority = 0;
                                final AbstractPrologTerm closingAtom = tokenizer.nextToken(prologReader, this).getResult();
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
                    final PrologVariable var = (PrologVariable) readAtom;
                    if (!var.isAnonymous()) {
                        // it's not an anonymous variable so we need to process it
                        // and cache if it is not at the var table yet
                        final PrologVariable cachedVar = variableSet.get(var.getText());
                        if (cachedVar == null) {
                            // first meet variable
                            // cache it
                            variableSet.put(var.getText(), var);
                        } else {
                            // set cached variable as linked variable
                            ((PrologVariable) readAtom).setLinkedVariable(cachedVar);
                        }
                    }
                }
                break;
                default: {
                    final TokenizerResult nextToken = tokenizer.nextToken(prologReader, this);
                    if (nextToken != null && nextToken.getResult().getText().equals(OPERATOR_LEFTBRACKET.getText())) {
                        // it is a structure
                        if (readAtom.getType() == PrologTermType.ATOM) {
                            readAtom = readStruct(readAtom);
                        } else {
                            tokenizer.pushTermBack(nextToken);
                            throw new PrologParserException("You must have an atom as the structure functor",
                                    nextToken.getLineNumber(), nextToken.getStringPosition());
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

            final TreeItem readAtomTreeItem = new TreeItem(this, readAtom,
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
                                    throw new Error("Unknown operator type");
                            }
                        }

                    } else if (currentTreeItem.getPriority() > readAtomPriority) {
                        // new has great priority
                        if (readAtomTreeItem.getType() != PrologTermType.OPERATOR) {
                            // it's a ground atom
                            // so check that the right branch is empty
                            if (currentTreeItem.getRightBranch() != null) {
                                throw new PrologParserException(
                                        "There is not any operator before the atom",
                                        readAtomContainer.getLineNumber(),
                                        readAtomContainer.getStringPosition());
                            }
                        }
                        // make it as right
                        currentTreeItem = currentTreeItem.makeAsRightBranch(readAtomTreeItem);
                    }
                } else {
                    // check that it is an operator
                    if (currentTreeItem != null
                            && currentTreeItem.getType() != PrologTermType.OPERATOR
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
     * The method allows to close the read char stream being used by the parser and clear inside variables of the parser.
     * I don't recommend to use the parser after the method call.
     * 
     * @throws IOException
     * @since 1.01
     */
    public void close() throws IOException {
        multithreadOperationsLocker.lock();
        try {
            variableSet.clear();
            if (prologReader != null) {
                prologReader.close();
                prologReader = null;
            }
        } finally {
            multithreadOperationsLocker.unlock();
        }
    }

    /**
     * Inside auxiliary function to generate a map contains OperatorContainer
     * objects
     * 
     * @param containers
     *            operator containers to be added into the result map
     * @return the map contains all operator containers
     * @since 1.00
     */
    private static Map<String, OperatorContainer> makeMapFromOperatorContainers(
            final OperatorContainer... containers) {
        final Map<String, OperatorContainer> result = new HashMap<String, OperatorContainer>();
        for (final OperatorContainer current : containers) {
            result.put(current.getText(), current);
        }
        return result;
    }

    /**
     * Inside auxiliary function to initialize inside system operators table
     * from annotations
     * @since 1.00
     */
    private static void initSystemOperatorsFromClassAnnotations() {
        META_SYSTEM_OPERATORS.clear();
        
        OPERATOR_DOT = new OperatorContainer(Operator.METAOPERATOR_DOT);
        META_SYSTEM_OPERATORS.put(Operator.METAOPERATOR_DOT.getText(),OPERATOR_DOT);
        
        OPERATOR_LEFTBRACKET = new OperatorContainer(Operator.METAOPERATOR_LEFT_BRACKET);
        META_SYSTEM_OPERATORS.put(Operator.METAOPERATOR_LEFT_BRACKET.getText(),OPERATOR_LEFTBRACKET);
        
        OPERATOR_RIGHTBRACKET = new OperatorContainer(Operator.METAOPERATOR_RIGHT_BRACKET);
        META_SYSTEM_OPERATORS.put(Operator.METAOPERATOR_RIGHT_BRACKET.getText(),OPERATOR_RIGHTBRACKET);
        
        OPERATOR_LEFTSQUAREBRACKET = new OperatorContainer(Operator.METAOPERATOR_LEFT_SQUARE_BRACKET);
        META_SYSTEM_OPERATORS.put(Operator.METAOPERATOR_LEFT_SQUARE_BRACKET.getText(),OPERATOR_LEFTSQUAREBRACKET);
        
        OPERATOR_RIGHTSQUAREBRACKET = new OperatorContainer(Operator.METAOPERATOR_RIGHT_SQUARE_BRACKET);
        META_SYSTEM_OPERATORS.put(Operator.METAOPERATOR_RIGHT_SQUARE_BRACKET.getText(),OPERATOR_RIGHTSQUAREBRACKET);
        
        OPERATOR_VERTICALBAR = new OperatorContainer(Operator.METAOPERATOR_VERTICAL_BAR);
        META_SYSTEM_OPERATORS.put(Operator.METAOPERATOR_VERTICAL_BAR.getText(),OPERATOR_VERTICALBAR);
        
        SYSTEM_OPERATORS.clear();
        SYSTEM_OPERATORS_PREFIXES.clear();

        SYSTEM_OPERATORS_PREFIXES.add(Operator.METAOPERATOR_DOT.getText());
        SYSTEM_OPERATORS_PREFIXES.add(Operator.METAOPERATOR_LEFT_BRACKET.getText());
        SYSTEM_OPERATORS_PREFIXES.add(Operator.METAOPERATOR_LEFT_SQUARE_BRACKET.getText());
        SYSTEM_OPERATORS_PREFIXES.add(Operator.METAOPERATOR_RIGHT_BRACKET.getText());
        SYSTEM_OPERATORS_PREFIXES.add(Operator.METAOPERATOR_RIGHT_SQUARE_BRACKET.getText());
        SYSTEM_OPERATORS_PREFIXES.add(Operator.METAOPERATOR_VERTICAL_BAR.getText());
        
        final PrologOperators operators = PrologParser.class.getAnnotation(PrologOperators.class);
        if (operators != null) {
            final StringBuilder accum = new StringBuilder(10);
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

                accum.setLength(0);
                for (char chr : operator.Name().toCharArray()) {
                    accum.append(chr);
                    SYSTEM_OPERATORS_PREFIXES.add(accum.toString());
                }
            }
        }
    }
}
