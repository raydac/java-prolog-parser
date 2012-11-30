/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.exceptions.CriticalSoftwareDefectError;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.operators.Operator;
import com.igormaznitsa.prologparser.operators.OperatorType;
import com.igormaznitsa.prologparser.terms.AbstractPrologNumericTerm;
import com.igormaznitsa.prologparser.terms.AbstractPrologTerm;
import com.igormaznitsa.prologparser.terms.PrologStructure;
import com.igormaznitsa.prologparser.terms.PrologTermType;

/**
 * Inside auxiliary class represents a tree item
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 */
final class ParserTreeItem {

    /**
     * The term saved by the item.
     */
    private final AbstractPrologTerm savedTerm;
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
    private final boolean insideBrakes;
    /**
     * The link to the owner parser for the item.
     */
    private final PrologParser parser;

    /**
     * The constructor
     *
     * @param builder the builder which has the item, must not be null
     * @param term the term has been read from the input stream, must not be
     * null
     * @param insideBrakes the flag shows that the term was in the brakes so it
     * has the max priority
     * @param lineNum the line number of the line where the term has been found
     * @param strPos the string position of the read stream
     */
    /**
     * The constructor
     *
     * @param parser the builder which has the item, must not be null
     * @param term the term has been read from the input stream, must not be
     * null
     * @param insideBrakes the flag shows that the term was in the brakes so it
     * has the max priority
     * @param lineNum the line number of the line where the term has been found
     * @param strPos the string position of the read stream
     */
    ParserTreeItem(final PrologParser parser, final AbstractPrologTerm term, final boolean insideBrakes, final int lineNum, final int strPos) {
        this.parser = parser;
        if (term.getType() == PrologTermType.OPERATOR || term.getType() == PrologTermType.OPERATORS) {
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
     */
    int getPriority() {
        if (insideBrakes) {
            return 0;
        }
        return savedTerm.getPriority();
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
     * @return the item which will be used as the root, it can be this item or
     * the set item (it depends on priorities)
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
     * @return the item which will be used as the root, it can be this item or
     * the set item (it depends on priorities)
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
     * @return the root item for the tree where the item is being, if the item
     * is the root then it will be returned
     */
    ParserTreeItem findRoot() {
        ParserTreeItem result = this;
        while (true) {
            final ParserTreeItem theParent = result.parentItem;
            if (theParent == null) {
                break;
            } else {
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
     * @return found root item which has the equal or less priority than the
     * value
     */
    ParserTreeItem findFirstNodeWithSuchOrLowerPriority(final int priority) {
        ParserTreeItem result = this;

        while (true) {
            final ParserTreeItem itsparent = result.parentItem;
            if (itsparent == null || result.getPriority() >= priority) {
                break;
            } else {
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
        if (this.equals(parentItem.getLeftBranch())) {
            parentItem.setLeftBranch(newItem);
        } else {
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
     */
    AbstractPrologTerm convertTreeItemIntoTerm() throws PrologParserException {
        AbstractPrologTerm result;
        final ParserContext ctx = parser.getContext();
        final boolean ctxNotNull = ctx != null;
        switch (savedTerm.getType()) {
            case OPERATOR:
                final PrologTermWrapper wrapper = (PrologTermWrapper) savedTerm;
                PrologStructure operatorStruct;
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
                    operatorStruct = new PrologStructure((Operator) wrapper.getWrappedTerm(), new AbstractPrologTerm[]{right});
                } else {
                    if (right == null) {
                        operatorStruct = new PrologStructure((Operator) wrapper.getWrappedTerm(), new AbstractPrologTerm[]{left});
                    } else {
                        operatorStruct = new PrologStructure((Operator) wrapper.getWrappedTerm(), new AbstractPrologTerm[]{left, right});
                    }
                }
                operatorStruct.setStrPosition(wrapper.getStrPosition());
                operatorStruct.setLineNumber(wrapper.getLineNumber());
                if (ctxNotNull) {
                    ctx.processNewStructure(parser, operatorStruct);
                }
                result = operatorStruct;
                break;
            case STRUCT:
                if (ctxNotNull) {
                    ctx.processNewStructure(parser, (PrologStructure) savedTerm);
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
