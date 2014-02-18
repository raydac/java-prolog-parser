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
package com.igormaznitsa.prologparser.terms;
import com.igormaznitsa.prologparser.utils.FastStringBuilder;
import static com.igormaznitsa.prologparser.utils.AssertionUtils.*;

/**
 * The class describes a prolog list being used by the prolog parser.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.01
 */
public final class PrologList extends PrologStructure {
    private static final long serialVersionUID = -3781638438477876869L;

    /**
     * The functor is used for a structure describing a prolog lisp
     */
    public static final AbstractPrologTerm LIST_FUNCTOR = new PrologAtom(".");

    /**
     * A Constructor. It allows to create an empty ([]) list.
     */
    public PrologList() {
        super(LIST_FUNCTOR, 2);
        elements[0] = null;
        elements[1] = null;
    }

    /**
     * A Constructor. It allows to create an empty list and set its first char position in the source stream.
     * @param strPosition the first term char string position
     * @param lineNumber the first term char line number
     */
    public PrologList(final int strPosition, final int lineNumber) {
        this();
        setStrPosition(strPosition);
        setLineNumber(lineNumber);
    }

    /**
     * A Constructor. It allows to create a prolog list chain from an array
     * containing prolog term objects.
     * 
     * @param array
     *            an array contains prolog term objects, the array must not be
     *            null and it must not contains null elements.
     */
    public PrologList(final AbstractPrologTerm[] array) {
        this();
        checkNotNull("Array is null", array);

        PrologList current = this;

        for (final AbstractPrologTerm term : array) {
            checkNotNull("Array contains a null element", term);
            current = current.addAsNewListToEndOfListChain(term);
        }
    }

    /**
     * A Constructor. It allows to create a prolog list chain from an array containing prolog term objects and set the first list char position in the source stream.
     * @param array an array contains prolog term objects, the array must not be null and it must not contains null elements.
     * @param strPos the first term char string position in the source stream
     * @param lineNumber the first term char line number in the source stream
     */
    public PrologList(final AbstractPrologTerm[] array, final int strPos, final int lineNumber) {
        this(array);
        setStrPosition(strPos);
        setLineNumber(lineNumber);
    }

    /**
     * A Constructor. It allows to create a list and set the argument as the
     * list head element.
     * 
     * @param head
     *            a term to be used as the list head. Must not be null;
     */
    public PrologList(final AbstractPrologTerm head) {
        this();
        setHead(head);
        setTail(new PrologList());
    }

    /**
     * A Constructor. It allows to create a list with the term as the list head and set the first term char position in the source stream
     * @param head a term to be used as the list head. Must not be null.
     * @param strPosition the first term char string position
     * @param lineNumber the first term char line number
     */
    public PrologList(final AbstractPrologTerm head, final int strPosition, final int lineNumber) {
        this(head);
        setStrPosition(strPosition);
        setLineNumber(lineNumber);
    }

    /**
     * A Constructor. It allows to create a list and set the head and the tail.
     * 
     * @param head
     *            a term to be used as the head for the list. Must not be null.
     * @param tail
     *            a term to be used as the tail for the list. Must not be null.
     */
    public PrologList(final AbstractPrologTerm head,
            final AbstractPrologTerm tail) {
        this();
        setHead(head);
        setTail(tail);
    }

    /**
     * A Constructor. It allows to create a list and set the head and the tail and set the first term char position in the source stream.
     * 
     * @param head
     *            a term to be used as the head for the list. Must not be null.
     * @param tail
     *            a term to be used as the tail for the list. Must not be null.
     * @param strPosition the first term char string position
     * @param lineNumber the first term char line number
     */
    public PrologList(final AbstractPrologTerm head, final AbstractPrologTerm tail, final int strPosition, final int lineNumber) {
        this(head, tail);
        setStrPosition(strPosition);
        setLineNumber(lineNumber);
    }

    /**
     * Check that the list instance is the null list.
     * 
     * @return true if the list is a null list ([]), else false
     */
    public boolean isNullList() {
        return getHead() == null && getTail() == null;
    }

    /**
     * Get the head of the list.
     * 
     * @return the head of the list, if it is a null list then it will return
     *         null.
     */
    public AbstractPrologTerm getHead() {
        return getElement(0);
    }

    /**
     * Get the tail of the list.
     * 
     * @return the tail of the list, if it is a null list then it will return
     *         null
     */
    public AbstractPrologTerm getTail() {
        return getElement(1);
    }

    /**
     * Set the head for the list.
     * 
     * @param term
     *            a term to be used as the head for the list, must not be null.
     */
    public void setHead(final AbstractPrologTerm term) {
        this.setElement(0, term);
        if (getTail() == null) {
            setTail(new PrologList());
        }
    }

    /**
     * Set the tail for the list.
     * 
     * @param term
     *            a term to be used as the tail for the list, must not be null.
     */
    public void setTail(final AbstractPrologTerm term) {
        this.setElement(1, term);
        if (getHead() == null) {
            setHead(EMPTY_ATOM);
        }
    }

    /**
     * Set new list tail to a list and return the created list (or the same list
     * if the list is an empty list).
     * 
     * @param list
     *            a list which tail should be changed, must not be null.
     * @param term
     *            a term to be added as the list tail, must not be null
     * @return if the list is an empty one then the list will be returned else
     *         new list will be created and returned
     */
    public static PrologList setTermAsNewListTail(final PrologList list,
            final AbstractPrologTerm term) {
        checkNotNull("The list is null", list);
        checkNotNull("The term is null", term);

        PrologList result = list;

        if (list.isNullList()) {
            list.setHead(term);
            list.setTail(new PrologList());
        } else {
            result = new PrologList(term, new PrologList());
            list.setTail(result);
        }

        return result;
    }

    /**
     * Add a term as new list into the end of the list chain where the current
     * list is being presented. If the last list in the chain contains the tail
     * which is not a list then its tail will be just replaced else new list
     * will be created and set as the tail for the last list.
     * 
     * @param term
     *            a term to be added, must not be null.
     * @return new list or the last list in the chain.
     */
    public PrologList addAsNewListToEndOfListChain(
            final AbstractPrologTerm term) {
        checkNotNull("The term is null", term);

        if (isNullList()) {
            setHead(term);
            setTail(new PrologList());
            return this;
        } else {
            PrologList current = this;
            while (true) {
                if (current.isNullList()) {
                    current.setHead(term);
                    current.setTail(new PrologList());
                    return current;
                } else {
                    final AbstractPrologTerm ltail = current.getTail();
                    if (ltail.getType() == PrologTermType.LIST) {
                        current = (PrologList) ltail;
                        continue;
                    } else {
                        final PrologList newOne = new PrologList(term,
                                new PrologList());
                        current.setTail(newOne);
                        return newOne;
                    }
                }
            }
        }
    }

    /**
     * Replace the tail of the last element of the list chain by a term.
     * 
     * @param elementToReplace
     *            a prolog term to replace the tail of the last list in the
     *            chain, must not be null
     */
    public void replaceLastElement(
            final AbstractPrologTerm elementToReplace) {
        checkNotNull("The element is null", elementToReplace);

        PrologList curList = this;
        while (true) {
            final AbstractPrologTerm tail = curList.getTail();
            if (tail.getType() == PrologTermType.LIST) {
                final PrologList ltail = (PrologList) tail;
                if (ltail.isNullList()) {
                    curList.setTail(elementToReplace);
                    break;
                }
            } else {
                curList.setTail(elementToReplace);
                break;
            }
            curList = (PrologList) tail;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PrologTermType getType() {
        return PrologTermType.LIST;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        String result = "[]";

        if (!isNullList()) {
            final FastStringBuilder builder = new FastStringBuilder("[");

            boolean notfirst = false;
            AbstractPrologTerm list = this;

            while (true) {
                if (list.getType() == PrologTermType.LIST) {
                    final PrologList asList = (PrologList) list;

                    if (asList.isNullList()) {
                        break;
                    }

                    if (notfirst) {
                        builder.append(", ");
                    }

                    final AbstractPrologTerm currentHead = asList.getHead();
                    if (currentHead != null) {
                        builder.append(currentHead.toString());
                    }
                    list = asList.getTail();
                } else {
                    if (notfirst) {
                        builder.append('|');
                    }
                    builder.append(list.toString());
                    break;
                }
                notfirst = true;
            }

            builder.append(']');
            result = builder.toString();

        }
        return result;
    }
}
