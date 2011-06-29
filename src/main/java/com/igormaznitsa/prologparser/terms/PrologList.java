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
package com.igormaznitsa.prologparser.terms;

/**
 * The class describes a prolog list being used by the prolog parser.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.00
 */
public final class PrologList extends PrologStructure {
	/**
	 * The functor is used for a structure describing a prolog lisp
	 * 
	 * @since 1.00
	 */
	public static final AbstractPrologTerm LIST_FUNCTOR = new PrologAtom(".");

	/**
	 * A Constructor. It allows to create an empty ([]) list.
	 * 
	 * @since 1.00
	 */
	public PrologList() {
		super(LIST_FUNCTOR, 2);
		elements[0] = null;
		elements[1] = null;
	}

	/**
	 * A Constructor. It allows to create a prolog list chain from an array
	 * contains prolog term objects.
	 * 
	 * @param array
	 *            an array contains prolog term objects, the array must not be
	 *            null and it must not contains null elements.
	 * @since 1.00
	 */
	public PrologList(final AbstractPrologTerm[] array) {
		this();
		if (array == null)
			throw new NullPointerException("Array is null");

		PrologList current = this;

		for (final AbstractPrologTerm term : array) {
			if (term == null)
				throw new NullPointerException("Array contains a null element");
			current = current.addAsNewListToEndOfListChain(term);
		}
	}

	/**
	 * A Constructor. It allows to create a list and set the argument as the
	 * list head element.
	 * 
	 * @param head
	 *            a term to be used as the list head. Must not be null;
	 * @since 1.00
	 */
	public PrologList(final AbstractPrologTerm head) {
		this();
		setHead(head);
		setTail(new PrologList());
	}

	/**
	 * A Constructor. It allows to create a list and set the head and the tail.
	 * 
	 * @param head
	 *            a term to be used as the head for the list. Must not be null.
	 * @param tail
	 *            a term to be used as the tail for the list. Must not be null.
	 * @since 1.00
	 */
	public PrologList(final AbstractPrologTerm head,
			final AbstractPrologTerm tail) {
		this();
		setHead(head);
		setTail(tail);
	}

	/**
	 * Check that the list instance is the null list.
	 * 
	 * @return true if the list is a null list ([]), else false
	 * @since 1.00
	 */
	public boolean isNullList() {
		return getHead() == null && getTail() == null;
	}

	/**
	 * Get the head of the list.
	 * 
	 * @return the head of the list, if it is a null list then it will return
	 *         null.
	 * @since 1.00
	 */
	public AbstractPrologTerm getHead() {
		return getElement(0);
	}

	/**
	 * Get the tail of the list.
	 * 
	 * @return the tail of the list, if it is a null list then it will return
	 *         null
	 * @since 1.00
	 */
	public AbstractPrologTerm getTail() {
		return getElement(1);
	}

	/**
	 * Set the head for the list.
	 * 
	 * @param term
	 *            a term to be used as the head for the list, must not be null.
	 * @since 1.00
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
	 * @since 1.00
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
	 * @since 1.00
	 */
	public static PrologList setTermAsNewListTail(PrologList list,
			final AbstractPrologTerm term) {
		if (list == null)
			throw new NullPointerException("The list is null");
		if (term == null)
			throw new NullPointerException("Term is null");

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
	 * @since 1.00
	 */
	public final PrologList addAsNewListToEndOfListChain(
			final AbstractPrologTerm term) {
		if (term == null)
			throw new NullPointerException("Term is null");

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
					AbstractPrologTerm ltail = current.getTail();
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
	 * @since 1.00
	 */
	public final void replaceLastElement(
			final AbstractPrologTerm elementToReplace) {
		if (elementToReplace == null)
			throw new NullPointerException("The element is null");

		PrologList curList = this;
		while (true) {
			AbstractPrologTerm tail = curList.getTail();
			if (tail.getType() == PrologTermType.LIST) {
				final PrologList ltail = (PrologList) tail;
				if (ltail.isNullList()) {
					curList.setTail(elementToReplace);
					break;
				} else
					curList = ltail;
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
			StringBuilder builder = new StringBuilder("[");

			boolean notfirst = false;
			AbstractPrologTerm list = this;

			while (true) {
				if (list.getType() == PrologTermType.LIST) {
					final PrologList asList = (PrologList) list;

					if (asList.isNullList())
						break;

					if (notfirst) {
						builder.append(", ");
					}

					final AbstractPrologTerm currentHead = asList.getHead();
					if (currentHead != null)
						builder.append(currentHead.toString());
					list = asList.getTail();
				} else {
					if (notfirst) {
						builder.append('|');
					}
					if (list != null)
						builder.append(list.toString());
					break;
				}
				notfirst = true;
			}

			builder.append(']');
			return builder.toString();

		}
		return result;
	}
}
