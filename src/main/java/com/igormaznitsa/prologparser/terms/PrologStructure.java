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

import java.util.Arrays;

import com.igormaznitsa.prologparser.operators.Operator;

/**
 * The class describes a prolog structure.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.01
 */
public class PrologStructure extends AbstractPrologTerm {
	/**
	 * An auxiliary constant contains an empty prolog term array
	 * 
	 * @since 1.00
	 */
	public static final AbstractPrologTerm[] EMPTY_TERM_ARRAY = new AbstractPrologTerm[0];

	/**
	 * An auxiliary constant contains an empty atom, as empty I mean that the
	 * atom contains the empty string
	 * 
	 * @since 1.00
	 */
	public static final PrologAtom EMPTY_ATOM = new PrologAtom("");

	/**
	 * The functor of the structure
	 * 
	 * @since 1.00
	 */
	protected final AbstractPrologTerm functor;

	/**
	 * The array contains structure elements
	 * 
	 * @since 1.00
	 */
	protected final AbstractPrologTerm[] elements;

	/**
	 * A Constructor. It allows to create a structure for a functor and a term
	 * array
	 * 
	 * @param functor
	 *            the functor for the new structure, must not be null
	 * @param elements
	 *            the elements of the new structure, must not be null and must
	 *            not contain null (!)
	 * @since 1.00
	 */
	public PrologStructure(final AbstractPrologTerm functor,
			final AbstractPrologTerm[] elements) {
		super(functor.getText());

		if (functor.getType() != PrologTermType.ATOM
				&& functor.getType() != PrologTermType.OPERATOR)
			throw new IllegalArgumentException(
					"Functor must be either atom or operator");
		if (functor instanceof AbstractPrologNumericTerm)
			throw new IllegalArgumentException("Number can't be a functor");

		if (elements == null)
			throw new NullPointerException("Elements array is null");

		this.functor = functor;
		this.elements = elements.clone();

		for (int li = 0; li < this.elements.length; li++) {
			if (this.elements[li] == null) {
				throw new NullPointerException(
						"There is a null element at the " + li
								+ " index, use EMPTY_ATOM instead null");
			}
		}
	}

	/**
	 * A Constructor. It allows to create a zero (I mean a zero arity one)
	 * structure with a prolog atom as the functor.
	 * 
	 * @param text
	 *            the text to create the functor, must not be null
	 * @since 1.00
	 */
	public PrologStructure(final String text) {
		this(new PrologAtom(text), 0);
	}

	/**
	 * A Constructor. It allows to create a zero (I mean a zero arity one)
	 * structure with a prolog term as a functor
	 * 
	 * @param functor
	 *            a prolog term to be used as the functor, must not be null
	 * @since 1.00
	 */
	public PrologStructure(final AbstractPrologTerm functor) {
		this(functor, 0);
	}

	/**
	 * A Constructor. It allows to create a prolog structure for a functor and
	 * needed arity (it will use EMPTY_ATOM as each element)
	 * 
	 * @param functor
	 *            a prolog term to be used as the structure functor, it must not
	 *            be null.
	 * @param arity
	 *            the arity of the new structure, must not be less than zero.
	 * @since 1.00
	 */
	protected PrologStructure(final AbstractPrologTerm functor, final int arity) {
		super(functor.getText());

		if (functor.getType() != PrologTermType.ATOM
				&& functor.getType() != PrologTermType.OPERATOR
				&& functor.getType() != PrologTermType.OPERATORS) {
			throw new IllegalArgumentException(
					"Wrong functor type, must be either atom or operator(s)");
		}

		if (functor instanceof AbstractPrologNumericTerm) {
			throw new IllegalArgumentException(
					"Functor must not be a numeric term");
		}

		if (arity < 0)
			throw new IllegalArgumentException("Negative arity");

		this.functor = functor;
		elements = new AbstractPrologTerm[arity];
		Arrays.fill(elements, EMPTY_ATOM);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public PrologTermType getType() {
		return PrologTermType.STRUCT;
	}

	/**
	 * Get the arity of the structure
	 * 
	 * @return the arity as integer
	 * @since 1.00
	 */
	public int getArity() {
		return elements.length;
	}

	/**
	 * Get a structure element for an index
	 * 
	 * @param index
	 *            the index of the element, the first element is 0
	 * @return the structure element at the needed position, it can't be null
	 *         (!)
	 * @since 1.00
	 */
	public AbstractPrologTerm getElement(final int index) {
		return elements[index];
	}

	/**
	 * Set a structure element at a position by a prolog term
	 * 
	 * @param index
	 *            the position of the element, the first is 0
	 * @param term
	 *            the term to be set into the position, must not be null
	 * @since 1.00
	 */
	public void setElement(final int index, final AbstractPrologTerm term) {
		if (index < 0 || index >= getArity())
			throw new ArrayIndexOutOfBoundsException();
		if (term == null)
			throw new NullPointerException(
					"You can't set null as a structure element");
		elements[index] = term;
	}

	/**
	 * Get the functor of the structure
	 * 
	 * @return the functor
	 * @since 1.00
	 */
	public AbstractPrologTerm getFunctor() {
		return functor;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public int getPriority() {
		if (functor.getType() == PrologTermType.OPERATOR) {
			return functor.getPriority();
		} else {
			return 0;
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		if (functor.getType() == PrologTermType.OPERATOR) {
			// an operator based struct
			final Operator operatorFunctor = (Operator) functor;
			final String opName = operatorFunctor.getText();
			final int priority = operatorFunctor.getPriority();

			final String text = getElement(0).toString();
			final String text2 = getArity() > 1 ? getElement(1).toString()
					: null;

			switch (operatorFunctor.getOperatorType()) {
			case FX: {
				builder.append(opName).append(' ');

				if (getElement(0).getPriority() >= priority) {
					builder.append('(').append(text).append(')');
				} else {
					builder.append(text);
				}
			}
				break;
			case FY: {
				builder.append(opName);
				builder.append(' ');

				if (getElement(0).getPriority() > priority) {
					builder.append('(').append(text).append(')');
				} else {
					builder.append(text);
				}
			}
				break;
			case XF: {
				if (getElement(0).getPriority() >= priority) {
					builder.append('(').append(text).append(')');
				} else {
					builder.append(text);
				}

				builder.append(' ').append(opName);
			}
				break;
			case YF: {
				if (getElement(0).getPriority() > priority) {
					builder.append('(').append(text).append(')');
				} else {
					builder.append(text);
				}

				builder.append(' ').append(opName);
			}
				break;
			case XFX: {
				if (getElement(0).getPriority() >= priority) {
					builder.append('(').append(text).append(')');
				} else {
					builder.append(text);
				}

				builder.append(' ').append(opName).append(' ');

				if (getElement(1).getPriority() >= priority) {
					builder.append('(').append(text2).append(')');
				} else {
					builder.append(text2);
				}
			}
				break;
			case YFX: {
				if (getElement(0).getPriority() > priority) {
					builder.append('(').append(text).append(')');
				} else {
					builder.append(text);
				}

				builder.append(' ').append(opName).append(' ');

				if (getElement(1).getPriority() >= priority) {
					builder.append('(').append(text2).append(')');
				} else {
					builder.append(text2);
				}
			}
				break;
			case XFY: {
				if (getElement(0).getPriority() >= priority) {
					builder.append('(').append(text).append(')');
				} else {
					builder.append(text);
				}

				builder.append(' ').append(opName).append(' ');

				if (getElement(1).getPriority() > priority) {
					builder.append('(').append(text2).append(')');
				} else {
					builder.append(text2);
				}
			}
				break;
			default:
				throw new Error(
						"Unsupported operator type detected, it is a programming error.");
			}

		} else {
			String functorText = functor.getText();

			if (functorText.equals("!")) {
				// special structure detected
				if (getArity() == 0)
					return functorText;
			}

			// just structure
			functorText = functor.toString();
			builder.append(functorText);
			builder.append('(');
			boolean next = false;
			for (final AbstractPrologTerm term : elements) {
				if (next) {
					builder.append(", ");
				} else {
					next = true;
				}
				builder.append(term);
			}
			builder.append(')');

		}
		return builder.toString();
	}
}
