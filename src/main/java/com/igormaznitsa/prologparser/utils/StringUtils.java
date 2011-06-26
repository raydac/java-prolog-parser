/*
 * Copyright 2011 Igor Maznitsa (http://www.igormaznitsa.com)
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of version 2.1 of the GNU Lesser General Public
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
package com.igormaznitsa.prologparser.utils;

/**
 * The class contains misc auxiliary string functions and classes.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @version 1.00
 */
public final class StringUtils {

	/**
	 * It is a parameterized class allows to make a container saves a mutable value (I mean that the value can be changed).
	 * The class is not a thread safe one.
	 * 
	 * @author Igor Maznitsa (http://www.igormaznitsa.com)
	 * @version 1.00
	 * @param <T> the class of a carried object
	 */
	public final static class Mutable<T> {
		/**
		 * The variable contains the value carried by the container 
		 */
		private T value;

		/**
		 * A Constructor. It allows to create a container with null.
		 */
		public Mutable() {
			value = null;
		}

		/**
		 * A Constructor. It allows to create a container for a defined value.
		 * @param initValue the init value for the new container, it can be null.
		 */
		public Mutable(final T initValue) {
			value = initValue;
		}

		/**
		 * Set new carried value.
		 * @param newValue the new value for the container, it can be null.
		 */
		public void set(final T newValue) {
			value = newValue;
		}

		/**
		 * Get the current carried value.
		 * @return the current value, it can be null.
		 */
		public T get() {
			return value;
		}
	}

	/**
	 * The constructor. It is a private one to disable making of class instances.
	 */
	private StringUtils() {
	}

	/**
	 * The function can convert a string value describes a special escape char into its code.
	 * To understand that the string has been decoded successfully you can if the result has non-null value and the function has returned true.
	 * If the function has returned false and the result contains the null value then there is error in data format.
	 * If the function has returned false but the result contains not-null data, it signals that there is not enough information.
	 * 
	 * @param afterString the string to be decoded
	 * @param result the container which will contain the result after the execution or null
	 * @return true if the string has been decoded successfully, false if it can't be decoded or there is not enough information to decode
	 */
	public static boolean unescapeCharacter(final String afterString,
			final Mutable<Character> result) {
		if (result == null)
			throw new NullPointerException("Result object must not be null");

		if (afterString == null) {
			result.set(null);
			return false;
		}

		final int len = afterString.length();

		if (len == 1) {
			switch (afterString.charAt(0)) {
			case 'a':
				result.set((char) 7);
				return true;
			case 'b':
				result.set((char) 8);
				return true;
			case 'n':
				result.set('\n');
				return true;
			case 'r':
				result.set('\r');
				return true;
			case 'e':
				result.set((char) 27);
				return true;
			case 't':
				result.set('\t');
				return true;
			case 's':
				result.set((char) 32);
				return true;
			case 'v':
				result.set((char) 11);
				return true;
			case '\\':
				result.set('\\');
				return true;
			case '\'':
				result.set('\'');
				return true;

			case 'u':
			case 'x':
				result.set(null);
				return true;

			default:
				result.set(null);
				return false;
			}
		} else {
			switch (afterString.charAt(0)) {
			case 'u': {
				int num = -1;
				try {
					num = Integer.parseInt(afterString.substring(1), 16);
				} catch (NumberFormatException ex) {
					result.set(null);
					return false;
				}

				if (len == 5) {
					result.set((char) num);
					return true;
				} else {
					if (len > 5) {
						result.set(null);
						return false;
					}
					result.set(null);
					return true;
				}
			}
			case 'x': {
				int num = -1;
				try {
					num = Integer.parseInt(afterString.substring(1), 16);
				} catch (NumberFormatException ex) {
					result.set(null);
					return false;
				}

				if (len == 3) {
					result.set((char) num);
					return true;
				} else {
					if (len > 3) {
						result.set(null);
						return false;
					}
					result.set(null);
					return true;
				}
			}
			default: {
				result.set(null);
				return false;
			}
			}
		}
	}

	/**
	 * The function allows to escape a string and replace special chars by special escape sequences.
	 * @param str the string to be escaped, must not be null
	 * @return an escaped string.
	 */
	public static String escapeString(final String str) {
		final StringBuilder result = new StringBuilder(str.length() * 3 / 2);

		for (final char chr : str.toCharArray()) {
			switch (chr) {
			case 7:
				result.append("\\a");
				break;
			case 8:
				result.append("\\b");
				break;
			case '\n':
				result.append("\\n");
				break;
			case '\r':
				result.append("\\r");
				break;
			case 27:
				result.append("\\e");
				break;
			case '\t':
				result.append("\\t");
				break;
			case '\'':
				result.append("\\'");
				break;
			case 11:
				result.append("\\v");
				break;
			default:
				result.append(chr);
				break;
			}
		}

		return result.toString();
	}
}
