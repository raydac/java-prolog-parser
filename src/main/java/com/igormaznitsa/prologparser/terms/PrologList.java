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

package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.utils.AssertUtils;
import com.igormaznitsa.prologparser.utils.StringBuilderEx;

import java.util.Collections;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * Representation of prolog list term.
 */
public final class PrologList extends PrologStruct implements Iterable<PrologTerm> {
  public static final PrologTerm LIST_FUNCTOR = new PrologAtom(".", Quotation.SINGLE);
  private static final long serialVersionUID = -3781631438477816869L;

  private static final PrologVar EMPTY_ANONYMOUS_VAR = new PrologVar("_");

  public PrologList() {
    super(LIST_FUNCTOR, 2);
    this.elements[0] = null;
    this.elements[1] = null;
  }

  public PrologList(final int line, final int pos) {
    this();
    setPos(pos);
    setLine(line);
  }

  public PrologList(final PrologTerm[] array) {
    this();
    for (final PrologTerm prologTerm : array) {
      this.addAsNewListToEndOfListChain(prologTerm);
    }
  }

  public PrologList(final PrologTerm[] array, final int line, final int pos) {
    this(array);
    setPos(pos);
    setLine(line);
  }

  public PrologList(final PrologTerm head) {
    this();
    setHead(head);
    setTail(new PrologList());
  }

  public PrologList(final PrologTerm head, final int line, final int pos) {
    this(head);
    setPos(pos);
    setLine(line);
  }

  public PrologList(final PrologTerm head, final PrologTerm tail) {
    this();
    setHead(head);
    setTail(tail);
  }

  public PrologList(final PrologTerm head, final PrologTerm tail, final int line, final int pos) {
    this(head, tail);
    setPos(pos);
    setLine(line);
  }

  public static PrologList setTermAsNewListTail(final PrologList list, final PrologTerm term) {
    PrologList result = list;

    if (list.isEmpty()) {
      list.setHead(term);
      list.setTail(new PrologList());
    } else {
      result = new PrologList(term, new PrologList());
      list.setTail(result);
    }

    return result;
  }

  @Override
  public PrologTerm getTermAt(final int index) {
    final PrologTerm result = super.getTermAt(index);
    return result == null ? EMPTY_ANONYMOUS_VAR : result;
  }

  @Override
  public boolean isEmpty() {
    return this.elements[0] == null && this.elements[1] == null;
  }

  /**
   * Get the current head element of the list.
   * @return the head element, can be null
   */
  public PrologTerm getHead() {
    return this.elements[0];
  }

  /**
   * Replace the head element.
   * @param term the new head term, can be null
   */
  public void setHead(final PrologTerm term) {
    this.setElementAt(0, term);
    if (this.elements[1] == null) {
      setTail(new PrologList());
    }
  }

  /**
   * Get the current tail element
   * @return the current tail element, can be null
   */
  public PrologTerm getTail() {
    return this.elements[1];
  }

  /**
   * Set the current tail element
   * @param term the new tail element, can be null
   */
  public void setTail(final PrologTerm term) {
    this.setElementAt(1, term);
    if (this.elements[0] == null) {
      setHead(EMPTY_ATOM);
    }
  }

  /**
   * Add term as new list into the end of the list chain.
   * @param term to be added as new list in the end of chain
   * @return generated list (may be this)
   */
  public PrologList addAsNewListToEndOfListChain(final PrologTerm term) {

    PrologList result = this;

    if (isEmpty()) {
      setHead(term);
      setTail(new PrologList());
    } else {
      while (!Thread.currentThread().isInterrupted()) {
        if (result.isEmpty()) {
          result.setHead(term);
          result.setTail(new PrologList());
          break;
        } else {
          final PrologTerm leftTail = result.elements[1];
          if (leftTail.getType() == TermType.LIST) {
            result = (PrologList) leftTail;
          } else {
            final PrologList newOne = new PrologList(term, new PrologList());
            result.setTail(newOne);
            result = newOne;
            break;
          }
        }
      }
    }
    return result;
  }

  /**
   * Replace last tail element in list chain.
   *
   * @param newTailElement new element
   */
  public void replaceEndListElement(final PrologTerm newTailElement) {
    PrologList current = this;

    while (!Thread.currentThread().isInterrupted()) {
      final PrologTerm tail = current.elements[1];

      if (tail.getType() == TermType.LIST) {
        final PrologList leftTail = (PrologList) tail;
        if (leftTail.isEmpty()) {
          current.setTail(newTailElement);
          break;
        }
      } else {
        current.setTail(newTailElement);
        break;
      }

      current = (PrologList) tail;
    }
  }

  @Override
  public int getArity() {
    return this.isEmpty() ? 0 : 2;
  }

  @Override
  public void setElementAt(final int index, final PrologTerm term) {
    if (index < 0 || index >= 2) {
      throw new ArrayIndexOutOfBoundsException(index);
    }
    this.elements[index] = AssertUtils.assertNotNull(term);
  }


  @Override
  public TermType getType() {
    return TermType.LIST;
  }

  @Override
  public String toString() {
    String result = "[]";

    if (!isEmpty()) {
      final StringBuilderEx builder = new StringBuilderEx("[");

      boolean notFirst = false;
      PrologTerm list = this;

      while (!Thread.currentThread().isInterrupted()) {
        if (list.getType() == TermType.LIST) {
          final PrologList asList = (PrologList) list;

          if (asList.isEmpty()) {
            break;
          }

          if (notFirst) {
            builder.append(", ");
          }

          final PrologTerm currentHead = asList.elements[0];
          if (currentHead != null) {
            builder.append(currentHead.toString());
          }
          list = asList.elements[1];
        } else {
          if (notFirst) {
            builder.append('|');
          }
          builder.append(list.toString());
          break;
        }
        notFirst = true;
      }

      builder.append(']');
      result = builder.toString();

    }
    return result;
  }

  @Override
  public Iterator<PrologTerm> iterator() {
    if (this.isEmpty()) {
      return Collections.emptyIterator();
    } else {
      return new Iterator<PrologTerm>() {

        private PrologTerm head = elements[0];
        private PrologTerm tail = elements[1];

        @Override
        public boolean hasNext() {
          return head != null;
        }

        @Override
        public PrologTerm next() {
          if (this.head == null) {
            throw new NoSuchElementException();
          }
          final PrologTerm result = this.head;

          if (this.tail == null) {
            this.head = null;
          } else {
            if (this.tail instanceof PrologList) {
              final PrologList nextList = (PrologList) this.tail;
              this.head = nextList.elements[0];
              this.tail = nextList.elements[1];
            } else {
              this.head = this.tail;
              this.tail = null;
            }
          }

          return result;
        }
      };
    }
  }

  @Override
  public Stream<PrologTerm> stream() {
    return StreamSupport
        .stream(Spliterators
                .spliteratorUnknownSize(
                    this.iterator(),
                    Spliterator.ORDERED | Spliterator.NONNULL),
            false);
  }
}
