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
  public static final PrologTerm LIST_FUNCTOR = new PrologAtom(".");
  private static final long serialVersionUID = -3781631438477876869L;

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

    PrologList current = this;

    for (final PrologTerm term : array) {
      current = current.addAsNewListToEndOfListChain(term);
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

    if (list.isNullList()) {
      list.setHead(term);
      list.setTail(new PrologList());
    } else {
      result = new PrologList(term, new PrologList());
      list.setTail(result);
    }

    return result;
  }

  public boolean isNullList() {
    return getHead() == null && getTail() == null;
  }

  public PrologTerm getHead() {
    return getElementAt(0);
  }

  public void setHead(final PrologTerm term) {
    this.setElementAt(0, term);
    if (getTail() == null) {
      setTail(new PrologList());
    }
  }

  public PrologTerm getTail() {
    return getElementAt(1);
  }

  public void setTail(final PrologTerm term) {
    this.setElementAt(1, term);
    if (getHead() == null) {
      setHead(EMPTY_ATOM);
    }
  }

  public PrologList addAsNewListToEndOfListChain(
      final PrologTerm term) {

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
          final PrologTerm ltail = current.getTail();
          if (ltail.getTermType() == TermType.LIST) {
            current = (PrologList) ltail;
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

  public void replaceTail(final PrologTerm newTailElement) {

    PrologList curList = this;
    while (true) {
      final PrologTerm tail = curList.getTail();
      if (tail.getTermType() == TermType.LIST) {
        final PrologList ltail = (PrologList) tail;
        if (ltail.isNullList()) {
          curList.setTail(newTailElement);
          break;
        }
      } else {
        curList.setTail(newTailElement);
        break;
      }
      curList = (PrologList) tail;
    }
  }

  @Override
  public TermType getTermType() {
    return TermType.LIST;
  }

  @Override
  public String toString() {
    String result = "[]";

    if (!isNullList()) {
      final StringBuilderEx builder = new StringBuilderEx("[");

      boolean notfirst = false;
      PrologTerm list = this;

      while (true) {
        if (list.getTermType() == TermType.LIST) {
          final PrologList asList = (PrologList) list;

          if (asList.isNullList()) {
            break;
          }

          if (notfirst) {
            builder.append(", ");
          }

          final PrologTerm currentHead = asList.getHead();
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

  @Override
  public Iterator<PrologTerm> iterator() {
    if (this.isNullList()) {
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
              this.head = nextList.getHead();
              this.tail = nextList.getTail();
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
