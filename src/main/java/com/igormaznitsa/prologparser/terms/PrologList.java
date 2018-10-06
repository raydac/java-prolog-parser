package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.utils.StringBuilderEx;

public final class PrologList extends PrologStruct {
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
    return getElement(0);
  }

  public void setHead(final PrologTerm term) {
    this.setElement(0, term);
    if (getTail() == null) {
      setTail(new PrologList());
    }
  }

  public PrologTerm getTail() {
    return getElement(1);
  }

  public void setTail(final PrologTerm term) {
    this.setElement(1, term);
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
          if (ltail.getType() == TermType.LIST) {
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
      if (tail.getType() == TermType.LIST) {
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
  public TermType getType() {
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
        if (list.getType() == TermType.LIST) {
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
}
