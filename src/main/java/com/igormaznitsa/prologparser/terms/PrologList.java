package com.igormaznitsa.prologparser.terms;

import com.igormaznitsa.prologparser.utils.StrBuffer;

public final class PrologList extends PrologStructure {
  public static final AbstractPrologTerm LIST_FUNCTOR = new PrologAtom(".");
  private static final long serialVersionUID = -3781638438477876869L;

  public PrologList() {
    super(LIST_FUNCTOR, 2);
    elements[0] = null;
    elements[1] = null;
  }

  public PrologList(final int strPosition, final int lineNumber) {
    this();
    setStrPosition(strPosition);
    setLineNumber(lineNumber);
  }

  public PrologList(final AbstractPrologTerm[] array) {
    this();

    PrologList current = this;

    for (final AbstractPrologTerm term : array) {
      current = current.addAsNewListToEndOfListChain(term);
    }
  }

  public PrologList(final AbstractPrologTerm[] array, final int strPos, final int lineNumber) {
    this(array);
    setStrPosition(strPos);
    setLineNumber(lineNumber);
  }

  public PrologList(final AbstractPrologTerm head) {
    this();
    setHead(head);
    setTail(new PrologList());
  }

  public PrologList(final AbstractPrologTerm head, final int strPosition, final int lineNumber) {
    this(head);
    setStrPosition(strPosition);
    setLineNumber(lineNumber);
  }

  public PrologList(final AbstractPrologTerm head,
                    final AbstractPrologTerm tail) {
    this();
    setHead(head);
    setTail(tail);
  }

  public PrologList(final AbstractPrologTerm head, final AbstractPrologTerm tail, final int strPosition, final int lineNumber) {
    this(head, tail);
    setStrPosition(strPosition);
    setLineNumber(lineNumber);
  }

  public static PrologList setTermAsNewListTail(final PrologList list,
                                                final AbstractPrologTerm term) {
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

  public AbstractPrologTerm getHead() {
    return getElement(0);
  }

  public void setHead(final AbstractPrologTerm term) {
    this.setElement(0, term);
    if (getTail() == null) {
      setTail(new PrologList());
    }
  }

  public AbstractPrologTerm getTail() {
    return getElement(1);
  }

  public void setTail(final AbstractPrologTerm term) {
    this.setElement(1, term);
    if (getHead() == null) {
      setHead(EMPTY_ATOM);
    }
  }

  public PrologList addAsNewListToEndOfListChain(
      final AbstractPrologTerm term) {

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

  public void replaceLastElement(
      final AbstractPrologTerm elementToReplace) {

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

  @Override
  public PrologTermType getType() {
    return PrologTermType.LIST;
  }

  @Override
  public String toString() {
    String result = "[]";

    if (!isNullList()) {
      final StrBuffer builder = new StrBuffer("[");

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
