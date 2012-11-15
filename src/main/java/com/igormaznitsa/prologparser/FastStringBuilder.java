package com.igormaznitsa.prologparser;

public final class FastStringBuilder {

    private char[] charArray;
    private int pointer;
    private int maxPosition;

    public FastStringBuilder(final String initial) {
        this(Math.max(initial.length()<<1,64));
        append(initial);
    }

    public FastStringBuilder(final int capacity) {
        charArray = new char[capacity];
        maxPosition = capacity - 1;
    }

    private void doubleBuffer() {
        final int newlen = charArray.length << 1;
        final char[] newbuffer = new char[newlen];
        System.arraycopy(charArray, 0, newbuffer, 0, pointer);
        charArray = newbuffer;
        maxPosition = newlen - 1;
    }

    public FastStringBuilder append(final char chr) {
        if (pointer == maxPosition) {
            doubleBuffer();
        }
        charArray[pointer++] = chr;
        return this;
    }

    public int length() {
        return this.pointer;
    }

    public char charAt(final int position) {
        return charArray[position];
    }

    public int indexOf(final char chr) {
        final char[] local = charArray;
        final int localp = pointer;
        for (int i = 0; i < localp; i++) {
            if (local[i] == chr) {
                return i;
            }
        }
        return -1;
    }

    public boolean hasOnlyChar(final char chr) {
        if (pointer != 1) {
            return false;
        }
        return charArray[0] == chr;
    }

    public boolean hasLastChar(final char chr) {
        if (pointer == 0) {
            return false;
        }
        return charArray[pointer - 1] == chr;
    }

    public String toStringExcludeLastChar() {
        if (pointer == 0) {
            throw new IndexOutOfBoundsException("The buffer is empty");
        }
        return new String(charArray, 0, pointer - 1);
    }

    public String substring(final int offset, final int count) {
        final int maxpos = offset + count;
        if (maxpos > pointer) {
            throw new IndexOutOfBoundsException("The end of the substring is out of bound of the inside buffer [" + maxpos + ']');
        }
        return new String(charArray, offset, count);
    }

    public FastStringBuilder append(final String str) {
        int strlen = str.length();
        while (true) {
            final int newlen = pointer + strlen;
            if (newlen <= maxPosition) {
                break;
            }
            doubleBuffer();
        }
        
        final char [] localarray = charArray;
        
        int index = 0;
        int localpointer = pointer;
        while(strlen!=0){
            localarray [localpointer++] = str.charAt(index++);
            strlen--;
        }
        pointer = localpointer;
        return this;
    }

    public void setLength(final int length) {
        pointer = length;
    }

    @Override
    public String toString() {
        return new String(charArray, 0, pointer);
    }
}
