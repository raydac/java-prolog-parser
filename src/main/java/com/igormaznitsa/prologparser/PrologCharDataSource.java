/*
 * Copyright 2011-2012 Igor Maznitsa (http://www.igormaznitsa.com)
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
package com.igormaznitsa.prologparser;

import static com.igormaznitsa.prologparser.utils.AssertionUtils.*;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.util.ArrayDeque;
import java.util.Deque;

/**
 * The class is the main char data source for a prolog parser, the class adapts
 * different standard Java input stream classes to be used by a prolog parser.
 * The class is not thread safe so it must not be simultaneously used from
 * different threads.
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 */
public class PrologCharDataSource {

    /**
     * The Inside buffer to keep chars
     */
    private static final class InsideCharBuffer {
        private int position = 0;
        private char[] buffer;
        private int border;

        InsideCharBuffer(final int capacity) {
            buffer = new char[capacity];
            border = capacity - 1;
        }

        private void doubleInsideBuffer() {
            final int size = buffer.length << 1;
            final char[] newbuffer = new char[size];
            border = size - 1;
            System.arraycopy(buffer, 0, newbuffer, 0, position);
            buffer = newbuffer;
        }

        public boolean isEmpty() {
            return position == 0;
        }

        public void addLast(final char chr) {
            if (position == border) {
                doubleInsideBuffer();
            }
            buffer[position++] = chr;
        }

        public char removeLast() {
            return buffer[--position];
        }
    }
    /**
     * The text reader which is being used by the reader to read incoming text
     * data
     */
    private final Reader inReader;
    /**
     * Inside char stack to save back-pushed data
     */
    private final InsideCharBuffer insideCharBuffer = new InsideCharBuffer(16);
    /**
     * The variable contains the previous value of the string position indicator
     */
    private int strPosPrev;
    /**
     * The variable contains the previous position of the line number indicator
     */
    private int lineNumPrev;
    /**
     * The variable contains current value of the string position indicator
     */
    private int strPos;
    /**
     * The variable contains current value of the line number indicator
     */
    private int lineNum;

    /**
     * A constructor. To make a reader based on a String object.
     *
     * @param string A string object which will be used as the source for the
     * reader, must not be null
     */
    public PrologCharDataSource(final String string) {
        this(new StringReader(string));
    }

    /**
     * A constructor. To make a reader based on an input stream (with the
     * default charset)
     *
     * @param inStream an input stream object which will be used as the source
     * for the reader, must not be null
     */
    public PrologCharDataSource(final InputStream inStream) {
        this(new InputStreamReader(inStream));
    }

    /**
     * A constructor. To make a reader based on a channel
     *
     * @param channel the channel to be used as the data source, must not be
     * null
     */
    public PrologCharDataSource(final ReadableByteChannel channel) {
        this(Channels.newInputStream(channel));
        checkNotNull("Channel is null", channel);
    }

    /**
     * A constructor. To make a reader based on a java reader object.
     *
     * @param reader a java reader object, must not be null
     */
    public PrologCharDataSource(final Reader reader) {
        checkNotNull("Reader is null", reader);

        inReader = reader;
        strPos = 1;
        lineNum = 1;
        strPosPrev = strPos;
        lineNumPrev = lineNum;
    }

    /**
     * Read next char code from the reader
     *
     * @return the next char code or -1 if the stream end has been reached
     * @throws IOException it will be thrown if there is any transport error
     * during the operation
     */
    public int read() throws IOException {
        int ch;
        if (insideCharBuffer.isEmpty()) {
            ch = inReader.read();
        } else {
            ch = insideCharBuffer.removeLast();
        }

        strPosPrev = strPos;
        lineNumPrev = lineNum;
        if (ch == '\n') {
            strPos = 1;
            lineNum++;
        } else {
            if (ch >= 0) {
                strPos++;
            }
        }
        return ch;
    }

    /**
     * Push back the difference between an etalon string and a string buffer
     * content. For instance if there are 'test' as the etalon and a string
     * buffer containing 'testhello' then part 'hello' will be pushed back into
     * inside buffer to be used in next read operations.
     *
     * @param etalon an etalon string must not be null
     * @param buffer a string buffer object, must not be null
     */
    public void calculateDifferenceAndPushTheResultBack(
            final String etalon, final FastStringBuilder buffer) {
        int chars = buffer.length() - etalon.length();
        int pos = buffer.length() - 1;

        final InsideCharBuffer charBuffer = this.insideCharBuffer;

        while (chars > 0) {
            final char ch = buffer.charAt(pos--);
            charBuffer.addLast(ch);
            chars--;
            strPos--;
            if (strPos < 1) {
                strPos = 1;
            }
            strPosPrev = strPos;
            if (ch == '\n') {
                lineNum--;
                if (lineNum < 1) {
                    lineNum = 1;
                }
                lineNumPrev = lineNum;
            }
        }
    }

    /**
     * Get the previous line number, the first line is 1
     *
     * @return the previous line number
     */
    public int getPrevLineNumber() {
        return lineNumPrev;
    }

    /**
     * Get the previous value of the next char string position indicator, the
     * first char is 1
     *
     * @return the previous value of the next char string position
     */
    public int getPreviousNextCharStringPosition() {
        return strPosPrev;
    }

    /**
     * Get current line number, the first line is 1
     *
     * @return the line number as integer
     */
    public int getLineNumber() {
        return lineNum;
    }

    /**
     * Get current next char string position, the first char is 1
     *
     * @return the next char string position as integer
     */
    public int getNextCharStringPosition() {
        return strPos;
    }

    /**
     * Push a char back into the inside buffer to be read into the next read
     * operation
     *
     * @param ch the char to be placed into the inside buffer
     */
    public void pushCharBack(final char ch) {
        insideCharBuffer.addLast(ch);
        if (ch == '\n') {
            strPos = 1;
            lineNum--;
            if (lineNum <= 0) {
                lineNum = 1;
            }
        } else {
            strPos--;
            if (strPos <= 0) {
                strPos = 1;
            }
        }
    }

    /**
     * Close the current reader
     *
     * @throws IOException it will be thrown if there is any error during the
     * operation
     */
    public void close() throws IOException {
        inReader.close();
    }
}
