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

import static com.igormaznitsa.prologparser.utils.AssertionUtils.*;
import com.igormaznitsa.prologparser.utils.CharacterProcessor;
import java.io.Serializable;

/**
 * The abstract class describes an abstract prolog term for the prolog parser.
 * All data types being used by the prolog parser are successors of the class.
 *
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 */
public abstract class AbstractPrologTerm extends CharacterProcessor implements Serializable {

    private static final long serialVersionUID = 1482429096900255841L;
    /**
     * The variable save any Java object, it is not being used by the prolog
     * parser but allows a user to link some data to the structure, the default
     * value is null
     */
    protected Object linkedObject;
    /**
     * The variable contains the text for the term
     */
    protected final String text;
    /**
     * The variable contains the line number for the term in the source stream
     */
    private int lineNumber;
    /**
     * The variable contains the string position of the first term char in the
     * source stream
     */
    private int strPosition;

    /**
     * The constructor allows to make new instance based on a text value. The
     * string position and the line number will have -1 after the constructor.
     *
     * @param text the text representing the term, must not be null
     */
    public AbstractPrologTerm(final String text) {
        checkNotNull("Term text is null", text);
        this.text = text;
        this.strPosition = -1;
        this.lineNumber = -1;
    }

    /**
     * The constructor allows to make new instance based on a text value and set
     * both the string position and the line number values
     *
     * @param text the text representing the term, must not be null
     * @param strPosition the string position of the first term char in the
     * source stream, the first char is 1
     * @param lineNumber the line number of the first term char in the source
     * stream, the first line is 1
     */
    public AbstractPrologTerm(final String text, final int strPosition, final int lineNumber) {
        this(text);
        setStrPosition(strPosition);
        setLineNumber(lineNumber);
    }

    /**
     * Get the string position of the first char of the term in the source
     * stream.
     *
     * @return the string position as integer value, the first line char is 1,
     * it is -1 if undefined
     */
    public final int getStrPosition() {
        return strPosition;
    }

    /**
     * Set the string position of the term in the source stream. If the position
     * is zero or less than zero then the string position will be -1.
     *
     * @param strPosition the value to be set as the string position
     */
    public final void setStrPosition(final int strPosition) {
        this.strPosition = strPosition <= 0 ? -1 : strPosition;
    }

    /**
     * Get the line number of the first term char in the source stream.
     *
     * @return the line number as integer value, the first line is 1, if
     * undefined then -1
     */
    public final int getLineNumber() {
        return lineNumber;
    }

    /**
     * Set the first term char line number in the source stream
     *
     * @param lineNumber the line number, if it is zero or less then the value
     * will be -1
     */
    public final void setLineNumber(final int lineNumber) {
        this.lineNumber = lineNumber <= 0 ? -1 : lineNumber;
    }

    /**
     * Get the text representation of the term
     *
     * @return the text as String
     */
    public String getText() {
        return this.text;
    }

    /**
     * Get the priority of the term
     *
     * @return the priority as integer
     */
    public int getPriority() {
        return 0;
    }

    /**
     * Get a text prolog like representation for the term.
     *
     * @return the text representation as a String
     */
    @Override
    public String toString() {
        return text;
    }

    /**
     * Set the linked object for the term
     *
     * @param obj the new linked object, it can be null
     */
    public void setLinkedObject(final Object obj) {
        this.linkedObject = obj;
    }

    /**
     * Get the linked object for the term
     *
     * @return the linked object, it can be null
     */
    public Object getLinkedObject() {
        return this.linkedObject;
    }

    /**
     * Get the term type
     *
     * @return the term type
     */
    public abstract PrologTermType getType();
}
