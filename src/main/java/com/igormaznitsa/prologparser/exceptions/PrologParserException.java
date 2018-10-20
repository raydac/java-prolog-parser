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

package com.igormaznitsa.prologparser.exceptions;

/**
 * Exception is thrown if any problem with parsing of prolog sources.
 */
public class PrologParserException extends RuntimeException {
  private static final long serialVersionUID = -4404323844125857006L;

  protected final int line;
  protected final int pos;

  /**
   * Constructor
   *
   * @param text text of parsing error
   * @param line line where the error has been detected
   * @param pos  line position where the error has been detected
   */
  public PrologParserException(final String text, final int line, final int pos) {
    this(text, line, pos, null);
  }

  /**
   * Constructor
   *
   * @param text  text of parsing error
   * @param line  line where the error has been detected
   * @param pos   line position where the error has been detected
   * @param cause the root exception
   */
  public PrologParserException(final String text, final int line, final int pos, final Throwable cause) {
    super(text, cause);
    this.line = line;
    this.pos = pos;
  }

  /**
   * Get the error line
   *
   * @return the error line, -1 if the line is undefined
   */
  public int getLine() {
    return this.line;
  }

  /**
   * Get the error line position
   *
   * @return the error line position, -1 if the position is undefined
   */
  public int getPos() {
    return this.pos;
  }

  /**
   * Check that the exception contains valid position
   *
   * @return true if position and line have been defined in the exception
   */
  public boolean hasValidPosition() {
    return this.line > 0 && this.pos > 0;
  }

  @Override
  public String toString() {
    return String.format("%s[%d:%d]", this.getMessage(), this.line, this.pos);
  }
}
