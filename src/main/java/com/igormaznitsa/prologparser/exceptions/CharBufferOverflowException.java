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
 * Specialized exception shows that allowed char buffer size of one buffers in a tokenizer is reached.
 */
public class CharBufferOverflowException extends PrologParserException {

  private static final long serialVersionUID = 78123812638712L;

  private final String bufferText;

  /**
   * Constructor.
   *
   * @param bufferText current buffer content which is cause of the error, can be null but in the case it will be converted into empty string.
   */
  public CharBufferOverflowException(final String bufferText) {
    super("Char buffer limit is reached", -1, -1);
    this.bufferText = bufferText == null ? "" : bufferText;
  }

  /**
   * Get buffer text content which is cause of the error.
   *
   * @return the buffer text, can be empty but can't be null.
   */
  public String getBufferText() {
    return this.bufferText;
  }
}
