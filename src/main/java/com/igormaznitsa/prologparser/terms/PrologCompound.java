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

/**
 * Abstract class for all compound prolog terms.
 */
public abstract class PrologCompound extends PrologTerm {
  private static final long serialVersionUID = 723482637840123123L;

  public PrologCompound(final String text) {
    super(text, Quotation.NONE);
  }

  /**
   * Get element for its position.
   *
   * @param position zero-based element position
   * @return element in position
   */
  public abstract PrologTerm getTermAt(int position);
}
