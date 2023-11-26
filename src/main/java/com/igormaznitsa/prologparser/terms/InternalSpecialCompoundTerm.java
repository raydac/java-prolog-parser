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

import java.util.stream.Stream;

/**
 * Base class for special service terms for use on level of parser.
 */
@SuppressWarnings("unused")
public abstract class InternalSpecialCompoundTerm extends PrologCompound {

  private static final long serialVersionUID = 71286378126323213L;

  public InternalSpecialCompoundTerm(final String text) {
    super(text);
  }

  @Override
  public PrologTerm getTermAt(int position) {
    throw new UnsupportedOperationException();
  }

  @Override
  public final Stream<PrologTerm> stream() {
    throw new UnsupportedOperationException();
  }

  @Override
  public final int compareTo(PrologTerm that) {
    throw new UnsupportedOperationException();
  }
}
