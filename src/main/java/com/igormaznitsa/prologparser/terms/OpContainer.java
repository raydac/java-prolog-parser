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

import com.igormaznitsa.prologparser.exceptions.CriticalUnexpectedError;
import com.igormaznitsa.prologparser.tokenizer.Op;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import com.igormaznitsa.prologparser.utils.StringBuilderEx;

import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Container of operators which have same name but differently associative.
 */
public final class OpContainer extends SpecServiceCompound {

  private static final long serialVersionUID = 4946799717661204529L;

  private Op opFZ;
  private Op opZF;
  private Op opZFZ;
  private int numberAtContainer;

  private OpContainer(final Op operator) {
    super(operator.getText());
    operator.streamOp().forEach(this::add);
  }

  private OpContainer(final String text, final Op fz, final Op zf, final Op zfz) {
    super(text);
    this.opFZ = fz;
    this.opZF = zf;
    this.opZFZ = zfz;
    this.numberAtContainer = (fz == null ? 0 : 1)
            + (zf == null ? 0 : 1)
            + (zfz == null ? 0 : 1);
  }
  
  /**
   * Create new container based on operator name.
   *
   * @param operator operator which name will be used and which will be added to the new container
   * @return generated container with the operator
   */
  public static OpContainer make(final Op operator) {
    return new OpContainer(operator);
  }

  public static OpContainer make(final String text, final Op fz, final Op zf, final Op zfz) {
    return new OpContainer(text, fz, zf, zfz);
  }
  
  /**
   * Add operator into the container.
   *
   * @param operator the operator to be added
   * @return true if the operator has been added, false if it was impossible because similar operator already presented
   * @throws IllegalArgumentException if operator has different name
   */
  public boolean add(final Op operator) {
    if (!getText().equals(operator.getText())) {
      throw new IllegalArgumentException("Illegal operator name, must be '" + getText() + "'");
    }

    switch (operator.getAssoc()) {
      case FX:
      case FY:
        if (this.opFZ != null) {
          return false;
        }
        this.opFZ = operator;
        this.numberAtContainer++;
        break;
      case XF:
      case YF:
        if (this.opZF != null) {
          return false;
        }
        this.opZF = operator;
        this.numberAtContainer++;
        break;
      case XFX:
      case XFY:
      case YFX:
        if (this.opZFZ != null) {
          return false;
        }
        this.opZFZ = operator;
        this.numberAtContainer++;
        break;
      default:
        throw new CriticalUnexpectedError();
    }
    return true;
  }

  /**
   * Remove all saved operators.
   */
  public void clear() {
    this.opFZ = null;
    this.opZF = null;
    this.opZFZ = null;
    this.numberAtContainer = 0;
  }

  /**
   * Find operator with arity in the container
   *
   * @param arity arity to be used (1..2)
   * @return found operator or null
   */
  public Op findForArity(final int arity) {
    Op result;
    switch (arity) {
      case 1: {
        if (this.opFZ != null) {
          result = this.opFZ;
        } else {
          result = this.opZF;
        }
      }
      break;
      case 2: {
        result = this.opZFZ;
      }
      break;
      default:
        result = null;
        break;
    }
    return result;
  }

  /**
   * Remove operator from the container
   *
   * @param op operator to be removed
   * @return true if operator was removed, false otherwise
   * @throws IllegalArgumentException if operator name is not compatible with the container
   */
  public boolean remove(final Op op) {
    if (!getText().equals(op.getText())) {
      throw new IllegalArgumentException(
          "Unexpected operator: " + op);
    }

    boolean result = false;

    if (op.equals(this.opFZ)) {
      this.opFZ = null;
      this.numberAtContainer--;
      result = true;
    } else if (op.equals(this.opZF)) {
      this.opZF = null;
      this.numberAtContainer--;
      result = true;
    } else if (op.equals(this.opZFZ)) {
      this.opZFZ = null;
      this.numberAtContainer--;
      result = true;
    }
    return result;
  }

  @Override
  public TermType getType() {
    return TermType.SPEC_TERM_OPERATOR_CONTAINER;
  }

  @Override
  public int getArity() {
    return 2;
  }

  @Override
  public PrologTerm getTermAt(int position) {
    throw new UnsupportedOperationException("Can't get element from operator container");
  }

  /**
   * Get current size of the container.
   *
   * @return size of the container
   */
  public int size() {
    return this.numberAtContainer;
  }

  /**
   * Get operator if it is only one in the container
   *
   * @return the found only operator, null if there are severe operators
   */
  public Op getIfSingle() {
    Op found = null;
    if (this.numberAtContainer == 1) {
      if (this.opZFZ != null) {
        found = this.opZFZ;
      } else if (this.opFZ != null) {
        found = this.opFZ;
      } else {
        found = this.opZF;
      }
    }
    return found;
  }

  /**
   * Find operator for presented arguments.
   *
   * @param hasLeftArg  should have left argument
   * @param hasRightArg should have right argument
   * @return operator for needed condition, null if not found
   */
  public Op findSimilar(final boolean hasLeftArg, final boolean hasRightArg) {
    final Op result;
    if (hasLeftArg) {
      if (hasRightArg) {
        if (this.opZFZ != null) {
          result = this.opZFZ;
        } else if (this.opFZ != null) {
          result = this.opFZ;
        } else {
          result = this.opZF;
        }
      } else {
        if (this.opZF != null) {
          result = this.opZF;
        } else {
          result = this.opFZ;
        }
      }
    } else {
      if (hasRightArg) {
        if (this.opFZ != null) {
          result = this.opFZ;
        } else {
          result = this.opZF;
        }
      } else {
        result = null;
      }
    }
    return result;
  }

  /**
   * Find operator for its type
   *
   * @param type type of needed operator
   * @return found operator, null if not found
   */
  public Op findForType(final OpAssoc type) {
    Op result = null;
    switch (type) {
      case FY:
      case FX:
        if (this.opFZ != null) {
          result = this.opFZ;
        }
        break;
      case XF:
      case YF:
        if (this.opZF != null) {
          result = this.opZF;
        }
        break;
      case XFX:
      case YFX:
      case XFY:
        if (this.opZFZ != null) {
          result = this.opZFZ;
        }
        break;
      default:
        throw new CriticalUnexpectedError();
    }

    return result == null || result.getAssoc() != type ? null : result;
  }

  /**
   * Find operator with similar type. For instance FX is similar to FY and XF is similar to YF.
   *
   * @param type type of operator to find
   * @return operator with similar type or null if not found
   */
  public Op findSimilar(final OpAssoc type) {
    Op result;
    switch (type) {
      case FX:
      case FY:
        result = this.opFZ;
        break;
      case XF:
      case YF:
        result = this.opZF;
        break;
      case XFX:
      case YFX:
      case XFY:
        result = this.opZFZ;
        break;
      default:
        throw new CriticalUnexpectedError();
    }
    return result;
  }

  /**
   * Remove operator for type
   *
   * @param type type of removing operator
   * @return true if removed, false if not found
   */
  public boolean removeForType(final OpAssoc type) {
    boolean result = false;
    switch (type) {
      case FX:
      case FY:
        if (this.opFZ != null && this.opFZ.getAssoc() == type) {
          this.opFZ = null;
          result = true;
        }
        break;
      case XF:
      case YF:
        if (this.opZF != null && this.opZF.getAssoc() == type) {
          this.opZF = null;
          result = true;
        }
        break;
      case XFX:
      case YFX:
      case XFY:
        if (this.opZFZ != null && this.opZFZ.getAssoc() == type) {
          opZFZ = null;
          result = true;
        }
        break;
      default:
        throw new CriticalUnexpectedError();
    }
    return result;
  }

  @Override
  public int getPrecedence() {
    return 0;
  }

  @Override
  public String toString() {
    final StringBuilderEx result = new StringBuilderEx("OpContainer ");
    result.append(Stream.of(this.opFZ, this.opZF, this.opZFZ)
        .filter(Objects::nonNull)
        .map(Op::toString)
        .collect(Collectors.joining(" ", "[", "]")));
    return result.toString();
  }
}
