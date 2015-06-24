/*
 * Copyright 2015 Igor Maznitsa.
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
package com.igormaznitsa.prologparser;

import com.igormaznitsa.prologparser.annotations.PrologOperator;
import com.igormaznitsa.prologparser.annotations.PrologOperators;
import com.igormaznitsa.prologparser.operators.OperatorType;

@PrologOperators(Operators = {
  @PrologOperator(Priority = 1200, Type = OperatorType.XFX, Name = ":-"),
  @PrologOperator(Priority = 1200, Type = OperatorType.XFX, Name = "-->"),
  @PrologOperator(Priority = 1200, Type = OperatorType.FX, Name = "?-"),
  @PrologOperator(Priority = 1200, Type = OperatorType.FX, Name = ":-"),
  @PrologOperator(Priority = 1100, Type = OperatorType.XFY, Name = ";"),
  @PrologOperator(Priority = 1050, Type = OperatorType.XFY, Name = "->"),
  @PrologOperator(Priority = 900, Type = OperatorType.FY, Name = "\\+"),
  @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "="),
  @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "\\="),
  @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "=="),
  @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "\\=="),
  @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "@<"),
  @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "@=<"),
  @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "@>"),
  @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "@>="),
  @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "=.."),
  @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "is"),
  @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "=:="),
  @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "=\\="),
  @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "<"),
  @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = "=<"),
  @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = ">"),
  @PrologOperator(Priority = 700, Type = OperatorType.XFX, Name = ">="),
  @PrologOperator(Priority = 500, Type = OperatorType.YFX, Name = "+"),
  @PrologOperator(Priority = 500, Type = OperatorType.YFX, Name = "-"),
  @PrologOperator(Priority = 500, Type = OperatorType.YFX, Name = "/\\"),
  @PrologOperator(Priority = 500, Type = OperatorType.YFX, Name = "\\/"),
  @PrologOperator(Priority = 400, Type = OperatorType.YFX, Name = "*"),
  @PrologOperator(Priority = 400, Type = OperatorType.YFX, Name = "/"),
  @PrologOperator(Priority = 400, Type = OperatorType.YFX, Name = "//"),
  @PrologOperator(Priority = 400, Type = OperatorType.YFX, Name = "<<"),
  @PrologOperator(Priority = 400, Type = OperatorType.YFX, Name = ">>"),
  @PrologOperator(Priority = 400, Type = OperatorType.YFX, Name = "rem"),
  @PrologOperator(Priority = 400, Type = OperatorType.YFX, Name = "mod"),
  @PrologOperator(Priority = 200, Type = OperatorType.XFX, Name = "**"),
  @PrologOperator(Priority = 200, Type = OperatorType.XFY, Name = "^"),
  @PrologOperator(Priority = 200, Type = OperatorType.FY, Name = "-"),
  @PrologOperator(Priority = 200, Type = OperatorType.FY, Name = "\\")})
public final class PrologParser extends AbstractPrologParser {

  static {
    readSystemOperators(PrologParser.class);
  }
  
  public PrologParser(final ParserContext context){
    super(context);
  }
}
