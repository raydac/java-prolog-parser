/*
 * Copyright 2019 Igor Maznitsa.
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

import com.igormaznitsa.prologparser.terms.PrologInt;
import com.igormaznitsa.prologparser.terms.PrologList;
import com.igormaznitsa.prologparser.terms.PrologStruct;
import com.igormaznitsa.prologparser.tokenizer.Op;
import java.io.IOException;
import org.junit.jupiter.api.Test;

import java.io.InputStreamReader;

import static com.igormaznitsa.prologparser.ParserContext.FLAG_NONE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

class BigDataTest extends AbstractIntegrationTest {

  @Test
  public void testBigSource_ClausesSplitted() {
    final int CLAUSES = 1000;
    assertEquals(CLAUSES, new GenericPrologParser(new InputStreamReader(new PrologSourceKoi7Generator(CLAUSES, true)), DefaultParserContext.of(FLAG_NONE, Op.SWI)).stream().count());
  }

  @Test
  public void testBigSource_ClausesNotSplitted() {
    final int CLAUSES = 1000;
    assertEquals(CLAUSES, new GenericPrologParser(new InputStreamReader(new PrologSourceKoi7Generator(CLAUSES, false)), DefaultParserContext.of(FLAG_NONE, Op.SWI)).stream().count());
  }


  @Test
  void testVeryLongStructure() {
    final int ELEMENTS = 100_000;

    final StringBuilder buffer = new StringBuilder(ELEMENTS);
    buffer.append("test(");
    boolean nonfirst = false;
    for (int i = 0; i < ELEMENTS; i++) {
      if (nonfirst) {
        buffer.append(',');
      } else {
        nonfirst = true;
      }
      buffer.append(i - 100);
    }
    buffer.append(").");

    try(final PrologParser parser = parseEd(buffer.toString())) {
      PrologStruct struct = (PrologStruct) parser.next();
      assertEquals(ELEMENTS, struct.getArity());
      assertEquals("test", struct.getFunctor().getText());
      for (int i = 0; i < ELEMENTS; i++) {
        assertEquals(i - 100, ((PrologInt) struct.getTermAt(i)).getNumber().intValue());
      }
    } catch (IOException ex) {
      fail(ex);
    }
  }

  @Test
  void testVeryLongList() {
    final int ELEMENTS = 100_000;

    final StringBuilder buffer = new StringBuilder(ELEMENTS);

    buffer.append('[');
    boolean nonFirst = false;

    for (int i = 0; i < ELEMENTS; i++) {
      if (nonFirst) {
        buffer.append(',');
      } else {
        nonFirst = true;
      }
      buffer.append(i);
    }
    buffer.append("].");

    try (final PrologParser parser = parseEd(buffer.toString())) {
      PrologList list = (PrologList) parser.next();

      for (int i = 0; i < ELEMENTS; i++) {
        final PrologInt head = (PrologInt) list.getHead();
        assertEquals(i, head.getNumber().intValue());
        list = (PrologList) list.getTail();
      }

      assertTrue(list.isEmpty());
    } catch (IOException ex) {
      fail(ex);
    }
  }

}
