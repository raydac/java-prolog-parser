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
package com.igormaznitsa.prologparser.annotations;

import com.igormaznitsa.prologparser.PrologParser;
import com.igormaznitsa.prologparser.operators.OperatorType;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The annotation being used to define a prolog operator. It allows to define an
 * operator in the shortest way.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @see PrologParser
 */
@Target(value = ElementType.TYPE)
@Retention(value = RetentionPolicy.CLASS)
public @interface PrologOperator {

    /**
     * The operator priority (0..1200)
     * 
     * @return the priority as integer
     */
    int Priority();

    /**
     * The operator type
     * 
     * @return the operator type
     */
    OperatorType Type();

    /**
     * The operator name
     * 
     * @return the operator name as a String
     */
    String Name();
}
