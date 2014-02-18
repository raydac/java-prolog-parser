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
package com.igormaznitsa.prologparser.exceptions;

/**
 * The error will be throws if a critical work logic defect has been found
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class CriticalSoftwareDefectError extends Error {
    private static final long serialVersionUID = -8219655356191420973L;
    
    public CriticalSoftwareDefectError(){
        super("Critical software defect, contact developers please and check the new version of the software!");
    }
}
