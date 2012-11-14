/*
 * Copyright 2011-2012 Igor Maznitsa (http://www.igormaznitsa.com)
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of version 3 of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307  USA
 */
package com.igormaznitsa.prologparser.utils;

/**
 * It is an auxiliary class contains methods to check arguments
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public enum AssertionUtils {
    ;
        
    public static void checkNotNull(final String message, final Object obj) {
        if (obj == null){
            throw new NullPointerException(message);
        }
    }
    
    public static void checkArrayForNullElements(final String message, final Object [] array) {
        for(final Object obj : array){
            if (obj == null){
                throw new NullPointerException(message);
            }
        }
    }
    
}
