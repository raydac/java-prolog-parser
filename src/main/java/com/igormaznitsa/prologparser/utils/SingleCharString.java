/*
 * Copyright 2011-2013 Igor Maznitsa (http://www.igormaznitsa.com)
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

public final class SingleCharString {
    private static final int MAX_CODE = 0xFF;
    private static final String [] map = new String[MAX_CODE+1];
    static{
        for(int i=0;i<=MAX_CODE;i++){
            map[i] = String.valueOf((char)i);
        }
    }
    
    public static String valueOf(final char chr){
        return chr>MAX_CODE ? String.valueOf(chr) : map[chr];
    }
}
