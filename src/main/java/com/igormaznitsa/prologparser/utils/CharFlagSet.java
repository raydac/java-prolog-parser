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

import java.util.Arrays;

/**
 * The class allows to keep packed boolean flags in memory, addressed by a char code point.
 * 
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 */
public final class CharFlagSet {
    /**
     * Inside array keeps packed boolean flags as integer values.
     */
    private final int [] charFlagMap = new int [0x800];
    
    /**
     * The Constructor.
     */
    public CharFlagSet(){
    }

    /**
     * Set the char in the set.
     * @param chr a char
     */
    public void addChar(final char chr){
        final int code = (int) chr;
        final int address = code >>> 5;
        final int pos = code & 0x1F;
        
        charFlagMap[address] |= (1<<pos);
    }

    /**
     * Check that a char is in the set.
     * @param chr a char to be checked.
     * @return true if the char is presented in the set, false otherwise.
     */
    public boolean containsChar(final char chr){
        final int code = (int) chr;
        final int address = code >>> 5;
        final int pos = code & 0x1F;
        
        return (charFlagMap[address] & (0x1<<pos)) != 0;
    }
    
    /**
     * Clear the set.
     */
    public void clear(){
        Arrays.fill(charFlagMap, 0);
    }
}
