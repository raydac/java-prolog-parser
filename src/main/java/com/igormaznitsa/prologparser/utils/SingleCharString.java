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
