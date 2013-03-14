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
package com.igormaznitsa.prologparser.utils.ringbuffer;

/**
 * The class implements a ring buffer allows to cache some items. But also the buffer creates new items if there is not any free one for a request.
 * It is soft one because it can lost items if it is full and generate new ones if it is empty.
 * NB! It is not a thread-safe class
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @param <T> the type of an item kept by the buffer
 */
public class SoftCache <T extends SoftCacheItem> {
    /**
     * The factory allows to create new items.
     */
    private final SoftCacheItemFactory<T> factory;
    /**
     * The inside buffer.
     */
    private final  SoftCacheItem  [] buffer;
    
    /**
     * The pointer to the first element in the buffer.
     */
    private int headPointer;
    
    /**
     * The max element index in the buffer.
     */
    private final int maxElementIndex;
    
    /**
     * The constructor.
     * @param factory the factory to create new items, it must not be null.
     * @param size the size of the buffer.
     */
    @SuppressWarnings("unchecked")
    public SoftCache(final SoftCacheItemFactory<T> factory, final int size){
        this.factory = factory;
        this.buffer =  new SoftCacheItem[size];
        this.maxElementIndex = size;
        this.headPointer = 0;
    }
    
    /**
     * Get an item.
     * @return a cached item if it is detected in the cache or a new one.
     */
    @SuppressWarnings("unchecked")
    public T get(){
        T result;
        int pointer = headPointer-1;
        if (pointer < 0) {
          // create new one
          result = factory.makeNew();
          result.setSoftCache(this);
        }
        else {
          result = (T) buffer[pointer];
          headPointer = pointer;
        }
        return result;
    }
    
    /**
     * Dispose an item, place it into the buffer if there is a free place, drop the item otherwise.
     * @param item an item to be disposed.
     */
    @SuppressWarnings("unchecked")
    public void dispose(final SoftCacheItem item){
        int pointer = headPointer;
        if (pointer < maxElementIndex) {
          final T ringitem = (T) item;
          ringitem.reset();
          buffer[pointer++] = ringitem;
          headPointer = pointer;
        }
    }
}
