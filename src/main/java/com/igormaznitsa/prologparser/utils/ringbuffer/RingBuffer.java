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
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @param <T> the type of an item kept by the buffer
 */
public class RingBuffer <T extends RingBufferItem> {
    /**
     * The factory allows to create new items.
     */
    private final RingBufferFactory<T> factory;
    /**
     * The inside ring buffer.
     */
    private final  RingBufferItem  [] ringBuffer;
    
    /**
     * The pointer to the first element in the buffer.
     */
    private int headPointer;
    
    /**
     * The pointer to the last element in the buffer.
     */
    private int tailPointer;
    
    /**
     * The size of the buffer.
     */
    private final int bufferSize;
    
    /**
     * The constructor.
     * @param factory the factory to create new items, it must not be null.
     * @param size the size of the buffer.
     */
    @SuppressWarnings("unchecked")
    public RingBuffer(final RingBufferFactory<T> factory, final int size){
        this.factory = factory;
        this.ringBuffer =  new RingBufferItem[size];
        this.bufferSize = size;
        this.headPointer = 0;
        this.tailPointer = 0;
    }
    
    /**
     * Get an item.
     * @return a cached item if it is detected in the cache or a new one.
     */
    @SuppressWarnings("unchecked")
    public T get(){
        T result;
        int pointer = headPointer;
        if (pointer == tailPointer){
            // create new one
            result = factory.makeNew();
            result.setRingBuffer(this);
        }else{
            result = (T)ringBuffer[pointer++];
            if (pointer==bufferSize){
                pointer = 0;
            }
            headPointer = pointer;
        }
        return result;
    }
    
    /**
     * Dispose an item, place it into the buffer if there is a free place, drop the item otherwise.
     * @param item an item to be disposed.
     */
    @SuppressWarnings("unchecked")
    public void dispose(final RingBufferItem item){
        int pointer = tailPointer+1;
        if (pointer == bufferSize){
            pointer = 0;
        }
        if (pointer != headPointer){
            final T ringitem = (T) item;
            ringitem.reset();
            ringBuffer[tailPointer] = ringitem;
            tailPointer = pointer;
        }
    }
}
