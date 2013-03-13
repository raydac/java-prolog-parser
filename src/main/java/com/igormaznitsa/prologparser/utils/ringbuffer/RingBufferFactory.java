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
package com.igormaznitsa.prologparser.utils.ringbuffer;

/**
 * The interface describes a factory to produce items for the RingBuffer
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @param <T> the object type produced by the factory.
 * @see RingBuffer
 */
public interface RingBufferFactory <T extends RingBufferItem> {
    /**
     * To create new item.
     * @return the new created item.
     */
    T makeNew();
}
