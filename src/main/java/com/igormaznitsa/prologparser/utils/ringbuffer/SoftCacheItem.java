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
 * The interface describes an item to be saved in a SoftCache.
 * @author Igor Maznitsa (http://www.igormaznitsa.com)
 * @see SoftCache
 */
public interface SoftCacheItem {
    /**
     * Set the SoftCache owns the item.
     * @param softCache a SoftCache to be the owner, it must not be null.
     */
    void setSoftCache(SoftCache<? extends SoftCacheItem> softCache);
    /**
     * Reset data in the item.
     */
    void reset();
    /**
     * Dispose the item.
     */
    void dispose();
}
