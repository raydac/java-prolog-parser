package com.igormaznitsa.prologparser.utils.ringbuffer;

public interface SoftCacheItem {

  void setCache(SoftCache<? extends SoftCacheItem> parentCache);

  void reset();

  void release();
}
