package com.igormaznitsa.prologparser.utils;

import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

class SoftObjectPoolTest {

  @Test
  public void testGetPush() {
    final AtomicInteger counter = new AtomicInteger();

    final SoftObjectPool<Object> pool = new SoftObjectPool<Object>(64){
      @Override
      public Object get() {
        counter.incrementAndGet();
        return new Object();
      }
    };


    assertEquals(0, pool.size());
    assertEquals(0, counter.get());
    pool.fill();
    assertEquals(64, pool.size());
    assertEquals(64, counter.get());
    Object obj = pool.findCached();
    assertEquals(64, counter.get());
    assertEquals(63, pool.size());
    pool.push(obj);
    assertEquals(64, pool.size());
    assertEquals(64, counter.get());

    IntStream.range(0, 128).forEach(x -> assertNotNull(pool.findCached()));
    assertEquals(128, counter.get());

    assertEquals(0, pool.size());

    IntStream.range(0, 128).forEach(x -> pool.push(new Object()));
    assertEquals(128, counter.get());

    assertEquals(64, pool.size());
  }

}