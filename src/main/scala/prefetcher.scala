//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Data Prefetcher
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2013 Mar 3

// Monitor the request stream (with miss information) to and from the cache
// (between the core and the data cache), and issue data prefetch requests to
// the cache via the front side (sharing the request port with the core).

package boom
{

import Chisel._
import config.Parameters

//*************************************************************
// IOs

// addresses being sent to the cache, delayed by two cycles so we can see if
// they missed or not
class CoreRequest(implicit p: Parameters) extends BoomBundle()(p)
{
   val addr = UInt(width = coreMaxAddrBits)
   val miss = Bool()           // was the access a miss in the cache?
   val secondary_miss = Bool() // was the access a secondary miss?
                               // (i.e., the cache is already servicing a miss
                               // to the same cache line
}

// this is our access port to the cache, where we put our prefetch requests into
class CacheReq(implicit p: Parameters) extends BoomBundle()(p)
{
   val addr = UInt(width = coreMaxAddrBits)
}

class CacheIO(implicit p: Parameters) extends BoomBundle()(p)
{
   val req = new DecoupledIO(new CacheReq())
}

//*************************************************************

class Prefetcher(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new Bundle
   {
      val core_requests = (new ValidIO(new CoreRequest)).flip
      val cache = new CacheIO
   })


   // ********** ENTER YOUR CODE HERE ************
   //
   // CURRENT BEHAVIOR:
   //  This example code listens to the core, and fetches the next cache-line if
   //  the core's request missed in the cache.
   //
   // IDEAS:
   //  You can imagine more interesting examples that look for strides of a
   //  given size, or spends multiple cycles spitting out multiple requests
   //  (say, fetching the next 8 lines after a miss...).
   //
   // GO CRAZY!
   //
   // - Chris
   //
   // P.S. This is seriously the best I have so far, so I'd love it if your
   // prefetcher did better than what I wrote below!



   // Put our requests onto a queue, to store our requests since we must give
   // right-of-way to the core's memory requests.
   val request_queue = Module(new Queue(gen=new CacheReq, entries=2))
   // A Queue's IO has two parts, enqueue (enq) and dequeue (deq).
   // Both ends implement the FIFOIO interface, which has three parts:
   //    - valid
   //    - ready
   //    - bits


   // set default value on output
   request_queue.io.enq.valid := Bool(false)

   // if we see a request and it was a miss, prefetch the next line
   // however, ignore it if it's a secondary miss (we've already sent out our
   // prefetch request the first time we saw a miss to this cache line).
   when (io.core_requests.valid && io.core_requests.bits.miss && !io.core_requests.bits.secondary_miss)
   {
      request_queue.io.enq.valid := Bool(true)
   }

   // fetch the next cache line
   request_queue.io.enq.bits.addr := io.core_requests.bits.addr + UInt(1 << p(uncore.tilelink.CacheBlockOffsetBits))


   // hook up our request to the outside world (notice the interfaces match)
   io.cache.req <> request_queue.io.deq

   // ********************************************
   // ********************************************

}

}
