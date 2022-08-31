package lectures.part3async

import scala.collection.mutable
import scala.util.Random
object ThreadCommunication extends App {
  /*
    the producer-consumer problem

    producer -> [ x ] -> consumer
    // Problem: forcing threads to run actions in a guaranteed certain order

   */

  class SimpleContainer {
    private var value: Int = 0
    def isEmpty: Boolean = value == 0
    def set(newValue: Int) = value = newValue
    def get: Int = {
      val result = value
      value = 0
      result
    }
  }

  def naiveProdCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      while(container.isEmpty) {
        println("[consumer] actively waiting...")
      }
      println("[consumer] I have consumed " + container.get)
    })

    val producer = new Thread(() => {
      println("[producer] computing...")
      Thread.sleep(500)
      val value = 42
      println("[producer] I have produced, after long work the value " + value)
    })

    consumer.start()
    producer.start()
  }
  //naiveProdCons()

  // wait and notify

  //Synchronized
  // Entering a synchronized expression on an object locks the object:
  // Monitor: is a data structure internally used by JVM to keep track of which object is locked by which thread
//  val someObject = "hello"
//  someObject.synchronized { // lock the object's monitor
//    // code // Any other thread trying to run this will block
//  }

  // Only AnyRefs can have synchronized blocks
  // General principles:
  // - make no assumptions about who gets the lock first
  // - keep locking to a minimum
  // - maintain thread safety at ALL times in parallel applications

  //wait and notify()
  // wait() -ing on an object's monitor suspends you (the thread) indefinitely
  // Thread 1
//  val someObject1 = "hello"
//  someObject1.synchronized { // lock the object's monitor
//    // ... code part 1
//    someObject.wait() // release the lock and wait
//    // ... code part 2 // when allowed to proceed, lock the monitor again and continue
//  }
//
//  // Thread 2
//  someObject1.synchronized{ // lock the object's monitor
//    //... code
//    someObject1.notify() // signal ONE sleeping thread they may continue (which thread? You don't know) (use notifyAll to awaken ALL threads)
//    // ... more code
//  }

  // Waiting and notifying only work in synchronized expressions.


  //Smarted producer - Consumer

  def smartProdCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting ...")
      container.synchronized {
        container.wait()
      }

      // container must hace some value
      println("[consumer] I have consumed " + container.get)
    })

    val producer = new Thread(() => {
      println("[producer] Hard at work...")
      Thread.sleep(2000)
      val value = 42
      container.synchronized {
        println("[producer] I'm producing " + value)
        container.set(value)
        container.notify()
      }
    })
    consumer.start()
    producer.start()
  }

  // smartProdCons()

  /* Level 2: at the producer/consumer problem
    producer -> [? ? ?] -> consumer

    1 problem: producer and consumer may block each other

   */

  def prodConsLargeBuffer(): Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 3
    val consumer = new Thread(() => {
      val random = new Random()
      while (true) {
        buffer.synchronized {
          if (buffer.isEmpty) {
            println("[consumer] buffer empty, waiting...")
            buffer.wait()
          }
          // there must be at least ONE value in the buffer
          val x = buffer.dequeue()
          println(s"[consumer] consumed ${x}")

          // todo
          buffer.notify()
          Thread.sleep(random.nextInt(500))
        }
      }
    })

    val producer = new Thread(() => {
      val random = new Random()
      var i = 0
      while(true) {
        buffer.synchronized {
          if (buffer.size == capacity) {
            println("[producer] buffer is full, waiting...")
            buffer.wait()
          }
          // there emust be at least ONE EMPTY SPACE in the buffer
          println(s"[producer] producing ${i}")
          buffer.enqueue(i)
          // todo
          buffer.notify()
          i+=1
        }
      }
    })
    consumer.start()
    producer.start()
  }

  prodConsLargeBuffer()
}


