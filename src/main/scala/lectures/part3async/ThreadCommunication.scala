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

  //prodConsLargeBuffer()

  /*
    Prod-cons, level 3
      producer1 -> [? ? ?] -> consumer1
      producer2 -> [? ? ?] -> consumer2
   */

  class Consumer(id: Int, buffer: mutable.Queue[Int]) extends Thread {
    override def run(): Unit = {
      val random = new Random()
      while (true) {
        buffer.synchronized {
          /*
           * Producer produces value, two Cons are waiting
           * Notifies ONE consumer, notifies on buffer
           * notifies the other consumer
           *
           */
          while (buffer.isEmpty) {
            println(s"[consumer $id] buffer empty, waiting...")
            buffer.wait()
          }
          // there must be at least ONE value in the buffer
          val x = buffer.dequeue()
          println(s"[consumer $id] consumed ${x}")

          // todo
          buffer.notifyAll()
          Thread.sleep(random.nextInt(500))
        }
      }
    }
  }

    class Producer(id: Int, buffer:mutable.Queue[Int], capacity: Int) extends Thread {
      val random = new Random()
      var i = 0
      while (true) {
        buffer.synchronized {
          while (buffer.size == capacity) {
            println("[producer] buffer is full, waiting...")
            buffer.wait()
          }
          // there must be at least ONE EMPTY SPACE in the buffer
          println(s"[producer] producing ${i}")
          buffer.enqueue(i)
          buffer.notifyAll()
          i += 1
        }
        Thread.sleep(random.nextInt(500))
      }
    }

  def multiProdCons(nConsumers: Int, nProducers: Int): Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 12

    (1 to nConsumers).foreach( i => new Consumer(i, buffer).start())
    (1 to nProducers).foreach( i => new Producer(i, buffer, capacity).start())
  }

  //multiProdCons(3,6)

  /*
      Exercises.
      1) think of an example where notifyALL acts in a different way than notify?
      2) create a deadlock
      3) create a livelock
   */

    //notifyAll
  def testNotifyAll(): Unit = {
    val bell = new Object
    (1 to 10).foreach(i => new Thread(() => {
      bell.synchronized {
        println(s"[Thread ${i}] waiting")
        bell.wait()
        println(s"[thread ${i}] hooray!")
      }
    }).start())

    new Thread(() => {
      Thread.sleep(2000)
      println("[announcer] Rock'n roll")
      bell.synchronized{
        bell.notifyAll()
      }
    }).start()
  }
  //testNotifyAll()

  // 2 - deadlock
  case class Friend(name: String) {
    def bow(other: Friend): Unit = {
      this.synchronized {
        println(s"$this: I am bowing to my friend $other")
        other.rise(this)
        println(s"$this: my friend $other has risen ")
      }
    }

    def rise(other: Friend): Unit = {
      this.synchronized{
        println(s"$this: I am rising to my friend other")
      }
    }
    var side = "right"
    def switchSide(): Unit = {
      if (side == "right") side = "left"
      else side = "right"
    }

    // 3 - livelock
    def pass(other: Friend): Unit = {
      while (this.side == other.side){
        println(s"$this: Oh, but please, $other, feel free to pass...")
        switchSide()
        Thread.sleep(1000)
      }
    }
  }

  val sam = Friend("Sam")
  val pierre = Friend("Pierre")
  new Thread(() => sam.pass(pierre)).start()
  new Thread(() => pierre.pass(sam)).start()


}


