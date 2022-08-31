package lectures.part3concurrency

import java.util.concurrent.Executors

object Intro extends App {
  // Creation, Manipulation & Communication of JVM threads

  // JVM Threads
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Running in parallel")
  })

  // Creates a JVM thread which will run on top of an OS thread
  aThread.start() // gives the signal to the JVM to start a JVM thread

  //Blocks until a thread finishes running
  aThread.join()

  val threadHello = new Thread(() => (1 to 5).foreach(_ => println("hello")))
  val threadGoodbye = new Thread(() => (1 to 5).foreach(_ => println("goodbye")))

  threadHello.start()
  threadGoodbye.start()
  // Different runs within a multithreaded env produce different results

  // Executors
  val pool = Executors.newFixedThreadPool(10)
  pool.execute(() => println("Something in the thread pool"))

  pool.execute(() => {
    Thread.sleep(1000)
    println("done after 1 second")
  })

  pool.execute(() => {
    Thread.sleep(1000)
    println("almost done")
    Thread.sleep(1000)
    println("Done after 2 seconds")
  })

//  pool.shutdown()
//  //Following should throw an exception in the calling thread
//  pool.execute(() => println("Should not appear"))

  //Will interrupt current running threads
//pool.shutdownNow()

  // will return true after shutdown has been called even if there are still threads running
  println(pool.isShutdown)


}
