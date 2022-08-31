package lectures.part3async

object ConcurrencyProblems {
  def runInParallel(): Unit = {
    var x = 0
    val thread1 = new Thread(() => { // abstract method pattern
      x = 1
    })

    val thread2 = new Thread(() => {
      x = 2
    })

    thread1.start()
    thread2.start()
    println(x) // race condition
  }

  case class BankAccount (var amount: Int)

  def buy(bankAccount: BankAccount, thing: String, price: Int): Unit = {
    /*
      involves 3 steps:
        - read old value
        - compute result
        - write new value
     */
    bankAccount.amount -= price
  }

  /*
    Example race condition:
    thread1 (shoes)
      - reads amount 50000
      - compute result 50000 - 3000 = 47000
    thread2 (iPhone)
      - reads amount 50000
      - compute result 50000 - 4000 = 46000
    thread1 (shoes)
      - write amount 47000
    thread2 (iPhone)
      - write amount 46000

   */

  def buySafe(bankAccount: BankAccount, thing: String, price: Int): Unit = {
    bankAccount.synchronized { // does not allow multiple thread to run the critical section AT THE SAME TIME
      bankAccount.amount -= price // critical section
    }
  }

  def demoBankingProblem(): Unit = {
    (1 to 10000000).foreach {
      _ =>
        val account = BankAccount(50000)
        val thread1 = new Thread(() => buy(account, "shoes", 3000))
        val thread2 = new Thread(() => buy(account, "iPhone", 4000))
        thread1.start()
        thread2.start()
        thread1.join()
        thread2.join()
        if (account.amount != 43000) println(s"AHA! I've just broken the bank: ${account.amount}")
        //else println(s"Everything looks good here: ${account.amount}")
    }
  }

  /**
   * Exercises
   * 1 - create "inception threads"
   *  thread1
   *    -> thread 2
   *      -> thread 3
   *   each thread prints "hello from thread $i)
   *   print all messages  IN REVERSE ORDER
   *  2. what's the max/min value of x
   *  3 - "sleep fallacy"
   */

    // 1- inception threads
    def inceptionThreads(maxThreads: Int, i: Int = 1): Thread =
      new Thread(() => {
        if (i < maxThreads) {
          val newThread = inceptionThreads(maxThreads, i + 1)
          newThread.start()
          newThread.join()
        }
        println(s"Hello from thread $i")
      })
  // 2
  /*
      Max value of x = 100
      min value = 1
        all thread read x = 0 at the same time
        all threads (in parallel) compute 0 + 1 = 1
        all threads try to write x = 1
   */

  def minMaxX(): Unit = {
    var x = 0
    val threads = (1 to 100).map(_ => new Thread(() => x += 1))
  }

  // 3
  /*
      almost always message = "Scala is awesome"
      is it guaranteed? NO
      Obnoxious situation (possible):
        main thread:
        message = "Scala sucks"
        awesomeTrhead.start(
        sleep(1001) - yields execution
      awesome thread:
        sleep(1000) - yields execution
      OS gives the CPU to some important thread, takes > 2s
      OS gives the CPU back to main thread
      main thread:
        println(message) // Scala sucks
      awesome thread:
        message = "Scala is awesome"

   */
  def demoSleepFallacy(): Unit = {
      var message = ""
      val awesomeThread = new Thread(() => {
        Thread.sleep(1000)
        message = "Scala is awesome"
      })
      message = "Scala sucks"
      awesomeThread.start()
      Thread.sleep(1001)
      // Solution: join the worker thread
      awesomeThread.join()
      println(message)
    }


  def main(args: Array[String]): Unit = {
//    runInParallel()
//    demoBankingProblem()
    inceptionThreads(50).start()
  }
}
