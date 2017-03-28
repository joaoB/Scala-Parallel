import scala.util.Random
  def mcCount(iter: Int) : Int = {
    val randomX = new Random
    val randomY = new Random
    (0 until iter) count { _ =>
      val x = randomX.nextDouble
      val y = randomY.nextDouble
      x*x + y*y < 1
    }
  }

  def time[R](message: String)(block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println(s"$message : Elapsed time: ${t1 - t0} ns")
    result
  }

  def carloRec(iter : Int) : Double = time ("Parallel") {
    val nThreads = 8
    val iterPerThread = iter / nThreads //for simplicity, assume iter % nThreads == 0
    val sums = (1 to nThreads).toList.par.map(_ => mcCount(iterPerThread)).sum
    4.0 * sums / iter
  }

  def carloSeq(iter : Int) : Double = time ("Sequential") {
    val sums = mcCount(iter)
    4.0 * sums / iter
  }
