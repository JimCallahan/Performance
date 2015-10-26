package perf

trait BenchUtil 
{ 
  /* Number of random vectors to generate. */
  val size = 100000
  
  /* Number of iterations of the test. */
  val iters = 10000
  
  /* Counters and timers for keeping performance statistics. */
  class Stats(val duration: Long, val ops: Long) 
  {
    val timeInSec = duration.toDouble / 1000.0
    val timePerOp = duration.toDouble / ops.toDouble
    val opsPerSec = (ops*1000L) / duration
    
    def report() {
      println("Total Time: %.2f sec".format(timeInSec))
      println(" Total Ops: " + ops)
      println(" Time / Op: %.8f ms".format(timePerOp))
      println(" Ops / Sec: " + opsPerSec)
    }
    
    def summary(title: String) {
      println("%s (MOps/Sec): %.2f".format(title, opsPerSec.toDouble / 1000000.0))
    }
    
    def + (that: Stats) = new Stats(duration+that.duration, ops+that.ops)
  }
  
  /* Run the test body repeatedly, timing the runs. */
  def timeTest(title: String, iters: Int, ops: Long)(test: => Double): Stats = {
  	println("----------------------------------------------")
    print("Starting " + title + " Test: ")
    val stamp = System.currentTimeMillis
    var result = 0.0
    for(_ <- 0 until iters) 
      result = result + test
    val duration = System.currentTimeMillis - stamp
    println(" Done (%.2f)".format(result))
    println

    val stats = new Stats(duration, iters.toLong * ops)
  	stats.report
  	stats
  }
}