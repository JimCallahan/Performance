package perf.abs

import perf.BenchUtil
import java.io.FileOutputStream
import java.util.Date

object Benchmark
  extends BenchUtil {
  /** The top-level entry method. */
  def main(args: Array[String]) {
    try {
      args.length match {
        case 0 =>
        case 1 => Console.setOut(new FileOutputStream(args(0)))
        case _ => throw new IllegalArgumentException("usage: Benchmark [logfile]")
      }

      println("Benchmark of Abstract Vectors.")
      println(new Date())
      println

      print("Generating Test Data: ")
      val vecA = Array.fill(size)(Vec3d.random)
      val vecB = Array.fill(size)(Vec3d.random)
      val vecC = Array.fill(size)(Vec3d.random)
      println(" Done.")

      timeTest("Warmup", iters / 2, size * 3) {
        var result = Vec3d(0.0)
        for (i <- 0 until size)
          result = (vecA(i) cross vecB(i)) * (vecB(i) dot vecC(i)) - (vecA(i) / vecB(i))
        result.magSq
      }

      val add = timeTest("Add", iters, size * 3) {
        var result = Vec3d(0.0)
        for (i <- 0 until size)
          result = result + vecA(i) + vecB(i) + vecC(i)
        result.magSq
      }

      val subtract = timeTest("Subtract", iters, size * 3) {
        var result = Vec3d(0.0)
        for (i <- 0 until size)
          result = result - vecA(i) - vecB(i) - vecC(i)
        result.magSq
      }

      val multiply = timeTest("Multiply", iters, size * 3) {
        var result = Vec3d(1.0)
        for (i <- 0 until size)
          result = result * vecA(i) * vecB(i) * vecC(i)
        result.magSq
      }

      val divide = timeTest("Divide", iters, size * 3) {
        var result = Vec3d(1.0)
        for (i <- 0 until size)
          result = result / vecA(i) / vecB(i) / vecC(i)
        result.magSq
      }

      val dot = timeTest("Dot", iters, size * 3) {
        var result = 0.0
        for (i <- 0 until size)
          result = result + (vecA(i) dot vecB(i)) + (vecB(i) dot vecC(i)) + (vecC(i) dot vecA(i))
        result
      }

      val cross = timeTest("Cross", iters, size * 3) {
        var result = Vec3d(1.0)
        for (i <- 0 until size)
          result = result cross vecA(i) cross vecB(i) cross vecC(i)
        result.magSq
      }

      val mag = timeTest("Mag", iters, size * 2) {
        var result = 0.0
        for (i <- 0 until size)
          result = vecA(i).mag + vecB(i).mag + vecC(i).mag
        result
      }

      println("----------------------------------------------")
      add.summary("     Add")
      subtract.summary(" Subract")
      multiply.summary("Multiply")
      divide.summary("  Divide")
      dot.summary("     Dot")
      cross.summary("   Cross")
      mag.summary("     Mag")
      println

      (add + subtract + multiply + divide + dot + cross + mag).summary("   TOTAL")

      sys.exit
    } 
    catch {
      case ex: IllegalArgumentException =>
        println(ex.getMessage)
      case ex =>
        println("Uncaught Exception: " + ex.getMessage + "\n" +
          "Stack Trace:\n" + ex.getStackTraceString)
    }
  }
}