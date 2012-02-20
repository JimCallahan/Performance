package perf.abs2

import java.nio.{ CharBuffer, ShortBuffer, IntBuffer, LongBuffer, FloatBuffer, DoubleBuffer }

class Vec2d private (val x: Double, val y: Double) 
  extends Tuple2[Double, Vec2d]
  with VecOps[Double, Vec2d]
  with Vector
{
  def apply(i: Int): Double = 
    i match { 
      case 0 => x
      case 1 => y
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }
  
  def update(i: Int, e: Double): Vec2d =
    i match { 
      case 0 => Vec2d(e, y)
      case 1 => Vec2d(x, e)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    } 
  
  def updateX(e: Double): Vec2d = Vec2d(e, y)
  def updateY(e: Double): Vec2d = Vec2d(x, e)
  
  def magSq: Double = dot(this) 
  def mag: Double = scala.math.sqrt(magSq)
  def normalized: Vec2d = this / mag
  
  def dot (that: Vec2d): Double = x*that.x + y*that.y
      
  def - : Vec2d = Vec2d(-x, -y)
  
  def + (that: Vec2d): Vec2d = Vec2d(x+that.x, y+that.y)
  def - (that: Vec2d): Vec2d = Vec2d(x-that.x, y-that.y)
  def * (that: Vec2d): Vec2d = Vec2d(x*that.x, y*that.y)
  def / (that: Vec2d): Vec2d = Vec2d(x/that.x, y/that.y)
    
  def + (s: Double): Vec2d = Vec2d(x+s, y+s)
  def - (s: Double): Vec2d = Vec2d(x-s, y-s)
  def * (s: Double): Vec2d = Vec2d(x*s, y*s)
  def / (s: Double): Vec2d = Vec2d(x/s, y/s)
  
  def forall(p: (Double) => Boolean): Boolean = p(x) && p(y)
  def forall(that: Vec2d)(p: (Double, Double) => Boolean): Boolean = p(x, that.x) && p(y, that.y)

  def forany(p: (Double) => Boolean): Boolean = p(x) || p(y)
  def forany(that: Vec2d)(p: (Double, Double) => Boolean): Boolean = p(x, that.x) || p(y, that.y)

  def foreach(p: (Double) => Unit): Unit = { p(x); p(y) }
  
  def map(p: (Double) => Double): Vec2d = Vec2d(p(x), p(y))

  def foldLeft[A](start: A)(f: (A, Double) => A): A = f(f(start, x), y)
  def /:[A](start: A)(f: (A, Double) => A): A = foldLeft(start)(f)

  def foldRight[A](start: A)(f: (Double, A) => A): A = f(x, f(y, start))
  def :\[A](start: A)(f: (Double, A) => A): A = foldRight(start)(f)

  def reduce(p: (Double, Double) => Double): Double = p(x, y)
  def min: Double = reduce(_ min _)
  def max: Double = reduce(_ max _)

  def compwise(that: Vec2d, p: (Double, Double) => Double): Vec2d =
    Vec2d(p(x, that.x), p(y, that.y))
  def min(that: Vec2d): Vec2d = compwise(that, _ min _)
  def max(that: Vec2d): Vec2d = compwise(that, _ max _)

  override def toString() = "Vec2d(%.2f, %.2f)".format(x, y)
  
  def toList: List[Double] = List(x, y)
  def toArray: Array[Double] = Array(x, y)

  //def toVector4d: Vector4d = Vector4d(x, y, 0.0, 1.0) 
  //def toVector3d: Vector3d = Vector3d(x, y, 0.0) 
  //def toVector2d: Vector2d = Vector2d(x, y) 

  def toVec3d: Vec3d = Vec3d(x, y, 0.0)
  def toVec2d: Vec2d = this

  def toPos3d: Pos3d = Pos3d(x, y, 0.0)
  def toPos2d: Pos2d = Pos2d(x, y)

  def toIndex3i: Index3i = Index3i(x.toInt, y.toInt, 0)
  def toIndex2i: Index2i = Index2i(x.toInt, y.toInt)
  
  def putNative(buf: CharBuffer) {
    buf.put(x.toChar); buf.put(y.toChar)
  }
  def >>>(buf: CharBuffer) { putNative(buf) }
  
  def putNative(buf: ShortBuffer) {
    buf.put(x.toShort); buf.put(y.toShort)
  }
  def >>>(buf: ShortBuffer) { putNative(buf) }
  
  def putNative(buf: IntBuffer) {
    buf.put(x.toInt); buf.put(y.toInt)
  }
  def >>>(buf: IntBuffer) { putNative(buf) }
  
  def putNative(buf: LongBuffer) {
    buf.put(x.toLong); buf.put(y.toLong)
  }
  def >>>(buf: LongBuffer) { putNative(buf) }
  
  def putNative(buf: FloatBuffer) {
    buf.put(x.toFloat); buf.put(y.toFloat)
  }
  def >>>(buf: FloatBuffer) { putNative(buf) }
  
  def putNative(buf: DoubleBuffer) {
    buf.put(x); buf.put(y)
  }
  def >>>(buf: DoubleBuffer) { putNative(buf) }
}

object Vec2d 
{
  def apply(s: Double): Vec2d = new Vec2d(s, s)
  def apply(x: Double, y: Double): Vec2d = new Vec2d(x, y)
  def random: Vec2d = new Vec2d(scala.math.random, scala.math.random)
  def randomUnit: Vec2d = {
    val v = random - Vec2d(0.5)
    val ms = v.magSq
    if((ms < 0.25) && (ms > 0.001)) v / scala.math.sqrt(ms)
    else randomUnit
  }
}
