package perf.abs2

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
      
  def + (that: Vec2d): Vec2d = Vec2d(x+that.x, y+that.y)
  def - (that: Vec2d): Vec2d = Vec2d(x-that.x, y-that.y)
  def * (that: Vec2d): Vec2d = Vec2d(x*that.x, y*that.y)
  def / (that: Vec2d): Vec2d = Vec2d(x/that.x, y/that.y)
    
  def + (s: Double): Vec2d = Vec2d(x+s, y+s)
  def - (s: Double): Vec2d = Vec2d(x-s, y-s)
  def * (s: Double): Vec2d = Vec2d(x*s, y*s)
  def / (s: Double): Vec2d = Vec2d(x/s, y/s)
  
  def map (p: (Double) => Double): Vec2d = Vec2d(p(x), p(y))
  def reduce (p: (Double, Double) => Double): Double = p(x, y)
  def compwise (that: Vec2d, p: (Double, Double) => Double): Vec2d = 
    Vec2d(p(x, that.x), p(y, that.y))
    
  override def toString() = "Vec2d(%.2f, %.2f)".format(x, y)
}

object Vec2d 
{
  def apply(s: Double): Vec2d = new Vec2d(s, s)
  def apply(x: Double, y: Double): Vec2d = new Vec2d(x, y)
  def random: Vec2d = new Vec2d(scala.math.random, scala.math.random)
  def randomUnit: Vec2d = {
    val v = random - Vec2d(0.5)
    val ms = v.magSq
    if((ms < 0.25) && (ms > 0.05)) v / scala.math.sqrt(ms)
    else randomUnit
  }
}
