package perf.abs

trait Tuple3[Elem, Repr]
{
  val x: Elem
  val y: Elem
  val z: Elem
  
  def fromScalar(s: Elem): Repr = fromComps(s, s, s)
  def fromComps(x: Elem, y: Elem, z:Elem): Repr
  def copy(that: Repr): Repr
}

trait TupleOps3[Elem, Repr <: Tuple3[Elem, Repr]]
  extends Tuple3[Elem, Repr]
{  
  def + (that: Repr): Repr
  def - (that: Repr): Repr
  def * (that: Repr): Repr
  def / (that: Repr): Repr
  
  def + (s: Elem): Repr
  def - (s: Elem): Repr
  def * (s: Elem): Repr
  def / (s: Elem): Repr
  
  def magSq: Elem
  def mag: Elem
  def normalized: Repr
  
  def dot (that: Repr): Elem
  def cross(that: Repr): Repr
  
  def map (that: Repr, p: (Elem) => Elem): Repr
  def reduce (p: (Elem, Elem) => Elem): Elem
  def compwise (that: Repr, p: (Elem, Elem) => Elem): Repr
}

class Vec3d private (val x: Double, val y: Double, val z: Double) 
  extends TupleOps3[Double, Vec3d]
{
  def fromComps(x: Double, y: Double, z:Double): Vec3d = new Vec3d(x, y, z)
  
  def copy(that: Vec3d): Vec3d = new Vec3d(that.x, that.y, that.z)
  def copy: Vec3d = new Vec3d(x, y, z)
  
  def magSq: Double = dot(this) 
  def mag: Double = scala.math.sqrt(magSq)
  def normalized: Vec3d = this / mag
  
  def dot (that: Vec3d): Double = x*that.x + y*that.y + z+that.z
  def cross(that: Vec3d): Vec3d = 
    fromComps(y*that.z - z*that.y, z*that.x - x*that.z,	x*that.y - y*that.x)	
    
  def + (that: Vec3d): Vec3d = fromComps(x+that.x, y+that.y, z+that.z)
  def - (that: Vec3d): Vec3d = fromComps(x-that.x, y-that.y, z-that.z)
  def * (that: Vec3d): Vec3d = fromComps(x*that.x, y*that.y, z*that.z)
  def / (that: Vec3d): Vec3d = fromComps(x/that.x, y/that.y, z/that.z)
    
  def + (s: Double): Vec3d = fromComps(x+s, y+s, z+s)
  def - (s: Double): Vec3d = fromComps(x-s, y-s, z-s)
  def * (s: Double): Vec3d = fromComps(x*s, y*s, z*s)
  def / (s: Double): Vec3d = fromComps(x/s, y/s, z/s)
  
  def map (that: Vec3d, p: (Double) => Double): Vec3d = fromComps(p(x), p(y), p(z))
  def reduce (p: (Double, Double) => Double): Double = p(x, p(y, z))
  def compwise (that: Vec3d, p: (Double, Double) => Double): Vec3d = 
    fromComps(p(x, that.x), p(y, that.y), p(z, that.z))
    
  override def toString() = "Vec3d(%.2f, %.2f, %.2f)".format(x, y, z)
}

object Vec3d 
{
  def apply(s: Double): Vec3d = new Vec3d(s, s, s)
  def apply(x: Double, y: Double, z:Double): Vec3d = new Vec3d(x, y, z)
  def random: Vec3d = new Vec3d(scala.math.random, scala.math.random, scala.math.random)
  def randomUnit: Vec3d = {
    val v = random - Vec3d(0.5)
    val ms = v.magSq
    if((ms < 0.25) && (ms > 0.05)) v / scala.math.sqrt(ms)
    else randomUnit
  }
}
