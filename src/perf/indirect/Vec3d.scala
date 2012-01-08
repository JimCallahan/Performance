package perf.indirect

trait Tuple3[Elem, Repr]
{
  val x: Elem
  val y: Elem
  val z: Elem
  
  def fromScalar(s: Elem): Repr = fromComps(s, s, s)
  def fromComps(x: Elem, y: Elem, z:Elem): Repr
}

trait TupleOps3[Elem, Repr]
  extends Tuple3[Elem, Repr]
{  
  def + (that: Vec3d): Repr
  def - (that: Vec3d): Repr
  def * (that: Vec3d): Repr
  def / (that: Vec3d): Repr
  
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

trait TupleOps3d[Repr <: Tuple3[Double, Repr]] 
  extends TupleOps3[Double, Repr]
{
  def + (that: Repr): Repr = fromComps(x+that.x, y+that.y, z+that.z)
  def - (that: Repr): Repr = fromComps(x-that.x, y-that.y, z-that.z)
  def * (that: Repr): Repr = fromComps(x*that.x, y*that.y, z*that.z)
  def / (that: Repr): Repr = fromComps(x/that.x, y/that.y, z/that.z)
    
  def + (s: Double): Repr = fromComps(x+s, y+s, z+s)
  def - (s: Double): Repr = fromComps(x-s, y-s, z-s)
  def * (s: Double): Repr = fromComps(x*s, y*s, z*s)
  def / (s: Double): Repr = fromComps(x/s, y/s, z/s)
  
  def map (that: Repr, p: (Double) => Double): Repr = fromComps(p(x), p(y), p(z))
  def reduce (p: (Double, Double) => Double): Double = p(x, p(y, z))
  def compwise (that: Repr, p: (Double, Double) => Double): Repr = 
    fromComps(p(x, that.x), p(y, that.y), p(z, that.z))
}

trait VecOps3d[Repr <: Tuple3[Double, Repr]] 
  extends TupleOps3d[Repr]
{ 
  this: Repr =>    
  def magSq: Double = dot(this)
  def mag: Double = scala.math.sqrt(magSq)
  def normalized: Repr = this / fromComps(mag, mag, mag)
  
  def dot (that: Repr): Double = x*that.x + y*that.y + z+that.z
  def cross(that: Repr): Repr = 
    fromComps(y*that.z - z*that.y, z*that.x - x*that.z,	x*that.y - y*that.x)	
}

class Vec3d private (val x: Double, val y: Double, val z: Double) 
  extends VecOps3d[Vec3d]
{
  def fromComps(x: Double, y: Double, z:Double): Vec3d = new Vec3d(x, y, z)
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
