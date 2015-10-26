package perf.concrete

class Vec3d private (val x: Double, val y: Double, val z: Double) 
{
  def fromComps(x: Double, y: Double, z:Double): Vec3d = new Vec3d(x, y, z)
  def fromScalar(s: Double): Vec3d = new Vec3d(s, s, s)
  
  def copy(that: Vec3d): Vec3d = new Vec3d(that.x, that.y, that.z)
  def copy: Vec3d = new Vec3d(x, y, z)
  
  def + (that: Vec3d): Vec3d = new Vec3d(x+that.x, y+that.y, z+that.z)
  def - (that: Vec3d): Vec3d = new Vec3d(x-that.x, y-that.y, z-that.z)
  def * (that: Vec3d): Vec3d = new Vec3d(x*that.x, y*that.y, z*that.z)
  def / (that: Vec3d): Vec3d = new Vec3d(x/that.x, y/that.y, z/that.z)
  
  def + (s: Double): Vec3d = new Vec3d(x+s, y+s, z+s)
  def - (s: Double): Vec3d = new Vec3d(x-s, y-s, z-s)
  def * (s: Double): Vec3d = new Vec3d(x*s, y*s, z*s)
  def / (s: Double): Vec3d = new Vec3d(x/s, y/s, z/s)
  
  def magSq: Double = dot(this)
  def mag: Double = scala.math.sqrt(magSq)
  def normalized: Vec3d = this / mag
  
  def dot (that: Vec3d): Double = x*that.x + y*that.y + z+that.z
  def cross(that: Vec3d): Vec3d = 
    new Vec3d(y*that.z - z*that.y, z*that.x - x*that.z,	x*that.y - y*that.x)	
    
  def map (p: (Double) => Double): Vec3d = new Vec3d(p(x), p(y), p(z))
  def reduce (p: (Double, Double) => Double): Double = p(x, p(y, z))
  def compwise (that: Vec3d, p: (Double, Double) => Double): Vec3d = 
    new Vec3d(p(x, that.x), p(y, that.y), p(z, that.z))
  
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
