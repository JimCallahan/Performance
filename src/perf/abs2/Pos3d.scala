package perf.abs2

class Pos3d private (val x: Double, val y: Double, val z: Double) 
  extends Tuple3[Double, Pos3d]
  with PosOps[Double, Pos3d, Vec3d]
  with Position
{  
  def apply(i: Int): Double = 
    i match { 
      case 0 => x
      case 1 => y
      case 2 => z
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }
  
  def update(i: Int, e: Double): Pos3d =
    i match { 
      case 0 => Pos3d(e, y, z)
      case 1 => Pos3d(x, e, z)
      case 2 => Pos3d(x, y, e)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    } 
  
  def updateX(e: Double): Pos3d = Pos3d(e, y, z)
  def updateY(e: Double): Pos3d = Pos3d(x, e, z)
  def updateZ(e: Double): Pos3d = Pos3d(x, y, e)
  
  def - (that: Pos3d): Vec3d = Vec3d(x-that.x, y-that.y, z-that.z)
  
  def + (that: Vec3d): Pos3d = Pos3d(x+that.x, y+that.y, z+that.z)
  def - (that: Vec3d): Pos3d = Pos3d(x-that.x, y-that.y, z-that.z)
  def * (that: Vec3d): Pos3d = Pos3d(x*that.x, y*that.y, z*that.z)
  def / (that: Vec3d): Pos3d = Pos3d(x/that.x, y/that.y, z/that.z)
    
  def + (s: Double): Pos3d = Pos3d(x+s, y+s, z+s)
  def - (s: Double): Pos3d = Pos3d(x-s, y-s, z-s)
  def * (s: Double): Pos3d = Pos3d(x*s, y*s, z*s)
  def / (s: Double): Pos3d = Pos3d(x/s, y/s, z/s)
  
  def map (p: (Double) => Double): Pos3d = Pos3d(p(x), p(y), p(z))
  def reduce (p: (Double, Double) => Double): Double = p(x, p(y, z))
  def compwise (that: Pos3d, p: (Double, Double) => Double): Pos3d = 
    Pos3d(p(x, that.x), p(y, that.y), p(z, that.z))
    
  override def toString() = "Pos3d(%.2f, %.2f, %.2f)".format(x, y, z)
}

object Pos3d 
{
  def apply(s: Double): Pos3d = new Pos3d(s, s, s)
  def apply(x: Double, y: Double, z: Double): Pos3d = new Pos3d(x, y, z)
}
