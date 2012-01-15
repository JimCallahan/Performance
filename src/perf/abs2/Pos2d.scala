package perf.abs2

class Pos2d private (val x: Double, val y: Double) 
  extends Tuple2[Double, Pos2d]
  with PosOps[Double, Pos2d, Vec2d]
  with Position
{  
  def apply(i: Int): Double = 
    i match { 
      case 0 => x
      case 1 => y
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }
  
  def update(i: Int, e: Double): Pos2d =
    i match { 
      case 0 => Pos2d(e, y)
      case 1 => Pos2d(x, e)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    } 
  
  def updateX(e: Double): Pos2d = Pos2d(e, y)
  def updateY(e: Double): Pos2d = Pos2d(x, e)
  
  def - (that: Pos2d): Vec2d = Vec2d(x-that.x, y-that.y)
  
  def + (that: Vec2d): Pos2d = Pos2d(x+that.x, y+that.y)
  def - (that: Vec2d): Pos2d = Pos2d(x-that.x, y-that.y)
  def * (that: Vec2d): Pos2d = Pos2d(x*that.x, y*that.y)
  def / (that: Vec2d): Pos2d = Pos2d(x/that.x, y/that.y)
    
  def + (s: Double): Pos2d = Pos2d(x+s, y+s)
  def - (s: Double): Pos2d = Pos2d(x-s, y-s)
  def * (s: Double): Pos2d = Pos2d(x*s, y*s)
  def / (s: Double): Pos2d = Pos2d(x/s, y/s)
  
  def map (p: (Double) => Double): Pos2d = Pos2d(p(x), p(y))
  def reduce (p: (Double, Double) => Double): Double = p(x, y)
  def compwise (that: Pos2d, p: (Double, Double) => Double): Pos2d = 
    Pos2d(p(x, that.x), p(y, that.y))
    
  override def toString() = "Pos2d(%.2f, %.2f)".format(x, y)
}

object Pos2d 
{
  def apply(s: Double): Pos2d = new Pos2d(s, s)
  def apply(x: Double, y: Double): Pos2d = new Pos2d(x, y)
}
