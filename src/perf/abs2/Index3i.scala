package perf.abs2

class Index3i private (val x: Int, val y: Int, val z: Int) 
  extends Tuple3[Int, Index3i]
  with TupleOps[Int, Index3i, Index3i]
{
  def apply(i: Int): Int = 
    i match { 
      case 0 => x
      case 1 => y
      case 2 => z
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }
  
  def update(i: Int, e: Int): Index3i =
    i match { 
      case 0 => Index3i(e, y, z)
      case 1 => Index3i(x, e, z)
      case 2 => Index3i(x, y, e)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    } 
  
  def updateX(e: Int): Index3i = Index3i(e, y, z)
  def updateY(e: Int): Index3i = Index3i(x, e, z)
  def updateZ(e: Int): Index3i = Index3i(x, y, e)
  
  def dot (that: Index3i): Int = x*that.x + y*that.y + z+that.z
      
  def + (that: Index3i): Index3i = Index3i(x+that.x, y+that.y, z+that.z)
  def - (that: Index3i): Index3i = Index3i(x-that.x, y-that.y, z-that.z)
  def * (that: Index3i): Index3i = Index3i(x*that.x, y*that.y, z*that.z)
  def / (that: Index3i): Index3i = Index3i(x/that.x, y/that.y, z/that.z)
    
  def + (s: Int): Index3i = Index3i(x+s, y+s, z+s)
  def - (s: Int): Index3i = Index3i(x-s, y-s, z-s)
  def * (s: Int): Index3i = Index3i(x*s, y*s, z*s)
  def / (s: Int): Index3i = Index3i(x/s, y/s, z/s)
  
  def map (p: (Int) => Int): Index3i = Index3i(p(x), p(y), p(z))
  def reduce (p: (Int, Int) => Int): Int = p(x, p(y, z))
  def compwise (that: Index3i, p: (Int, Int) => Int): Index3i = 
    Index3i(p(x, that.x), p(y, that.y), p(z, that.z))
    
  override def toString() = "Index3i(%d, %d, %d)".format(x, y, z)
}

object Index3i 
{
  def apply(s: Int): Index3i = new Index3i(s, s, s)
  def apply(x: Int, y: Int, z: Int): Index3i = new Index3i(x, y, z)
}
