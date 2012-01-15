package perf.abs2

class Index2i private (val x: Int, val y: Int) 
  extends Tuple2[Int, Index2i]
  with TupleOps[Int, Index2i, Index2i]
{
  def apply(i: Int): Int = 
    i match { 
      case 0 => x
      case 1 => y
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }
  
  def update(i: Int, e: Int): Index2i =
    i match { 
      case 0 => Index2i(e, y)
      case 1 => Index2i(x, e)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    } 
  
  def updateX(e: Int): Index2i = Index2i(e, y)
  def updateY(e: Int): Index2i = Index2i(x, e)
  
  def dot (that: Index2i): Int = x*that.x + y*that.y
      
  def + (that: Index2i): Index2i = Index2i(x+that.x, y+that.y)
  def - (that: Index2i): Index2i = Index2i(x-that.x, y-that.y)
  def * (that: Index2i): Index2i = Index2i(x*that.x, y*that.y)
  def / (that: Index2i): Index2i = Index2i(x/that.x, y/that.y)
    
  def + (s: Int): Index2i = Index2i(x+s, y+s)
  def - (s: Int): Index2i = Index2i(x-s, y-s)
  def * (s: Int): Index2i = Index2i(x*s, y*s)
  def / (s: Int): Index2i = Index2i(x/s, y/s)
  
  def map (p: (Int) => Int): Index2i = Index2i(p(x), p(y))
  def reduce (p: (Int, Int) => Int): Int = p(x, y)
  def compwise (that: Index2i, p: (Int, Int) => Int): Index2i = 
    Index2i(p(x, that.x), p(y, that.y))
    
  override def toString() = "Index2i(%d, %d)".format(x, y)
}

object Index2i 
{
  def apply(s: Int): Index2i = new Index2i(s, s)
  def apply(x: Int, y: Int): Index2i = new Index2i(x, y)
}
