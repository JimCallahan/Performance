package perf.abs2

class VecNd private (val elems: Array[Double]) 
  extends VecOps[Double, VecNd]
  with Vector
{
  def dimens = elems.length

  def apply(idx: Int): Double = elems(idx)
  def update(idx: Int, e: Double): VecNd = {
    val nelems = Array[Double](dimens)
    for(i <- 0 until dimens) 
      nelems(i) = if(i == idx) e else elems(i)
    new VecNd(nelems)
  } 
  
  def magSq: Double = dot(this) 
  def mag: Double = scala.math.sqrt(magSq)
  def normalized: VecNd = this / mag
  
  def dot (that: VecNd): Double = compwise(this, _ * _).reduce(_ + _)

  def + (that: VecNd): VecNd = compwise(that, _ + _)
  def - (that: VecNd): VecNd = compwise(that, _ - _)
  def * (that: VecNd): VecNd = compwise(that, _ * _)
  def / (that: VecNd): VecNd = compwise(that, _ / _)
    
  def + (s: Double): VecNd = map(_ + s)
  def - (s: Double): VecNd = map(_ - s)
  def * (s: Double): VecNd = map(_ * s)
  def / (s: Double): VecNd = map(_ / s)
  
  def map (p: (Double) => Double): VecNd = new VecNd(elems.map(p))
  def reduce (p: (Double, Double) => Double): Double = elems.reduce(p)
  def compwise (that: VecNd, p: (Double, Double) => Double): VecNd = {
    if(dimens != that.dimens)
      throw new IllegalArgumentException(
        "The number of dimensions for both vectors must be identical!")
    val nelems = Array[Double](dimens)
    for(i <- 0 until dimens) 
      nelems(i) = p(elems(i), that.elems(i))
    new VecNd(nelems)
  }
  
  //override def toString() = "VecNd(%.2f, %.2f, %.2f)".format(x, y, z)
}

object VecNd 
{
  def apply(size: Int, s: Double): VecNd = new VecNd(Array.fill(size)(s))
  def apply(elems: Double*): VecNd = new VecNd(elems.toArray)
  def random(size: Int): VecNd = new VecNd(Array.fill(size)(scala.math.random))
  def randomUnit(size: Int): VecNd = {
    val v = random(size) - VecNd(size, 0.5)
    val ms = v.magSq
    if((ms < 0.25) && (ms > 0.05)) v / scala.math.sqrt(ms)
    else randomUnit(size)
  }
}
