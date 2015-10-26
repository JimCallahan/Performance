package perf.abs2

/** Base trait for all tuples. */
trait Tuple

/** A Tuple that represents a position in space. */
trait Position 
  extends Tuple

/** A Tuple that represents an arbitrary vector. */
trait Vector 
  extends Tuple

/** Base trait for all tuples. */
trait TupleAccess[@specialized(Double,Int) Elem, Repr]
  extends Tuple
{
  /** The component identified by the given index. */
  def apply(i: Int): Elem
    
  /** A copy of this vector in which the component identified by index has been replaced with the given value. */ 
  def update(i: Int, e: Elem): Repr  
}

/** A two dimensional tuple. */
trait Tuple2[@specialized(Double,Int) Elem, Repr] 
  extends TupleAccess[Elem, Repr]
{
  /** The X-component of this vector. */
  val x: Elem
  
  /** The Y-component of this vector. */
  val y: Elem
  
  
  /** A copy of this vector in which the X-component has been replaced with the given value. */ 
  def updateX(e: Elem): Repr
  
  /** A copy of this vector in which the Y-component has been replaced with the given value. */ 
  def updateY(e: Elem): Repr
}

/** A three dimensional tuple. */
trait Tuple3[@specialized(Double,Int) Elem, Repr] 
  extends TupleAccess[Elem, Repr]
{
  /** The X-component of this vector. */
  val x: Elem
  
  /** The Y-component of this vector. */
  val y: Elem
  
  /** The Z-component of this vector. */
  val z: Elem

  
  /** A copy of this vector in which the X-component has been replaced with the given value. */ 
  def updateX(e: Elem): Repr
  
  /** A copy of this vector in which the Y-component has been replaced with the given value. */ 
  def updateY(e: Elem): Repr
  
  /** A copy of this vector in which the Z-component has been replaced with the given value. */ 
  def updateZ(e: Elem): Repr
}

/** A four dimensional tuple. */
trait Tuple4[@specialized(Double,Int) Elem, Repr] 
  extends TupleAccess[Elem, Repr]
{
  /** The X-component of this vector. */
  val x: Elem
  
  /** The Y-component of this vector. */
  val y: Elem
  
  /** The Z-component of this vector. */
  val z: Elem
  
  /** The W-component of this vector. */
  val w: Elem
  
  
  /** A copy of this vector in which the X-component has been replaced with the given value. */ 
  def updateX(e: Elem): Repr
  
  /** A copy of this vector in which the Y-component has been replaced with the given value. */ 
  def updateY(e: Elem): Repr
  
  /** A copy of this vector in which the Z-component has been replaced with the given value. */ 
  def updateZ(e: Elem): Repr

  /** A copy of this vector in which the W-component has been replaced with the given value. */ 
  def updateW(e: Elem): Repr
}
