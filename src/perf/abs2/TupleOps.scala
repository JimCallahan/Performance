package perf.abs2

trait TupleOps[@specialized(Double,Int) Elem, Repr <: Tuple, Alt <: Tuple]
{  
  /** The component-wise addition of this vector with another vector. */ 
  def + (that: Alt): Repr
  
  /** The component-wise subtraction of another vector from this vector. */ 
  def - (that: Alt): Repr
  
  /** The component-wise multiplication of this vector with another vector. */ 
  def * (that: Alt): Repr
  
  /** The component-wise division of this vector by another vector. */ 
  def / (that: Alt): Repr
  
  /** The addition of a scalar value to all components of this vector. */
  def + (s: Elem): Repr
  
  /** The subtraction of a scalar value from all components of this vector. */ 
  def - (s: Elem): Repr
  
  /** The product of a scalar value with all components of this vector. */ 
  def * (s: Elem): Repr
  
  /** The quotient of dividing all components of this vector by a scalar value. */ 
  def / (s: Elem): Repr
  
  /** Builds a new vector by applying a function to each component of this vector. */
  def map (p: (Elem) => Elem): Repr
  
  
  def reduce (p: (Elem, Elem) => Elem): Elem

  
  def compwise (that: Repr, p: (Elem, Elem) => Elem): Repr
}

