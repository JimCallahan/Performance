package perf.abs2

trait PosOps[@specialized(Double,Int) Elem, Pos <: Position, Vec <: Vector]
  extends TupleOps[Elem, Pos, Vec]
{   
  def - (that: Pos): Vec
}