package edu.luc.cs.laufer.cs473.shapealgebra

/**
 * The category of shape algebras extended to support additional shapes.
 */


trait ExtendedShapeAlgebra[R] extends ShapeAlgebra[R] {

  def visitPolygon(rs:Seq[R],p: Polygon): R
  def visitCircle(c:Circle): R
  def visitPoint(p:Point):R
  def visitFill(r:R,f:Fill):R
  def visitOutline(r:R,o:Outline):R
  def visitRotate(r:R,s:Shape):R
  def visitStroke(r:R, s:Stroke):R

  /**
   * The extended catamorphism for shapes.
   */
  override def fold(s: Shape): R = s match {
    
    
    // TODO: add missing cases similarly to Location
    case p: Polygon =>visitPolygon(p.points.map(fold(_)), p)
    case c: Circle=>visitCircle(c)
    case pt: Point=>visitPoint(pt)   
    case f: Fill=>visitFill(fold(f.shape),f)    
    case o: Outline=>visitOutline(fold(o.shape),o)
    case r:Rotate=>visitRotate(fold(r.shape),r)
    case st:Stroke=>visitStroke(fold(st.shape),st)
 
    
    case _ => super.fold(s)
  }
}
