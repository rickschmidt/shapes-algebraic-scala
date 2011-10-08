package edu.luc.cs.laufer.cs473.shapealgebra

/**
 * The category of shape algebras extended to support additional shapes.
 */
/**
 * Added import java.awt.Color
 */
import java.awt.Color

trait ExtendedShapeAlgebra[R] extends ShapeAlgebra[R] {

  def visitPolygon(rs:Seq[R],p: Polygon): R
  
  def visitCircle(c:Circle): R
  def visitPoint(p:Point):R
  def visitFill(r:R,f:Fill):R
  def visitOutline(s:Shape):R
  def visitRotate(int:Int,s:Shape):R
  def visitStroke(r:R, s:Stroke):R



  // TODO: add missing visit methods similarly to Location

  /**
   * The extended catamorphism for shapes.
   */
  override def fold(s: Shape): R = s match {
    case p: Polygon => {
      println("p fold: "+p)
      println("visitp in fold ")
      visitPolygon(p.points.map(fold(_)), p)
    }
    // TODO: add missing cases similarly to Location
    case c: Circle=>visitCircle(c)
    case pt: Point=>{
      println("fold point")
      println(pt)
      visitPoint(pt)
    }
    case f: Fill=>{
      println("fold Fill")
      visitFill(fold(f.shape),f)
    }
    case o: Outline=>visitOutline(o)
    case r:Rotate=>visitRotate(r.theta,r)
    case st:Stroke=>{
      println("fold stroke")
      println("fold stroke shape "+st.shape)
      visitStroke(fold(st.shape),st)
    }
    
//    case int:Int r: Rotate=>visitRotate(int, r)
//    case l: Location => visitLocation(fold(l.shape), l)
    
    case _ => super.fold(s)
  }
}
