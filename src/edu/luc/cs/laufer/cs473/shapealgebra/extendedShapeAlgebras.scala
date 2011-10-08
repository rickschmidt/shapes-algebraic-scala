package edu.luc.cs.laufer.cs473.shapealgebra


/**
 * Added import java.awt.Color
 */
import java.awt.Color

object ExtendedShapeSize extends ExtendedShapeAlgebra[Int] {
  // forward methods for original shapes at object level
  override def visitEllipse(e: Ellipse) = ShapeSize.visitEllipse(e)
  // TODO: methods for the other original shapes
  
  // new methods for extended shapes
  override def visitStroke(r:Int, shape: Stroke) = r
  // TODO: methods for the other additional (extended) shapes
  override def visitRotate(r:Int,s:Shape)=1
  override def visitOutline(s:Shape)=1
  override def visitFill(r:Int,f:Fill)={
    println("fill")
    r
  }
  override def visitPoint(p:Point)=0
  override def visitCircle(c:Circle)=1
  override def visitPolygon(ps:Seq[Int],p:Polygon)={
    
    println("visit polygon "+p)
    println("ps sum: "+ps.sum)
    
    1
  }
  override def visitLocation(r:Int, l:Location)=1
  override def visitGroup(rs:Seq[Int],g:Group)=1
  override def visitRectangle(r:Rectangle)=1
  
  
  
}

object ExtendedShapeDepth extends ExtendedShapeAlgebra[Int] {
  // TODO: all methods defined from scratch
  override def visitEllipse(e: Ellipse) = ShapeSize.visitEllipse(e)
  override def visitStroke(r:Int,s:Stroke) ={
    println("stroke")
    println(ExtendedShapeSize.visitStroke(r,s)+1)
    println(s.getClass())
    ExtendedShapeSize.visitStroke(r,s)+1
    
  }
  override def visitRotate(r:Int,s:Shape)=ExtendedShapeSize.visitRotate(r,s)+1
  override def visitOutline(s:Shape)=ExtendedShapeSize.visitOutline(s)+1
  override def visitFill(r:Int,f:Fill)={
    println("fill")
    println(ExtendedShapeSize.visitFill(r,f)+1)
    ExtendedShapeSize.visitFill(r,f)+1
  }
  override def visitPoint(p:Point)={
    
    ExtendedShapeSize.visitPoint(p)
  }
  override def visitCircle(c:Circle)=1
  override def visitPolygon(ps:Seq[Int],p:Polygon)={
    println("polygon")
    ExtendedShapeSize.visitPolygon(ps:Seq[Int],p:Polygon)
  }
  override def visitLocation(r:Int, l:Location)={
    println("loc")
    ShapeSize.visitLocation(fold(l.shape), l)+1}
  override def visitGroup(rs:Seq[Int],g:Group)=1
  override def visitRectangle(r:Rectangle)=1
  
}

//class ExtendedBoundingBox extends BoundingBox with ExtendedShapeAlgebra[Location] {
//  // methods for original shapes inherited at class level
//  // TODO: methods for the other additional (extended) shapes
//  override def visitStroke(r: Location, s: Stroke) = r
//  // TODO: reduce Circle to Ellipse (avoid code duplication)
//  // etc.
//}
//
//object ExtendedBoundingBox extends ExtendedBoundingBox