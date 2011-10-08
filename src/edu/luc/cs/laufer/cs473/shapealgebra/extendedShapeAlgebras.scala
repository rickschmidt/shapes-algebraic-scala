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
  override def visitStroke(r:Int, shape: Stroke) = 1
  // TODO: methods for the other additional (extended) shapes
  override def visitRotate(r:Int,s:Shape)=1
  override def visitOutline(s:Shape)=1
  override def visitFill(f:Shape)={
    println("fill")
    1
  }
  override def visitPoint(p:Point)=1
  override def visitCircle(c:Circle)=1
  override def visitPolygon(p:Polygon)={
    
    println(p)
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
  override def visitFill(f:Shape)={
    println("fill")
    println(ExtendedShapeSize.visitFill(f)+1)
    ExtendedShapeSize.visitFill(f)+1
  }
  override def visitPoint(p:Point)=ExtendedShapeSize.visitPoint(p)+1
  override def visitCircle(c:Circle)=1
  override def visitPolygon(p:Polygon)={
    println("polygon")
    ExtendedShapeSize.visitPolygon(p)+1
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