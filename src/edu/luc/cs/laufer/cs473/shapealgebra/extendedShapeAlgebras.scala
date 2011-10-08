package edu.luc.cs.laufer.cs473.shapealgebra




object ExtendedShapeSize extends ExtendedShapeAlgebra[Int] {
  override def visitEllipse(e: Ellipse) = ShapeSize.visitEllipse(e)
  override def visitStroke(r:Int, shape: Stroke) = r
  override def visitRotate(r:Int,s:Shape)=1
  override def visitOutline(r:Int,o:Outline)=1
  override def visitFill(r:Int,f:Fill)=r
  override def visitPoint(p:Point)=0
  override def visitCircle(c:Circle)=1
  override def visitPolygon(ps:Seq[Int],p:Polygon)=1
  override def visitLocation(r:Int, l:Location)=1
  override def visitGroup(rs:Seq[Int],g:Group)=1
  override def visitRectangle(r:Rectangle)=1  
  
}

object ExtendedShapeDepth extends ExtendedShapeAlgebra[Int] {
  // TODO: all methods defined from scratch
  override def visitEllipse(e: Ellipse) = ShapeSize.visitEllipse(e)
  override def visitStroke(r:Int,s:Stroke)=ExtendedShapeSize.visitStroke(r,s)+1
  override def visitRotate(r:Int,s:Shape)=ExtendedShapeSize.visitRotate(r,s)+1
  override def visitOutline(r:Int,o:Outline)=ExtendedShapeSize.visitOutline(r,o)+1
  override def visitFill(r:Int,f:Fill)=ExtendedShapeSize.visitFill(r,f)+1
  override def visitPoint(p:Point)=ExtendedShapeSize.visitPoint(p)
  override def visitCircle(c:Circle)=1
  override def visitPolygon(ps:Seq[Int],p:Polygon)=ExtendedShapeSize.visitPolygon(ps:Seq[Int],p:Polygon)
  override def visitLocation(r:Int, l:Location)=ShapeSize.visitLocation(fold(l.shape), l)+1
  override def visitGroup(rs:Seq[Int],g:Group)=1
  override def visitRectangle(r:Rectangle)=1
  
}

class ExtendedBoundingBox extends BoundingBox with ExtendedShapeAlgebra[Location] {
  // methods for original shapes inherited at class level
  // TODO: methods for the other additional (extended) shapes
  override def visitStroke(r: Location, s: Stroke) = r
  
  override def visitRotate(r:Location,s:Shape)=r
  override def visitOutline(r:Location,o:Outline)=r
  override def visitFill(r:Location,f:Fill)=r
  override def visitPoint(p:Point)=Location(0,0,p)
  override def visitPolygon(ps:Seq[Location],p:Polygon)={
    val xs=ps map(p=>p.shape.asInstanceOf[Point].x)
    val xsMin=xs.min
 
    val ys=ps map(p=>p.shape.asInstanceOf[Point].y)
    val ysMin=ys.min
    
    val xsMax=xs.max
    val ysMax=ys.max
    
    val width=xsMax-xsMin
    val height=ysMax-ysMin
    Location(xsMin,ysMin,Rectangle(width,height))
  }
  override def visitCircle(c:Circle)=visitEllipse(Ellipse(c.radius,c.radius))
 
  

}

object ExtendedBoundingBox extends ExtendedBoundingBox