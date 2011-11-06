package edu.luc.cs.laufer.cs473.shapealgebra




object ExtendedShapeSize extends ExtendedShapeAlgebra[Int] {
  override def visitEllipse(e: Ellipse) = ShapeSize.visitEllipse(e)
  override def visitStroke(r:Int, shape: Stroke) = r
  override def visitRotate(theta:Int,r:Rotate)=1
  override def visitOutline(r:Int,o:Outline)=1
  override def visitFill(r:Int,f:Fill)=r
  //override def visitPoint(p:Point)=0
  override def visitCircle(c:Circle)=1
  override def visitPolygon(p:Polygon)=1
  override def visitLocation(r:Int, l:Location)=r
  override def visitGroup(rs:Seq[Int],g:Group)=rs.sum
//  {
//    val a=g.shapes.foldLeft[Int](0)((a,c)=>{
//      println("c "+c)
//      a+1
//    })
//    println("a "+a)
//    a
//  }
//  {
//    val h=g.shapes.map(c=>ExtendedBoundingBox(c))
//    println("h "+h.size)
//    h.size
//  }
    
//    ExtendedShapeSize.visitGroup(rs,g)
//  {
//        println("rs "+rs)
//        println("g "+g.shapes)
//       
//        
//    val a=rs.foldLeft[Int](1)((a,c)=>{
//    	ExtendedShapeSize.visitGroup(rs,g)
//     
//    })
////   ExtendedShapeSize.visitGroup(rs.foldLeft[Int](1)((a,c)=>a+c),g)
//    
//    
//    
//  }
  override def visitRectangle(r:Rectangle)=1  
  
}

object ExtendedShapeDepth extends ExtendedShapeAlgebra[Int] {
  // TODO: all methods defined from scratch
  override def visitEllipse(e: Ellipse) = 1
  override def visitStroke(r:Int,s:Stroke)=1+r
  override def visitRotate(r:Int,s:Rotate)=1+ExtendedShapeSize.visitRotate(r,s)
  override def visitOutline(r:Int,o:Outline)=1+r
  override def visitFill(r:Int,f:Fill)=1+r
//  override def visitPoint(p:Point)=ExtendedShapeSize.visitPoint(p)
  override def visitCircle(c:Circle)=1
  override def visitPolygon(p:Polygon)=ExtendedShapeSize.visitPolygon(p:Polygon)
  override def visitLocation(r:Int, l:Location)=ShapeSize.visitLocation(fold(l.shape), l)+1
  override def visitGroup(rs:Seq[Int],g:Group)=1+rs.max
  override def visitRectangle(r:Rectangle)=1
  
}

class ExtendedBoundingBox extends BoundingBox with ExtendedShapeAlgebra[Location] {
  // methods for original shapes inherited at class level
  // TODO: methods for the other additional (extended) shapes
  override def visitStroke(r: Location, s: Stroke) = r
  
  override def visitRotate(r:Location,s1:Rotate)={
    
    
    val s=ExtendedBoundingBox(s1.shape)
    println("S is "+s)
    
            
    val leftWidth=Math.sin(Math.toRadians(s1.theta))*s.shape.asInstanceOf[Rectangle].height
    val rightWidth=Math.sin(Math.toRadians(90-s1.theta))*s.shape.asInstanceOf[Rectangle].width
    
    val x=(s.x-leftWidth).toInt
    
    val x1=s.shape.asInstanceOf[Rectangle].height
    val y1=s.shape.asInstanceOf[Rectangle].width
    val r1=Math.sqrt(Math.pow(x1,2)+Math.pow(y1,2))
    val theta=Math.atan2(y1,x1)
    
    val xheight=r1*Math.cos(Math.toRadians(s1.theta+(Math.atan(s.shape.asInstanceOf[Rectangle].height/s.shape.asInstanceOf[Rectangle].width))))
    
    
    val deg=90-s1.theta
    val theta2=Math.atan(s.shape.asInstanceOf[Rectangle].height/s.shape.asInstanceOf[Rectangle].width)
    val t3=90-Math.toDegrees(theta2)
    
    
    
    val yheight=r1*Math.sin(Math.toRadians(t3+s1.theta))



    
    val width=Math.round(leftWidth)+Math.round(rightWidth).toInt
//    //0 for y only works for extended 1 and 2
    Location(x,0,Rectangle(Math.floor(leftWidth).toInt+Math.round(rightWidth).toInt,Math.round(yheight).toInt))
//    Location(1,1,Rectangle(1,1))
  }
  override def visitOutline(r:Location,o:Outline)=r
  override def visitFill(r:Location,f:Fill)=r
  override def visitPolygon(p:Polygon)={
    println("points pre "+p.points.toList)
    println("points post "+p.points.toList.unzip{e => (e.x, e.y)})
    val xsys=p.points.toList.unzip{e=>{
      (e.x,e.y)
      }
    }
    println("xs: "+xsys._1.min)
    //width=xs.max -xs.min  
    //height=ys.max-ys.min
    //For the sake of readability...this could be more compact.
    val x=xsys._1.min
    val y=xsys._2.min
    val width=xsys._1.max-x
    val height=xsys._2.max-y
    Location(xsys._1.min,xsys._2.min,Rectangle(width,height))
  }
  override def visitCircle(c:Circle)=visitEllipse(Ellipse(c.radius,c.radius))
 
  

}

object ExtendedBoundingBox extends ExtendedBoundingBox