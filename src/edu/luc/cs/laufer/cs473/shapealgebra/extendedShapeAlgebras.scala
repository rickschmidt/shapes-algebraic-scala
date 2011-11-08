package edu.luc.cs.laufer.cs473.shapealgebra




object ExtendedShapeSize extends ExtendedShapeAlgebra[Int] {
  override def visitEllipse(e: Ellipse) = 1
  override def visitStroke(r:Int, shape: Stroke) = r
  override def visitRotate(theta:Int,r:Rotate)=fold(r.shape)
  override def visitOutline(r:Int,o:Outline)=1
  override def visitFill(r:Int,f:Fill)=r
  //override def visitPoint(p:Point)=0
  override def visitCircle(c:Circle)=1
  override def visitPolygon(p:Polygon)=1
  override def visitLocation(r:Int, l:Location)=r
  override def visitGroup(rs:Seq[Int],g:Group)=rs.sum

  override def visitRectangle(r:Rectangle)=1  
  
}

object ExtendedShapeDepth extends ExtendedShapeAlgebra[Int] {
  // TODO: all methods defined from scratch
  override def visitEllipse(e: Ellipse) = 1
  override def visitStroke(r:Int,s:Stroke)=1+r
  override def visitRotate(r:Int,s:Rotate)=1+fold(s.shape)
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
    println("S1 is "+s1.shape)
    println("r is "+r)
    println("theta is "+s1.theta)
    val leftWidth2=Math.cos(90-s1.theta)*s.shape.asInstanceOf[Rectangle].height
    val rightWidth2=Math.cos(s1.theta)*s.shape.asInstanceOf[Rectangle].width
    println("leftWidth2 "+leftWidth2)
    println("rightWidth2 "+rightWidth2)
    val totalWidth=leftWidth2+rightWidth2
    
            
    val leftWidth=Math.sin(Math.toRadians(s1.theta))*s.shape.asInstanceOf[Rectangle].height
    val rightWidth=Math.sin(Math.toRadians(90-s1.theta))*s.shape.asInstanceOf[Rectangle].width
    println("left width "+leftWidth)
    println("right width "+rightWidth)
    val x=(s.x-leftWidth).toInt
    
    
    val x1=s.shape.asInstanceOf[Rectangle].height
    val y1=s.shape.asInstanceOf[Rectangle].width
    val r1=Math.sqrt(Math.pow(x1,2)+Math.pow(y1,2))
    println("r1 "+r1)
    val theta=Math.atan2(y1,x1)
    val atan2=Math.atan2(200,100)
    println("res "+r1*Math.cos(atan2+Math.toRadians(60)))
    println("atan2 "+atan2)
    val atan2a=Math.atan2(-200,-100)
    println("res2 "+r1*Math.cos(atan2a+Math.toRadians(60)))
    println("atan2 "+atan2a)
    val xheight=r1*Math.cos(Math.toRadians(s1.theta+(Math.atan(s.shape.asInstanceOf[Rectangle].height/s.shape.asInstanceOf[Rectangle].width))))
    
    
    val deg=90-s1.theta
    val theta2=Math.atan(s.shape.asInstanceOf[Rectangle].height/s.shape.asInstanceOf[Rectangle].width)
    println("theta2 "+theta2)
    val t3=90-Math.toDegrees(theta2)
    
    val bottomLeft=List(s.x,s.y)
    val topLeft=List(s.x,s.y+s.shape.asInstanceOf[Rectangle].height)
    val bottomRight=List(s.x+s.shape.asInstanceOf[Rectangle].width,s.y)
    val topRight=List(s.x+s.shape.asInstanceOf[Rectangle].width,s.y+s.shape.asInstanceOf[Rectangle].height)
    
    val coords=List(bottomLeft,topLeft,bottomRight,topRight)
    //polars2 is a List of Lists where each list contains the polar coordinate the original x and original y of the rectangle
    val polars2=coords.map(c=>{
      List(Math.atan2(c(1),c(0)),c(0),c(1))
    })
  //x2 = x0+(x-x0)*cos(theta)+(y-y0)*sin(theta)
  //y2 = y0-(x-x0)*sin(theta)+(y-y0)*cos(theta)
    val transformedPolars=polars2.map(c=>{
           
      List(
          Math.sqrt( (Math.pow(c(1),2)+Math.pow(c(2),2)))*Math.cos(c(0)+ Math.toRadians(s1.theta)),          
          Math.sqrt( (Math.pow(c(1),2)+Math.pow(c(2),2)))*Math.sin(c(0)+ Math.toRadians(s1.theta))
          )
    })
    
    println("tranformedPolars "+transformedPolars)
    val xsys=transformedPolars.unzip{e=>{(e(0),e(1))}}
    
    
    println("polars 2: "+polars2)
    println("bl "+bottomLeft)
    println("tl "+topLeft)
    println("br "+bottomRight)
    println("tr "+topRight)
//    
//    val bottomLeftPolar=Math.atan2(bottomLeft(1),bottomLeft(0))
//    val topLeftPolar=Math.atan2(topLeft(1),topLeft(0))
//    val bottomRightPolar=Math.atan2(bottomRight(1),bottomRight(0))
//    val topRightPolar=Math.atan2(topRight(1),topRight(0))
//    
//    println("blp "+bottomLeftPolar)
//    println("tlp "+topLeftPolar)
//    println("brp "+bottomRightPolar)
//    println("trp "+topRightPolar)
//    
//    
//    val polars=List(bottomLeftPolar,bottomRightPolar,topLeftPolar,topRightPolar)
//    val transformedPolars=polars.map(c=>{
//      c+Math.toRadians(60)
//    })
//    println("transpolar "+transformedPolars)
//    //x2 = x0+(x-x0)*cos(theta)+(y-y0)*sin(theta)
//    //y2 = y0-(x-x0)*sin(theta)+(y-y0)*cos(theta)
//    val transformedCartesians=transformedPolars.map(c=>{
////      List((-200*Math.cos(c))+(-100*Math.sin(c)),(-100*Math.sin(c)+(-100*Math.cos(c))))
////      List(-200*Math.cos(c)+200*Math.sin(c),-100*Math.sin(c)+200*Math.cos(c))
//      
//    })
//    println("transformedCart "+transformedCartesians)
    val yheight=r1*Math.sin(Math.toRadians(t3+s1.theta))


    
    
    val width=Math.round(leftWidth)+Math.round(rightWidth).toInt
//    //0 for y only works for extended 1 and 2
    println(Location(x,0,Rectangle((leftWidth+rightWidth).toInt,r1.toInt)))
//    h' = |w*sin(alpha)| + | h* cos(alpha)|
//    w' = |w* cos(alpha)| + |h * sin(alpha)|
    Location(xsys._1.min.toInt,xsys._2.min.toInt,Rectangle((xsys._1.max-xsys._1.min).toInt,(xsys._2.max-xsys._2.min).toInt))
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