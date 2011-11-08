package edu.luc.cs.laufer.cs473.shapealgebra




object ExtendedShapeSize extends ExtendedShapeAlgebra[Int] {
  override def visitEllipse(e: Ellipse) = 1
  override def visitStroke(r:Int, shape: Stroke) = r
  override def visitRotate(theta:Int,r:Rotate)=fold(r.shape)
  override def visitOutline(r:Int,o:Outline)=1
  override def visitFill(r:Int,f:Fill)=r
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
    
    //Shape is put through a bounding box
    val s=ExtendedBoundingBox(s1.shape)
    
    //For the sake of readability each coordinate is listed here
    val bottomLeft=List(s.x,s.y)
    val topLeft=List(s.x,s.y+s.shape.asInstanceOf[Rectangle].height)
    val bottomRight=List(s.x+s.shape.asInstanceOf[Rectangle].width,s.y)
    val topRight=List(s.x+s.shape.asInstanceOf[Rectangle].width,s.y+s.shape.asInstanceOf[Rectangle].height)
    
    //Coordinates are then all placed in a list.
    val coords=List(bottomLeft,topLeft,bottomRight,topRight)
    
    //polars is a List of Lists where each list contains the polar coordinate the original x and original y of the rectangle
    val polars=coords.map(c=>{
      List(Math.atan2(c(1),c(0)),c(0),c(1))
    })

    //TransformedPolars will rotate the polar coordinates and then convert them back into cartesian.
    val transformedPolars=polars.map(c=>{
      List(
          Math.sqrt( (Math.pow(c(1),2)+Math.pow(c(2),2)))*Math.cos(c(0)+ Math.toRadians(s1.theta)),          
          Math.sqrt( (Math.pow(c(1),2)+Math.pow(c(2),2)))*Math.sin(c(0)+ Math.toRadians(s1.theta))
          )
    })
    
    //The rotated cartesian coordinates are then unzipped into a list of x's and y's.
    val xsys=transformedPolars.unzip{e=>{(e(0),e(1))}}
    
    

    /*
     * The final Location of the rotated bounding box is (lowerleft) at the minimum x and minimum y coordinate. It's width is
     * a result of subtracting the min x from the max x and its height is a result of subtracting the min y from the max y.
    */
    Location(xsys._1.min.toInt,xsys._2.min.toInt,Rectangle((xsys._1.max-xsys._1.min).toInt,(xsys._2.max-xsys._2.min).toInt))

  }
  override def visitOutline(r:Location,o:Outline)=r
  override def visitFill(r:Location,f:Fill)=r
  override def visitPolygon(p:Polygon)={
    val xsys=p.points.toList.unzip{e=>{
      (e.x,e.y)
      }
    }
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