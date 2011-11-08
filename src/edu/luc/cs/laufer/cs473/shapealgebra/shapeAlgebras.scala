package edu.luc.cs.laufer.cs473.shapealgebra

object ShapeSize extends ShapeAlgebra[Int] {
  override def visitEllipse(e: Ellipse) = 1
  override def visitRectangle(r: Rectangle) = 1
  override def visitLocation(r: Int, l: Location) = r
  override def visitGroup(rs: Seq[Int], g: Group) = rs.sum
}

class BoundingBox extends ShapeAlgebra[Location] {
  override def visitEllipse(e: Ellipse) =
    Location(-e.halfWidth, -e.halfHeight, Rectangle(2 * e.halfWidth, 2 * e.halfHeight))
  override def visitRectangle(r: Rectangle) =
    Location(0, 0, r)
  override def visitLocation(b: Location, l: Location) = {
    Location(l.x + b.x, l.y + b.y, b.shape)
  }
  override def visitGroup(rs: Seq[Location], g: Group) = {
	// TODO: implement based on algorithm from previous subproject
	
	val z=rs.unzip{e=>{
      (e.x,e.y)
      }
    }
	
  val locationList= rs map(s1=>{
        ExtendedBoundingBox(s1)
      })
      
  		val s1=locationList.foldLeft[Location](locationList.head)((a,c)=>{
        val b=BoundingBox(c)
        

        val wmax=Math.max(b.x+b.shape.asInstanceOf[Rectangle].width,a.x+a.shape.asInstanceOf[Rectangle].width)
        val wmin=Math.min(b.x,a.x)
        val width=(wmax-wmin)
        
        val hmax=Math.max(b.y+b.shape.asInstanceOf[Rectangle].height,a.y+a.shape.asInstanceOf[Rectangle].height)
        val hmin=Math.min(b.y,a.y)
        val height=(hmax-hmin)
       
        Location(a.x,a.y,(Rectangle(width,height)))
        
       })


   Location(z._1.min,z._2.min,s1.shape)   
    }
     
}

object BoundingBox extends BoundingBox