package edu.luc.cs.laufer.cs473.shapealgebra

import java.awt.Graphics2D

class ExtendedDraw extends Draw {

  override def draw(g: Graphics2D)(s: Shape): Unit = s match {
    // TODO: cases for the additional shapes
	// TODO: reduce Circle to Ellipse (avoid code duplication)

    case Circle(c)=>draw(g)(Ellipse(c,c))
    case Stroke(c,s)=>{
      g.setStroke(new java.awt.BasicStroke(0.0f))
      g.setPaint(c)
      draw(g)(s)


      g.setColor(java.awt.Color.white)

      
     
    }
    case Rotate(r,s)=>{
      g.rotate(Math.toRadians(r))
      draw(g)(s)
      g.rotate(Math.toRadians(-r))
    }
    case Polygon(points @ _*)=> 
      val pts=points.toList.unzip{p=>{
       (p.x,p.y) 
      	}
      }
      g.drawPolygon(pts._1.toArray,pts._2.toArray,pts._1.size)
    case Fill(s)=>fill(g)(s)   
    case Outline(s)=>draw(g)(s)
     
    case Shear(x,y,s)=>{
      g.shear(x,y)
      draw(g)(s)
      g.shear(0,0)
    }
      
    
    
    
    
    
  	case _ => super.draw(g)(s)

  }

  def fill(g: Graphics2D)(s: Shape): Unit = s match {
    case Ellipse(hw, hh) => g.fillArc(-hw, -hh, 2 * hw, 2 * hh, 0, 360)
    case Rectangle(w, h) =>g.fillRect(0, 0, w, h)      
    case Circle(c)=>fill(g)(Ellipse(c,c))
    case _ => draw(g)(s)
  }
}

object ExtendedDraw extends ExtendedDraw {
  def apply(g: Graphics2D) = draw(g)(_)
}