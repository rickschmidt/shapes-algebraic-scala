package edu.luc.cs.laufer.cs473.shapealgebra

import java.awt.Graphics2D

class ExtendedDraw extends Draw {

  override def draw(g: Graphics2D)(s: Shape): Unit = s match {
    // TODO: cases for the additional shapes
	// TODO: reduce Circle to Ellipse (avoid code duplication)

    case Circle(c)=>g.drawArc(-c, -c, 2 * c, 2 * c, 0, 360)
    case Stroke(c,s)=>{
     g.setPaint(c)
     
      println("Color set to "+g.getColor())
      val stroke=new java.awt.BasicStroke(0.0f)
      g.setStroke(stroke)
      g.setPaint(c)
      draw(g)(s)
      println("Color set to2 "+g.getColor())
      g.setColor(java.awt.Color.black)
      g.setColor(java.awt.Color.white)
      println("Color set to3 "+g.getColor())
//      g.setStroke(new java.awt.BasicStroke(1))
     
    }
    case Rotate(r,s)=>{
      g.rotate(r)
      draw(g)(s)
      g.rotate(-r)
    }
    case Polygon(s)=>println("points "+s)
    case Fill(s)=>{
      println("draw >fill")
      fill(g)(s)
    }
    case Outline(s)=>draw(g)(s)
      
      
    
    
    
    
    
  	case _ => super.draw(g)(s)

  }

  def fill(g: Graphics2D)(s: Shape): Unit = s match {
    case Ellipse(hw, hh) => g.fillArc(-hw, -hh, 2 * hw, 2 * hh, 0, 360)
    case Rectangle(w, h) =>{
      println("fill rect")
      g.fillRect(0, 0, w, h)
    }
    // TODO: reduce Circle to Ellipse (avoid code duplication)
    case Circle(c)=>{
      println("fill circle")
      g.fillArc(-c, -c, 2 * c, 2 * c, 0, 360)
    }
    case _ => draw(g)(s)
  }
}

object ExtendedDraw extends ExtendedDraw {
  def apply(g: Graphics2D) = draw(g)(_)
}