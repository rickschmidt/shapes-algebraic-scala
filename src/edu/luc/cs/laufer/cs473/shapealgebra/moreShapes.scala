package edu.luc.cs.laufer.cs473.shapealgebra

import java.awt.Color

/*
 * Some additional shape classes added later.
 */

// TODO: your job (avoid inheritance among case classes)


case class Stroke(color: Color, shape:Shape) extends Decorator(shape)
case class Circle(radius: Int) extends Shape
case class Fill(shape:Shape) extends Decorator(shape)
case class Outline(shape:Shape) extends Decorator(shape)
case class Polygon(points:Point*) extends Composite(points: _*)
case class Point(x:Int,y:Int) extends Shape
case class Rotate(theta:Int, shape:Shape) extends Decorator(shape)
case class Shear(xshift:Int,yshift:Int, shape:Shape)extends Decorator(shape)