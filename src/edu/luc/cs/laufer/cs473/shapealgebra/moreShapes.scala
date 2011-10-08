package edu.luc.cs.laufer.cs473.shapealgebra

import java.awt.Color

/*
 * Some additional shape classes added later.
 */

// TODO: your job (avoid inheritance among case classes)
//case class Ellipse(halfWidth: Int, halfHeight: Int) extends Shape
//case class Location(x: Int, y: Int, shape: Shape) extends Decorator(shape)
//val simpleStroke = Stroke(Color.ORANGE, Circle(50))

case class Stroke(color: Color, shape:Shape) extends Shape
case class Circle(radius: Int) extends Shape
case class Fill(s:Shape) extends Shape
case class Outline(s:Shape) extends Shape
case class Polygon(shapes:Shape*) extends Shape
case class Point(x:Int,y:Int) extends Shape
case class Rotate(theta:Int, s:Shape) extends Shape