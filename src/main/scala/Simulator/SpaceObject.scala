package Simulator


trait SpaceObject {

  val name: String
  val radius: Double
  var state: State

  override def toString = s"Name: $name, Mass: ${state.mass} Radius: $radius, Location: ${state.coords}, Velocity: ${state.v}."

}

class Planet(val name: String, val radius: Double, var state: State) extends SpaceObject

class Satellite(val name: String, val radius: Double, var state: State) extends SpaceObject