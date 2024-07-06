package Simulator
import math.sqrt

object Mathematics {

  // The gravitational constant.
  private val gravitationalConstant = 6.67408E-11

  // Returns the acceleration of an object influenced by the gravitation of another object.
  private def acceleration(target: State, other: State): Vector3D = {
    val distanceVector = other.coords - target.coords
    val acceleration = (gravitationalConstant * other.mass) / (distanceVector.length * distanceVector.length)
    distanceVector.getUnit * acceleration
  }

  // A method for calculating the acceleration caused to a single object by a system of objects.
  private def accelerationMultiple(target: State, others: Array[State]) = {
    var accelerationVector = Vector3D(0, 0, 0) // Initialize an acceleration vector.
     for (state <- others) {
       if (!state.eq(target)) { // Calculate the acceleration caused by every object except itself.
         accelerationVector = accelerationVector + acceleration(target, state) // Add the calculated acceleration to the total acceleration vector.
       }
     }
    accelerationVector // Finally return the calculated acceleration vector.
  }

  // Get the derivatives v -> dv/dt, x -> dx/dt at time t.
  private def getDerivative(state: State, others: Array[State]): Derivative = {
    val dv = accelerationMultiple(state, others) // dv = change in velocity = acceleration
    val dx = state.v // dx = change in position = velocity
    Derivative(state.mass, dv, dx) // Wrap the calculated values into a derivative object.
  }

  // Get all of the derivatives of a system.
  def getAllDerivatives(states: Array[State]): Array[Derivative] = {
    val derivatives = new Array[Derivative](states.length) // Initialize an Array for the derivatives.
    for (i <- states.indices) {
      derivatives(i) = getDerivative(states(i), states) // Calculate the derivative of a state and add it to the derivatives Array.
    }
    derivatives // Return the calculated derivatives.
  }

  // Calculates the physics state at t + dt and return the derivatives at t + dt.
  private def evaluate(initialStates: Array[State], step: Double, derivatives: Array[Derivative]) : Array[Derivative] = {
    val newDerivatives = new Array[Derivative](initialStates.length) // Initialize the new Array for the derivatives.
    val newStates = new Array[State](initialStates.length) // Initialize the new Array for the new states used to calculate the derivatives.

    // Calculate the new states of the objects and add them to newStates.
    for (i <- initialStates.indices) {
      val v = initialStates(i).v + ((derivatives(i).dv * step)) // v = v + dv * dt
      val x = initialStates(i).coords + ((derivatives(i).dx * step)) // x = x + dx * dt
      newStates(i) = State(initialStates(i).mass, v, x) // Create a new state with the calculated values and add it to the newStates Array.
    }

    // Calculate the new derivatives of the objects using the already calculated new states.
    for (i <- newStates.indices) {
      val dx = newStates(i).v
      val dv = accelerationMultiple(newStates(i), newStates)
      newDerivatives(i) = Derivative(newStates(i).mass, dv, dx)
    }
     newDerivatives // Return the calculated derivatives.
  }

  // Approximates the next states of a system at t + dt using the Runge-Kutta 4 algorithm.
  def integrate(initialStates: Array[State], step: Double) = {

    // Calculate the derivatives at four different points, each using the derivatives calculated in the previous steps.
    val a = evaluate(initialStates, 0, getAllDerivatives(initialStates))
    val b = evaluate(initialStates, step * 0.5, a)
    val c = evaluate(initialStates, step * 0.5, b)
    val d = evaluate(initialStates, step, c)

    // Initialize a new empty Array for the new states and calculate them using weighted values for a, b, c and d.
    val newStates = new Array[State](initialStates.length)
    for (i <- initialStates.indices) {
      val dxdt = ((b(i).dx + c(i).dx) * 2 + a(i).dx + d(i).dx) * (1.0/6.0) // approximation for dxdt = velocity (weighted value).
      val dvdt = ((b(i).dv + c(i).dv) * 2 + a(i).dv + d(i).dv) * (1.0/6.0) // approximation for dvdt = acceleration (weighted value).

      // Create a new state and add it to the newStates Array.
      newStates(i) = State(initialStates(i).mass, initialStates(i).v + (dvdt * step) , (initialStates(i).coords + (dxdt * step)))
    }
    // Finally returns all the new states calculated by the function.
    newStates
  }

  def distanceTo(first: SpaceObject, second: SpaceObject) = (second.state.coords - first.state.coords).length - second.radius

}


// A class for a state of an object.
case class State(val mass: Double, // The mass of the object in kilorgrams (kg).
                 val v: Vector3D, // The velocity vector of the object in meters per second (m/s)
                 val coords: Vector3D) // The coordinates vector of the object depicting them as a vector going from the origin to the position of the object. The units are meters (m).

// A class for the derivative of a state. v -> dv/dt and x -> dx/dt. Mass remains the same.
case class Derivative(val mass: Double, // The mass of the object in kilorgrams (kg).
                      val dv: Vector3D, // The vector depicting the acceleration (= dv/dt) of the object in meters per second squared (m/s^2).
                      val dx: Vector3D) // The vector depicting the change in position (= dx/dt) of the object in meters per second (m/s).

// A vector in three-dimensional space.
case class Vector3D(val x: Double, val y: Double, val z: Double) {

  // Method for adding vectors together.
  def +(other: Vector3D) = Vector3D(this.x + other.x, this.y + other.y, this.z + other.z)

  // Method for subtracting vectors.
  def -(other: Vector3D) = Vector3D(this.x - other.x, this.y - other.y, this.z - other.z)

  // Method for multiplying a vector by a scalar.
  def *(scalar: Double) = Vector3D(this.x * scalar, this.y * scalar, this.z * scalar)

  // Returns the length of the vector.
  def length = sqrt((this.x * this.x) + (this.y * this.y) + (this.z * this.z))

  // Makes the vector into an unit vector and returns it. (an unit vector is a vector of length 1)
  def getUnit = Vector3D(this.x / this.length, this.y / this.length, this.z / this.length)

  override def toString: String = s"x: $x, y: $y, z: $z"

}