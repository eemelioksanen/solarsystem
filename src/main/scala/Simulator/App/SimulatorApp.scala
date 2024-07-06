package Simulator.App
import Simulator.Simulation
import java.io._

object SimulatorApp extends App {

  val helloText =
    """Welcome to Solar System Simulator!
      |This program is created by Eemeli Oksanen.
      |What would you like to do?
      |""".stripMargin
  val options =
    """
      |1: Create a new simulation
      |2: Help
      |3: Exit
      |""".stripMargin

  val helpText =
    """
      |This is a program used for simulating solar systems or other space interactions.
      |To get started, you have to create a text file containing the initial information about the system you wish to simulate.
      |The file must be formatted the following way:
      |name; mass; velocity; position; radius
      |For velocity and position, enter the three-dimensional values separated by a comma (,).
      |Attributes are separated by the semicolon (;).
      |The objects are separated by entering them on their own lines.
      |Example data:
      |Test planet; 100; 1, 2, 3; 5, 5, 5; 10
      |Please enter all values in their respective SI-units. For example, mass in kg, speed in m/s and position in m.
      |Use a dot (.) as a decimal separator.
      |Check the documentation for more information.
      |""".stripMargin

  println(helloText)

  var hasQuit = false

  while (!hasQuit) {
    println(options)
    var choice = scala.io.StdIn.readLine()

      choice match {
        case "1" =>
          simulate()
        case "2" =>
          println(helpText)
        case "3" =>
          println("Bye!")
          hasQuit = true
        case _ =>
          println("That was not a valid choice!")
    }
  }

  // Start the simulation
  def simulate() = {
    var step = scala.io.StdIn.readLine("Please enter the timestep you would like to use in seconds: ").toDoubleOption
    while (step.isEmpty) {
      step = scala.io.StdIn.readLine("Please enter a valid number: ").toDoubleOption
    }

    var duration = scala.io.StdIn.readLine("Please enter the duration you would like to simulate in seconds: ").toDoubleOption
    while (duration.isEmpty || duration.get <= step.get) {
      duration = scala.io.StdIn.readLine(s"Please enter a valid number greater than ${step.get}: ").toDoubleOption
    }

    var filePath = scala.io.StdIn.readLine("Please enter the file path of the simulation file: ")
    var successfulRead = false
    var simulationOption: Option[Simulation] = None
    while (!successfulRead) {
      try {
        simulationOption = Option(new Simulation(step.get, duration.get, filePath))
        successfulRead = true
      } catch {
        case e: FileNotFoundException =>
          filePath = scala.io.StdIn.readLine("The file was not found, please enter a correct file path: ")
        case e: Exception =>
          println(s"An error occured while processing the file: ${e}!")
          println("Perhaps the file path was not correct or the file is not formatted correctly?")
          filePath = scala.io.StdIn.readLine("Please enter a valid file: ")
      }
    }
    // The simulation should have been properly read so save it to a variable.
    val simulation = simulationOption.get

    println("""
    |How would you like to get the calculated data?
    |1: Save as a gnuplot-readable text file.
    |2: Print the data to the console (Warning: will most likely be hard to read for large systems.)
    |""".stripMargin)

    // A variable for keeping track of whether the program should finish.
    var runFinished = false

    while (!runFinished) {
      var printType = scala.io.StdIn.readLine("Please choose the method of printing the data: ")
      printType match {
        case "1" =>
          runWithWriteToFile()
          runFinished = true
        case "2" =>
          runWithPrintToConsole()
          runFinished = true
        case _ =>
          println("Invalid choice!")
      }
    }

    // Run the program and write the data to a file.
    def runWithWriteToFile() = {
      var fileWriter: Option[FileWriter] = None
      while (fileWriter.isEmpty) {
      fileWriter = try {
        Option( new FileWriter(scala.io.StdIn.readLine("Please enter the file path you want to save the data to: ")))
        } catch {
        case e: FileNotFoundException =>
          println("There was an error creating or opening that file. Perhaps you don't have the permission to write?")
          None
        case e: IOException =>
          println(s"An IO-exception occured: ${e}")
          None
        case e: Exception =>
          println("An unknown exception occured while attempting to write the file..")
          None
        }
      }

      val lineWriter = new BufferedWriter(fileWriter.get)
      var ended = false
      try {
        while (!ended) {
          println("Processing...")
          try {
            writeSimulationToFile(simulation, lineWriter)
            if (simulation.simulationEnded) ended = true
            scala.io.StdIn.readLine("Process completed successfully! Press enter to return to main menu: ")
          } catch {
            case e: IOException =>
              println(s"An exception occured: ${e}")
              var choice = scala.io.StdIn.readLine("Do you want to attempt continuing? (y/n): ")
              while (choice != "y" || choice != "n") choice = scala.io.StdIn.readLine("Please enter a valid answer (y/n): ")
              choice match {
                case "y" =>
                  println("Reattempting...")
                case "n" =>
                  println("Process aborted.")
                  ended = true
              }
          }
        }
      } finally {
        lineWriter.close()
        fileWriter.get.close()
      }
    }

    // Run the program and print the data to the console.
    def runWithPrintToConsole() = {
      var finished = false
      while (!finished) {
        try {
          println("Processing...")
          while (!simulation.simulationEnded) {
            var writeData = s"Time: ${simulation.getTime.toString}"
            simulation.getItems.foreach(n => writeData += s" ${n.name}: x: ${n.state.coords.x} y: ${n.state.coords.y} z: ${n.state.coords.z}")
            println(writeData)
            simulation.advance()
          }
          finished = true
          scala.io.StdIn.readLine("Process completed successfully! Press enter to return to main menu: ")
        } catch {
          case e: IOException =>
            println(s"An exception occured: ${e}")
            var choice = scala.io.StdIn.readLine("Do you want to attempt continuing? (y/n): ")
            while (choice != "y" || choice != "n") choice = scala.io.StdIn.readLine("Please enter a valid answer (y/n): ")
            choice match {
              case "y" =>
                println("Reattempting...")
              case "n" =>
                println("Process aborted.")
                finished = true
            }
        }
      }
    }
  }

  private def writeSimulationToFile(simulation: Simulation, lineWriter: BufferedWriter) = {
    while (!simulation.simulationEnded) {
      var writeData = simulation.getTime.toString
      simulation.getItems.foreach(n => writeData += s" ${n.state.coords.x} ${n.state.coords.y} ${n.state.coords.z}")
      lineWriter.write(writeData)
      lineWriter.newLine()
      simulation.advance()
    }
  }
}
