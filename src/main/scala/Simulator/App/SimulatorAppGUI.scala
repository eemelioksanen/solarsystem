package Simulator.App
import Simulator.Simulation
import scalafx.Includes._
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Pos.Center
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, Button, Label, Menu, MenuBar, MenuItem, ProgressBar, ScrollPane, TextField}
import scalafx.scene.image.Image
import scalafx.scene.layout.{Background, BackgroundFill, BorderPane, ColumnConstraints, CornerRadii, GridPane, HBox, RowConstraints, VBox}
import scalafx.scene.paint.Color._
import scalafx.scene.text.Font
import scalafx.stage.FileChooser
import scalafx.stage.FileChooser.ExtensionFilter

import java.io.File
import javax.swing.Timer

object SimulatorAppGUI extends JFXApp {

  var simulationFile: Option[File] = None

  var simulationPath: Option[String] = None

  var simulation: Option[Simulation] = None

  def simulationFileLoaded = simulationFile.isDefined

  stage = new PrimaryStage {

    title = "Solar system simulator"

    val image = new Image("jupiter.png")

    icons.add(image)

    scene = new Scene(1330, 800) {

      resizable = false

      root = new BorderPane {

        top = new MenuBar {

          // File menu.
          val menuFile = new Menu("File") {
            val itemNew = new MenuItem("New simulator from file...")
            val itemSave = new MenuItem("Export as a .dat file...")
            items = List(itemNew, itemSave)
          }

          // File chooser.
          val filecChooser = new FileChooser {
            title = "Open simulation file:"
             extensionFilters ++= Seq(
               new ExtensionFilter("Text Files", "*.txt")
             )
          }
          filecChooser.setInitialDirectory(new File("./"))

          // Load the simulation file using the filechooser.
          menuFile.itemNew.onAction = (event) => {
            simulationFile = Option(filecChooser.showOpenDialog(stage))
            if (simulationFileLoaded) {
              simulationPath = Option(simulationFile.get.getAbsolutePath)
              log.children += new Label(s"Selected simulation file: ${simulationPath.get}")
              startButton.setDisable(false)
              advanceButton.setDisable(false)
            }
          }

          // Add the File and Edit menu to the menu bar.
          menus = List(menuFile)
        }

        // Create a new GridPane.
        val grid = new GridPane

        center = grid

        // Bottom box contaning the console.
        val bottomBox = new VBox
        bottomBox.setPadding(Insets(10, 0, 10, 10))
        val logScroller = new ScrollPane
        val log = new VBox
        logScroller.setContent(log)
        logScroller.setMinSize(100, 150)
        logScroller.setMaxSize(1000, 150)
        log.setPadding(Insets(10, 10, 10, 10))
        bottomBox.children.add(logScroller)


        // A side box containing the toolbox.
        val sideBox = new VBox
          sideBox.setPadding(Insets(10))

          val simulationTextBox = new HBox
          simulationTextBox.setPadding(Insets(10))
          val simulationText = new Label("Simulation settings")
          simulationText.font = Font.font(20)
          simulationTextBox.children.add(simulationText)
          simulationTextBox.setAlignment(Pos(Center))

        // A box for the step and duration fields.
          val stepBox = new HBox
          stepBox.setSpacing(10.0)
          stepBox.setAlignment(Pos(Center))
          val stepText = new TextField
          val durationText = new TextField
          stepText.setPromptText("Step:")
          durationText.setPromptText("Duration: ")
          stepBox.children.addAll(durationText, stepText)

        // A box for the start button.
          val startBox = new HBox
          startBox.setSpacing(10.0)
          startBox.setAlignment(Pos(Center))
          val startButton = new Button("Create simulation")
          startButton.setDisable(true)
          startButton.setMinWidth(250)
          startBox.children.add(startButton)
          startBox.setPadding(Insets(10, 10, 10, 10))
          startButton.onAction = (event) => { // Try reading a file and create a simulation.
            try {
              createSimulation(simulationPath.get, stepText.text.get().toDouble, durationText.text.get().toDouble)
              log.children += new Label("Successfully started the simulation..")
              canvas.draw(simulation.get.getItems)
            }
            catch {
              case e: Exception => new Alert(AlertType.Warning) { // In case of an exception, alert the user.
                initOwner(stage)
                title = "Error!"
                headerText = s"An error Occured!"
                contentText =  e match {
                  case e: NoSuchElementException => "Please select a file to load the simulation from!"
                  case e: NumberFormatException => s"Please enter proper values for step and duration!"
                  case e: Exception => e.getMessage
                }
              }.showAndWait()
            }
          }

        // A box for the progress bar.
          val progressBox = new HBox
          progressBox.setSpacing(10.0)
          progressBox.setAlignment(Pos(Center))
          val progressBar = new ProgressBar
          val progressText = Label("Progress: ")
          progressBox.children.addAll(progressText, progressBar)

        // A box for the advance-button.
          val advanceBox = new HBox
          advanceBox.setSpacing(10.0)
          advanceBox.setAlignment(Pos(Center))
          val advanceButton = new Button("Advance simulation")
          advanceButton.setDisable(true)
          advanceButton.setMinWidth(250)
          advanceBox.children.add(advanceButton)
          advanceBox.setPadding(Insets(10, 10, 10, 10))
          advanceButton.onAction = (event) => try {
            val timer = AnimationTimer(t => {
            simulation.get.advance()
            g.drawImage(spaceImage, 0, 0)
            canvas.draw(simulation.get.getItems)
            progressBar.setProgress(simulation.get.getTime / simulation.get.duration)
            })
            timer.start()
          }

        // Add all the items to the sidebox.
        sideBox.children.addAll(simulationText, stepBox, startBox, progressBox, advanceBox)

        // Create the canvas for drawing the planets.
        val canvas = new DrawArea(1000, 600)
        val g = canvas.graphicsContext2D
        g.fill = Black
        val spaceImage = new Image("space.jpg")
        g.drawImage(spaceImage, 0, 0)



        //Method usage: add(child, columnIndex, rowIndex, columnSpan, rowSpan)
        grid.add(canvas, 0, 0)
        grid.add(sideBox, 1, 0, 1, 2)
        grid.add(bottomBox, 0, 1)

        val column0 = new ColumnConstraints
        val column1 = new ColumnConstraints
        val row0 = new RowConstraints
        val row1 = new RowConstraints

        column0.percentWidth = 75
        column1.percentWidth = 25
        row0.percentHeight = 75
        row1.percentHeight = 25

        grid.columnConstraints = Array[ColumnConstraints](column0, column1) //Add constraints in order
        grid.rowConstraints = Array[RowConstraints](row0, row1)

        sideBox.background = new Background(Array(new BackgroundFill((LightGray), CornerRadii.Empty, Insets.Empty))) //Set sideBox background color
        bottomBox.background = new Background(Array(new BackgroundFill((LightGray), CornerRadii.Empty, Insets.Empty))) //Set bottomBox background color

        val textBoxes = Array(stepBox, durationText)

      }
    }
  }

  // A method for creating a simulation from a file.
  def createSimulation(filePath: String, step: Double, duration: Double) = {
    val createdSimulation = new Simulation(step, duration, filePath)
    simulation = Option(createdSimulation)
  }



}
