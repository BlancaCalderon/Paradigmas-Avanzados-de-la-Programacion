package codigo

import java.awt.{BorderLayout, Toolkit}
import java.awt.event.MouseAdapter
import java.io.File
import javax.imageio.ImageIO
import javax.swing.ImageIcon
import scala.collection.mutable.ListBuffer
import scala.runtime.BoxesRunTime.{add, takeNot}
import scala.swing.BorderPanel.Position
import scala.swing.MenuBar.NoMenuBar.contents
import scala.swing._
import scala.swing.event.{ButtonClicked, MouseClicked, MouseEvent}
import scala.util.Random
import scala.swing.BorderPanel


class ventanaInicial extends MainFrame {
  title = "Cundy Crosh Saga"
  preferredSize = new Dimension(320, 240)

  val button = new Button("Seleccionar opciones")
  {
    preferredSize = new Dimension(50, 20)
  }
  contents = new BorderPanel
   {
    layout(new Label("Cundy Crosh Saga!")) = Position.North
    layout(button) = Position.South
    }

  button.reactions +=
    {
    case event.ButtonClicked(_) =>
      val ventana2 = new ventanaMenu
      ventana2.visible = true
      dispose()
  }
}

class ventanaMenu extends MainFrame
{
  title = "Selecciona opciones de juego"
  preferredSize = new Dimension(320, 240)


  val numFilasTexto= new TextField {
    columns = 2
  }
  val numColTexto = new TextField {
    columns = 2
  }
  val dificultadCuadro = new ComboBox(List("Facil", "Dificil"))
  val modoJuegoCuadro = new ComboBox(List("Automatico", "Manual"))

  val startButton = new Button("Start game") {
    reactions += {
      case ButtonClicked(_) =>
        val numFilas = numFilasTexto.text.toInt
        val numCol = numColTexto.text.toInt
        val dificultad = dificultadCuadro.selection.item
        val modoJuego = modoJuegoCuadro.selection.item

        //Main.comenzarJuego(numFilas, numCol, dificultad, modoJuego)

        val ventana3 = new ventanaTablero(numFilas, numCol, Main.conversionDificultad(dificultad), Main.conversionModoJuego(modoJuego))
        ventana3.visible = true
        dispose()
    }
  }

  contents = new BorderPanel {
    layout(new GridPanel(4, 2) {
      contents += new Label("Numero de filas del tablero:")
      contents += numFilasTexto
      contents += new Label("Numero de columnas del tablero:")
      contents += numColTexto
      contents += new Label("Dificultad:")
      contents += dificultadCuadro
      contents += new Label("Modo de juego:")
      contents += modoJuegoCuadro
    }) = Position.Center
    layout(startButton) = Position.South
  }
}

class ventanaTablero(numFilas: Int, numCol: Int, dificultad: Int, modoJuego: Char) extends MainFrame
{
  title = "TABLERO DEL JUEGO"
  preferredSize = new Dimension(numCol * 50, numFilas * 50)

  val tablero: List[Int] = funcionesTablero.inicializarTablero(Nil, dificultad, numFilas * numCol)

  val panelTablero = new dibujarTablero(tablero, numFilas, numCol, dificultad, modoJuego)
  contents = new BoxPanel(Orientation.Vertical){
    contents += panelTablero
  }
  panelTablero.visible = true

  centerOnScreen()
  visible = true

}

/*class ventanaTablero(numFilas: Int, numCol: Int, dificultad: Int, modoJuego: Char) extends MainFrame {
  title = "Tablero"

  // Crea el tablero
  val tablero: List[Int] = funcionesTablero.inicializarTablero(Nil, dificultad, numFilas * numCol)


  funcionesTablero.mostrarTablero(tablero.toList, 0, numFilas, numCol)

  // Inicializar la lista mutable de labels
  val gridPanel = new GridPanel(numFilas, numCol) {
    preferredSize = new Dimension(400, 400)
    for (i <- 0 until numFilas * numCol) {
      // Crea un Label con una imagen de fondo aleatoria
      val label = new Label {
        preferredSize = new Dimension(25, 25)
        icon = new ImageIcon(getImagen(tablero(i)))
      }
      // Agrega el Label al GridPanel
      contents += label
    }

    // Función para obtener imagen correspondiente al numero del tablero
    def getImagen(n: Int): String = {
      val fichasNormales = Array("src/codigo/ficha1.PNG", "src/codigo/ficha2.PNG", "src/codigo/ficha3.PNG", "src/codigo/ficha4.PNG", "src/codigo/ficha5.PNG", "src/codigo/ficha6.PNG", "src/codigo/TNT.PNG")
      fichasNormales(n)
    }
  }

  // Agrega el GridPanel a la ventana
  contents = new BorderPanel {
    layout(gridPanel) = Position.Center
  }

  // Método para manejar el evento de ratón
  def mouseEventHandler(e: MouseEvent): Unit = {
    println("Holaaa")
    val point = e.point
    val peer = gridPanel.peer
    val insets = peer.getInsets()
    val cellWidth = peer.getWidth() / numCol
    val cellHeight = peer.getHeight() / numFilas
    val row = (point.getY() - insets.top) / cellHeight
    val col = (point.getX() - insets.left) / cellWidth

    if (row >= 0 && col >= 0 && row < numFilas && col < numCol)
    {
      println(s"Se ha pulsado en la casilla (${row.toInt}, ${col.toInt})")
      val pos_encontrar: Int = row.toInt * numCol + col.toInt
      funcionesTablero.jugar(numFilas, numCol, dificultad, tablero, modoJuego, pos_encontrar)
      val ventana3 = new ventanaTablero(numFilas, numCol, dificultad,modoJuego)
      ventana3.visible = true
      dispose()
    }
  }
    // Agrega el comportamiento de ratón al GridPanel
    listenTo(gridPanel.mouse.clicks)
    reactions += {
      case e: MouseClicked => mouseEventHandler(e)
    }

    // Mostrar la ventana
    centerOnScreen()
    visible = true
  }*/

class dibujarTablero(tablero: List[Int], numFilas: Int, numCol: Int, dificultad: Int, modoJuego: Char) extends Component
{
  val (ancho, alto) = (numCol * 50, numFilas * 50)

  var tableroPanel: List[Int] = tablero

  funcionesTablero.mostrarTablero(tablero, 0, numFilas, numCol)
  funcionesTablero.mostrarTablero(tableroPanel, 0, numFilas, numCol)
  override def paintComponent(g: Graphics2D): Unit =
  {
    super.paintComponent(g)

    preferredSize = new Dimension(ancho, alto)
    for {i <- 0 until numFilas
      j <- 0  until numCol} {
      val coordX = j * 25
      val coordY = i * 25

      val archivo = new File(getImagen(determinarFicha(tableroPanel(i * numCol + j))))
      val imagen = ImageIO.read(archivo)

      g.drawImage(imagen, coordX, coordY, 25, 25 , null)
    }
  }
    def getImagen(n: Int): String = {
      val fichasNormales = Array("src/codigo/ficha1.PNG", "src/codigo/ficha2.PNG", "src/codigo/ficha3.PNG", "src/codigo/ficha4.PNG", "src/codigo/ficha5.PNG", "src/codigo/ficha6.PNG", "src/codigo/RC1.PNG", "src/codigo/RC2.PNG", "src/codigo/RC3.PNG", "src/codigo/RC4.PNG", "src/codigo/RC5.PNG", "src/codigo/RC6.PNG", "src/codigo/bomba.PNG", "src/codigo/TNT.PNG")
      fichasNormales(n)
    }

    def determinarFicha( valor: Int): Int = {
      if(valor < 7)  valor - 1
      else if(valor < 14 && valor > 7) valor - 2
      else if(valor == 66) 12
      else if(valor == 84) 13
      else throw new Error("ERROR")
    }

  if(modoJuego == 'a')
  {
    tableroPanel =  funcionesTablero.jugar(numFilas, numCol, dificultad, tableroPanel, modoJuego, funcionesTablero.conseguirMejorJugada(tableroPanel, 0, numFilas, numCol, numFilas*numCol, dificultad, 0))
    repaint()
  }
  def mouseClicked(e: MouseEvent): Unit = {
    println("Holaaa")
    val point = e.point
    val insets = peer.getInsets()
    val cellWidth = 25 //peer.getWidth() / numCol
    val cellHeight = 25 //peer.getHeight() / numFilas
    val row = (point.getY() - insets.top) / cellHeight
    val col = (point.getX() - insets.left) / cellWidth
    if (row >= 0 && col >= 0 && row < numFilas && col < numCol) {
      println(s"Se ha pulsado en la casilla (${row.toInt}, ${col.toInt})")
      val pos_encontrar: Int = row.toInt * numCol + col.toInt
      tableroPanel = funcionesTablero.jugar(numFilas, numCol, dificultad, tableroPanel, modoJuego, pos_encontrar)
      this.repaint()
    }
  }

  // Agrega el comportamiento de ratón al GridPanel
  listenTo(mouse.clicks)
  reactions += {
    case e: MouseClicked => mouseClicked(e)
  }

  preferredSize = new Dimension(ancho, alto)

  val screenSize = Toolkit.getDefaultToolkit().getScreenSize()
  val x = (screenSize.getWidth() - preferredSize.getWidth()) / 2
  val y = (screenSize.getHeight() - preferredSize.getHeight()) / 2
  new Point(x.toInt, y.toInt)


}

object Main
{
  def main(args: Array[String]): Unit = {
    val ventana1 = new ventanaInicial
    ventana1.visible = true
    println("Fin")

  }

  def comenzarJuego( numFilas: Int, numCol: Int, dificultadInterfaz :String, modoJuegoInterfaz: String):Unit =
  {
    val dificultad: Int = conversionDificultad(dificultadInterfaz)
    val modoJuego: Char = conversionModoJuego(modoJuegoInterfaz)

    val size: Int = numFilas * numCol
    val tablero : List[Int] = funcionesTablero.inicializarTablero(Nil, dificultad, size)
    funcionesTablero.jugar(numFilas, numCol, dificultad, tablero,  modoJuego, size)
  }

  def conversionDificultad(dificultadInterfaz :String):Int =
   {
     if(dificultadInterfaz == "Facil") 4
     else if(dificultadInterfaz == "Dificil") 6
     else -1
   }

  def conversionModoJuego(modoJuegoInterfaz: String): Char = {
    if (modoJuegoInterfaz == "Automatico") 'a'
    else if (modoJuegoInterfaz == "Manual") 'm'
    else 'f'
  }

}