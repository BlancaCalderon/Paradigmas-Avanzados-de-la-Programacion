package codigo

import java.awt.Color
import java.io.File
import javax.imageio.ImageIO
import scala.swing.BorderPanel.Position
import scala.swing._
import scala.swing.event.{ButtonClicked, MouseClicked, MouseEvent}
import scala.swing.BorderPanel


/**
 * Clase Ventana Inicial, es la primera en mostrarse
 */
class ventanaInicial extends MainFrame {
  title = "Cundy Crosh Saga"                                //Titulo
  preferredSize = new Dimension(320, 240)     //Establecemos la dimension de la ventana

  val fondo = ImageIO.read(new File("src/recursos/fondo.jpg"))

  val button = new Button("Seleccionar opciones") //Establecemos el texto que se mostrará en el botón
  {
    background = java.awt.Color.yellow
    preferredSize = new Dimension(50, 20)   //Dimension del boton
  }
  contents = new BorderPanel      //Creamos un panel para mostrar los contenidos de la ventana inicial
   {
     //El layout nos permite colocar la etiqueta y el boton en las posiciones del panel norte y sur
    layout(new Label("Cundy Crosh Saga!") {foreground = java.awt.Color.RED}) = Position.North
    layout(button) = Position.South //Ponemos un boton abajo

     override def paintComponent(g: Graphics2D) = //Dibuja la imagen de fondo en la ventana mediante el objeto Graphics2D proporcionado como argumento
     {
       super.paintComponent(g)
       g.drawImage(fondo, 0, 0, null)     //Establecemos la imagen de fondo en la pantalla
     }
     preferredSize = new Dimension(320, 240)
    }

  button.reactions +=     //Eventos del boton
    {
    case event.ButtonClicked(_) =>      //Si se pulsa el boton
      val ventana2 = new ventanaMenu    //Se muestra el MENU
      ventana2.visible = true           //Muestra la nueva ventana del menu
      dispose()                         //Cierra y libera los recursos asociados a la ventana inicial
  }
  centerOnScreen()    //Se ajusta la ventana del usuario para que quede centrada
}

/**
 * Es el MENU del juego
 * El usuario seleccionará los distintos parametros con los que desea jugar la partida (numFilas, numCol, modoJuego y dificultad)
 */
class ventanaMenu extends MainFrame
{
  title = "Selecciona opciones de juego"
  preferredSize = new Dimension(320, 240)

  val fondo = ImageIO.read(new File("src/recursos/fondo.jpg"))

  val numFilasTexto = new TextField    //Numero de filas mediante el componente textField
  {
    columns = 2
  }
  val numColTexto = new TextField    //Numero de columnas mediante el componente textField
  {
    columns = 2
  }

  //Utilizamos comboBox para que el usuario pueda seleccionar los distintos parametros con los que desea jugar la partida
  val dificultadCuadro = new ComboBox(List("Facil", "Dificil"))     //Usuario podra seleccionar la dificultad
  val modoJuegoCuadro = new ComboBox(List("Automatico", "Manual"))  //Seleccion del modo de juego

  val startButton = new Button("Start game")    //Boton para iniciar el juego
  {
    background = java.awt.Color.yellow
    reactions += {
      case ButtonClicked(_) =>

        //Obtenemos el numero de filas introducidas por el usuario en el textField y lo convertimos a Int
        val numFilas = numFilasTexto.text.toInt

        //Obtenemos el numero de columnas introducidas por el usuario en el textField y lo convertimos a Int
        val numCol = numColTexto.text.toInt

        //Obtenemos la dificultad introducida por el usuario del comboBox seleccionando el item elegido por el usuario
        val dificultad = dificultadCuadro.selection.item

        //Obtenemos el modo de juego elegido por el usuario seleccionando el item
        val modoJuego = modoJuegoCuadro.selection.item

        //Creamos una NUEVA VENTANA en la que mostraremos el tablero generado y le pasamos los parametros introducidos por el usuario
        val ventana3 = new ventanaTablero(numFilas, numCol, Main.conversionDificultad(dificultad), Main.conversionModoJuego(modoJuego))
        ventana3.visible = true
        dispose()      //Cierra y libera los recursos asociados a la ventana del menu
    }
  }

  contents = new BorderPanel    //Panel de diseño
  {
    layout(new GridPanel(4, 2)
    {
      //Rellenamos los datos del panel en la variable contents
      contents += new Label ("Numero de filas:") {foreground = java.awt.Color.RED}
      contents += numFilasTexto
      contents += new Label("Numero de columnas:") {foreground = java.awt.Color.RED}
      contents += numColTexto
      contents += new Label("Dificultad:") {foreground = java.awt.Color.RED}
      contents += dificultadCuadro
      contents += new Label("Modo de juego:") {foreground = java.awt.Color.RED}
      contents += modoJuegoCuadro

      override def paintComponent(g: Graphics2D) = {
        super.paintComponent(g)
        g.drawImage(fondo, 0, 0, null)    //Ponemos la imagen de fondo
      }

      preferredSize = new Dimension(320, 240)

    }) = Position.Center    //Grid Panel en la posicion centrar

    layout(startButton) = Position.South    //Establecemos un boton en la posicion de abajo
  }

  centerOnScreen()    //Centramos la pantalla
}

/**
 * Ventana que muestra el tablero del juego
 * @param numFilas
 * @param numCol
 * @param dificultad
 * @param modoJuego
 */
class ventanaTablero(numFilas: Int, numCol: Int, dificultad: Int, modoJuego: Char) extends MainFrame
{
  title = "TABLERO DEL JUEGO"       //Establecemos el titulo
  preferredSize = new Dimension((numCol + 1) * 24, (numFilas + 5) * 19)   //Establecemos las dimensiones del tablero

  //Inicializamos su valor
  val tablero: List[Int] = funcionesTablero.inicializarTablero(Nil, dificultad, numFilas * numCol)

  //Mostramos el tablero en la interfaz
  val panelTablero = new dibujarTablero(tablero, numFilas, numCol, dificultad, modoJuego)

  contents = new BoxPanel(Orientation.Vertical)     //Creamos un panel
  {
    contents += panelTablero    //Mostramos el tablero en el panel
  }
  panelTablero.visible = true

  centerOnScreen()
  visible = true

}

/**
 * Clase que se encarga de dibujar el tablero
 * @param tablero
 * @param numFilas
 * @param numCol
 * @param dificultad
 * @param modoJuego
 */
class dibujarTablero(tablero: List[Int], numFilas: Int, numCol: Int, dificultad: Int, modoJuego: Char) extends Component
{
  val (ancho, alto) = ((numCol + 1) * 25, (numFilas + 1) * 25)    //Calculamos el ancho y el alto del tablero

  var tableroPanel: List[Int] = tablero     //Creamos una instancia de tipo var del tablero
  
  funcionesTablero.mostrarTablero(tableroPanel, 0, numFilas, numCol)    //Llamamos al objeto funcionesTablero para mostrar el tablero

  override def paintComponent(g: Graphics2D): Unit =
  {
    new Label("Vidas: " + funcionesTablero.getVidas()) {foreground = java.awt.Color.RED; Position.North}
    super.paintComponent(g)

    preferredSize = new Dimension(ancho, alto)

    //Recorremos el tablero mediante un bucle for para rellenarlo con las imagenes que se corresponden con el valor asociado a cada casilla
    for
    {
      i <- 0 until numFilas
      j <- 0  until numCol
    }
    {
      g.setColor(Color.RED)
      g.drawString("Vidas: " + funcionesTablero.getVidas(), 10, alto + 15)

      val coordX = j * 25   //Calculamos la coordenada X (multiplicamos por 25 ya que es lo que ocupa la imagen de cada ficha)
      val coordY = i * 25   //Calculamos la coordenada Y (multiplicamos por 25 ya que es lo que ocupa la imagen de cada ficha)

      //DeterminaFicha nos va a devolver la ruta al archivo con la imagen asociada a la casilla de dicho tabllero
      val rutaImagen = new File(getImagen(determinarImagenFicha(tableroPanel(i * numCol + j))))    //Llamamos a determinarFicha
      val imagen = ImageIO.read(rutaImagen)      //Leemoos la val archivo que contiene la imagen asociad a al ficha de la casilla

      g.drawImage(imagen, coordX, coordY, 25, 25 , null)  //Dibujamos la imagen en la posicion del tablero

    }
  }

  /**
   * Funcion para coger una imagen
   * @param n
   * @return ruta a la imagen
   */
    def getImagen(n: Int): String = {
      val fichasNormales = Array("src/recursos/ficha1.PNG", "src/recursos/ficha2.PNG", "src/recursos/ficha3.PNG", "src/recursos/ficha4.PNG", "src/recursos/ficha5.PNG", "src/recursos/ficha6.PNG", "src/recursos/RC1.PNG", "src/recursos/RC2.PNG", "src/recursos/RC3.PNG", "src/recursos/RC4.PNG", "src/recursos/RC5.PNG", "src/recursos/RC6.PNG", "src/recursos/bomba.PNG", "src/recursos/TNT.PNG")
      fichasNormales(n)
    }

  /**
   * Devuelve la posicion en el array de la funcion getImagen en funcion del tipo de casilla que es
   * @param valor
   * @return posicion en el array de getImagen
   */
    def determinarImagenFicha(valor: Int): Int = {
      if(valor < 7)  valor - 1    //Color
      else if(valor < 14 && valor > 7) valor - 2  //RC
      else if(valor == 66) 12   //Bomba
      else if(valor == 84) 13   //TNT
      else throw new Error("ERROR")
    }

  /**
   * Si se pulsa una casilla del tablero
   * @param e
   */
  def mouseClicked(e: MouseEvent): Unit = {
    //Usuario pulsa una casilla
    val point = e.point             //Almacena las coordenadas del evento e
    val insets = peer.getInsets()   //Almacena los márgenes (insets) del componente
    val cellWidth = 25              //Tam de la imagen
    val cellHeight = 25             //Tam de la imagen
    val row = (point.getY() - insets.top) / cellHeight    //Calculamos el numero de fila de la coordenada Y pulsada por el usuario
    val col = (point.getX() - insets.left) / cellWidth    //Calculamos el numero de columna de la coordenada Y pulsada por el usuario

    //Comprobamos que la fila y col se encuentren dentro de las dimensiones del tablero
    if (row >= 0 && col >= 0 && row < numFilas && col < numCol)
    {
      //Modo de juego automatico --> obtenemos la mejor jugada
      if (modoJuego == 'a')
      {
        //Obtenemos la mejor casilla como jugada
        val pos_encontrar: Int = funcionesTablero.conseguirMejorJugada(tableroPanel, 0, numFilas, numCol, numFilas * numCol, dificultad, 0)
        println("Mejor posicon a borrar " + pos_encontrar)
        //Tras ello comenzamos la partida y recogemos el tablero
        tableroPanel = funcionesTablero.jugar(numFilas, numCol, dificultad, tableroPanel, modoJuego, pos_encontrar)
      }
      else    //Modo de juego Manual --> calculamos la casilla pulsada por el usuario
      {
        println(s"Se ha pulsado en la casilla (${row.toInt}, ${col.toInt})")    //Mostramos la casilla seleccionada por el usuario
        //Calculamos la posicion a encontrar para enviarsela a jugar
        val pos_encontrar: Int = row.toInt * numCol + col.toInt
        //Llamamos a la funcion jugar() del objeto funcionesTablero pasandole el tableroPanel
        tableroPanel = funcionesTablero.jugar(numFilas, numCol, dificultad, tableroPanel, modoJuego, pos_encontrar)
      }

      if(funcionesTablero.getVidas() == 0)
      {
        val ventanaF = new ventanaFinal()
        ventanaF.visible
        this.visible = false
      }
      else this.repaint() //Se repinta el componente por pantalla
    }
  }

  //Agrega el comportamiento de ratón
  listenTo(mouse.clicks)
  reactions += {
    case e: MouseClicked => mouseClicked(e)   //Llamamos a la funcion definida justo arriba
  }

  preferredSize = new Dimension(ancho, alto)  //Establecemos las dimensiones

}

class ventanaFinal() extends MainFrame
{
  title = "Pantalla Final"
  preferredSize = new Dimension(320,240)

  val fondo = ImageIO.read(new File("src/recursos/fondo.jpg"))

  val buttonOtraVez = new Button("Volver a jugar") //Establecemos el texto que se mostrará en el botón
  {
    background = java.awt.Color.yellow
    preferredSize = new Dimension(50, 20) //Dimension del boton
  }

  val buttonTerminar = new Button("Salir") //Establecemos el texto que se mostrará en el botón
  {
    background = java.awt.Color.yellow
    preferredSize = new Dimension(50, 20) //Dimension del boton
  }

  contents = new BorderPanel
  {
    layout(new Label("Has perdido :(") {foreground = java.awt.Color.RED}) = Position.Center

    layout(buttonOtraVez) = Position.North //Ponemos un boton jugar otra vez
    layout(buttonTerminar) = Position.South //Ponemos un boton terminar de jugar

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.drawImage(fondo, 0, 0, null)
    }
  }
  buttonTerminar.reactions += //Eventos del boton
    {
      case event.ButtonClicked(_) => //Si se pulsa el boton
        dispose() //Cierra y libera los recursos asociados a la ventana inicial
    }
  buttonOtraVez.reactions += //Eventos del boton
    {
      case event.ButtonClicked(_) => //Si se pulsa el boton
        funcionesTablero.reiniciarVidas()
        val ventana2 = new ventanaMenu //Se muestra el MENU
        ventana2.visible = true //Muestra la nueva ventana del menu
        dispose() //Cierra y libera los recursos asociados a la ventana inicial
    }

  centerOnScreen()
  visible = true

}

object Main
{
  def main(args: Array[String]): Unit = {
    val ventana1 = new ventanaInicial   //Instanciamos la ventana inicial
    ventana1.visible = true             //Establecemos su visibilidad
    println("Fin")
  }

  /**
   * Devuelve la dificul en tipo int
   * @param dificultadInterfaz
   * @return dificultad
   */
  def conversionDificultad(dificultadInterfaz :String):Int =
   {
     if(dificultadInterfaz == "Facil") 4
     else if(dificultadInterfaz == "Dificil") 6
     else -1
   }

  /**
   * Devuelve un char que establece el modo de juego
   * @param modoJuegoInterfaz
   * @return modoDeJuego (automatico o manual)
   */
  def conversionModoJuego(modoJuegoInterfaz: String): Char = {
    if (modoJuegoInterfaz == "Automatico") 'a'
    else if (modoJuegoInterfaz == "Manual") 'm'
    else 'f'
  }

}