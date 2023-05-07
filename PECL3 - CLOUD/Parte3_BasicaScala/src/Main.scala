import scala.util.Random
import java.time.LocalDate
import org.json.JSONObject

import java.io.{BufferedWriter, File, FileWriter, OutputStreamWriter, PrintWriter}
import java.net.{HttpURLConnection, URL}
import scala.io.Source



object Main {

  def main(args: Array[String]): Unit =
  {
    val vidas: Int = 5
    val inicioEjecucion = System.currentTimeMillis()

    if (obtenerLongitud(args) > 0) { //Si se ha llamado al programa por comandos
      val modoJuego: Array[Char] = args(0).toCharArray
      programaConsola(obtenerLongitud(args), modoJuego(0), args(1).toInt, args(2).toInt, args(3).toInt, vidas, inicioEjecucion)
    }
    else //Si no se ha llamado al programa por comandos
    {
      programaTeclado(vidas, inicioEjecucion)
    }
  }

  /**
   * Recibe los datos introducidos por el usuario por consola
   *
   * @param args
   * @param modoJuego
   * @param dificultad
   * @param numFilas
   * @param numCol
   * @param vidas
   */
  def programaConsola(args: Int, modoJuego: Int, dificultad: Int, numFilas: Int, numCol: Int, vidas: Int, inicioEjecucion : Long): Unit = {
    if (args == 4) {
      val limiteNum: Int = definirDificultad(dificultad) //Establecemos la dificultad del juego en 4 o 6, en funcion del parametro introducido por el usuario
      val tablero: List[Int] = inicializarTablero(Nil, limiteNum, numFilas * numCol) //Rellenamos el tablero con colores
      mostrarTablero(tablero, 0, numFilas, numCol)
      jugar(numFilas, numCol, limiteNum, tablero, modoJuego.toChar, vidas, 0, inicioEjecucion) //Llamamos a jugar, se encarga de comenzar la partida
    }
    else if (args < 4) throw new Error("Faltan argumentos en la llamada ")
    else if (args > 4) throw new Error("Sobran argumentos en la llamdad")
  }

  /**
   * Los datos de las variables son introducidos por el usuario a través del teclado
   * @param vidas
   */
  def programaTeclado(vidas: Int, inicioEjecucion : Long): Unit = {
    println("Introduce el numero de Filas del tablero: ")
    val numFilas: Int = scala.io.StdIn.readInt()

    println("Introduce el numero de columnas del tablero: ")
    val numCol: Int = scala.io.StdIn.readInt()

    println("Introduce la dificultad del juego: ")
    val dificultad: Int = scala.io.StdIn.readInt()

    println("Introduce el modo de juego (a o m): ")
    val modoJuego: Char = scala.io.StdIn.readChar()
    val size: Int = numFilas * numCol

    val limiteNum: Int = definirDificultad(dificultad)

    val tablero: List[Int] = inicializarTablero(Nil, limiteNum, size)
    mostrarTablero(tablero, 0, numFilas, numCol)
    jugar(numFilas, numCol, limiteNum, tablero, modoJuego, vidas, 0, inicioEjecucion)
  }

  def seleccionModoJuego(modoJuego: Char, numFilas: Int, numCol: Int, dificultad: Int, tablero: List[Int]): Int = {
    //Modo automatico
    if (modoJuego == 'a' || modoJuego == 'A')
    {
      //Devuelve la MEJOR casilla a utilizar, se corresponde con la posición que eliminará el mayor número de posiciones
      val pos: Int = conseguirMejorJugada(tablero, 0, numFilas, numCol, numFilas * numCol, dificultad, 0)
      println("Posicion optima a borrar " + pos)
      pos
    }
    else if (modoJuego == 'm' || modoJuego == 'M')  //Modo manual, el usuario inroduce las coordenadas
    {
      println("Introduce la coordenada X de la posicion a borrar : ")
      val coordX: Int = scala.io.StdIn.readInt()

      println("Introduce la coordenada Y de la posicion a borrar : ")
      val coordY: Int = scala.io.StdIn.readInt()

      coordX * numCol + coordY
    }
    else throw new Error("Modo de juego incorrecto")
  }

  /**
   * Funcion que calcula cual seria la mejor posicion a borrar (mayor longitud de camino)
   * @param tablero
   * @param pos
   * @param numFilas
   * @param numCol
   * @param size
   * @param dificultad
   * @param mejorPos
   * @return mejorPos
   */
  def conseguirMejorJugada(tablero: List[Int], pos: Int, numFilas: Int, numCol: Int, size : Int, dificultad: Int, mejorPos : Int): Int =
  {
    if(pos == size) mejorPos //Caso base
    else                      //Caso recursivo
    {
      if(!contiene(tablero, pos, size)) mejorPos  //Si se encuentra fuera de los limites del tablero
      else  //Si se encuentra dentro de los limites
      {
        //Calculamos la longitud del camino encontrado desde la posicion actual
        val longActual: Int = calcularLongitud(tablero, pos, numFilas, numCol, size, dificultad)

        //Calculamos la longitud del camino desde la mejor posicion encontrada
        val mejorLong: Int = calcularLongitud(tablero, mejorPos, numFilas, numCol, size, dificultad)

        if (contiene(tablero, pos + 1, size)) //Comprobamos si la siguiente posicion a la que avanzar se encuentra dentro de los limites del tablero
        {
          if (longActual > mejorLong)   //La posicion actual es mejor que la posicion encontrado hasta entonces
            conseguirMejorJugada(tablero, pos + 1, numFilas, numCol, size, dificultad, pos)       //Enviamos pos actual como mejor posicion encontrada
          else
            conseguirMejorJugada(tablero, pos + 1, numFilas, numCol, size, dificultad, mejorPos)  //Mantenemos la posicion encontrada anteriormente
        }
        else  //Si la siguiente posicion se encuentra fuera de los límites del tablero
        {
          if (longActual > mejorLong) pos
          else mejorPos
        }
      }
    }
  }

  /**
   * Devuelve longitud de un camino teniendo en cuenta si la posicion corresponde a un bloque especial
   * @param tablero
   * @param pos
   * @param numFilas
   * @param numCol
   * @param size
   * @param dificultad
   * @return longitud de un camino desde la pos
   */
  def calcularLongitud(tablero: List[Int], pos: Int, numFilas: Int, numCol: Int, size : Int, dificultad: Int): Int =
  {
    //Si posicion tiene un bloque especial
    if(tablero(pos) > 6)  contarPosicionesBorradas(determinarAccion(tablero, pos, size, tablero(pos), numFilas, numCol, dificultad)) //Calcula casillas que se borraran
    else contarPosicionesBorradas(encontrarCaminosAux(tablero, pos, 0, numFilas, numCol, size, tablero(pos), pos)) //Si no es un bloque especial calcula camino normal
  }

  /**
   * La recursividad del juego que se lleva a cabo hasta que se acaban las vidas del jugador
   * Esta función se encargará de llamar a todos los métodos necesarios para eliminar la casilla introducida por el usuario
   * @param numFilas
   * @param numCol
   * @param dificultad
   * @param tablero
   * @param modoJuego
   * @param vidas
   */
  def jugar(numFilas: Int, numCol: Int, dificultad: Int, tablero: List[Int], modoJuego: Char, vidas: Int, puntuacion: Int, inicioEjecucion : Long): Unit= {
    val size: Int = numCol * numFilas
    println("VIDAS " + vidas)
    vidas match
    {
      case 0 =>
      {
        //Obtiene la hora de finalización
        val end = System.currentTimeMillis()

        //Calcula la duracion de la partida en segundos
        val duracion = (end - inicioEjecucion) / 1000

        println("Has perdido")
        println("La duracion de la partida en segundos ha sido: " + duracion + " segundos")
        println("Puntuación Total: " + puntuacion)

        println("Cual es tu nickname?")
        val nombreUsuario = scala.io.StdIn.readLine()

        //Saca la fecha actual
        val fecha = LocalDate.now()

        //Codigo que manda datos a la página web (nombre, puntuación, fecha y duración)
        val url = new URL("https://express609921618.azurewebsites.net/inventory")
        val connection = url.openConnection().asInstanceOf[HttpURLConnection]

        connection.setRequestMethod("POST")
        connection.setDoOutput(true)

        //Hace objeto JSON con los datos a enviar
        val datos = new JSONObject()   //.put("id", JSONObject.NULL)
          .put("name", nombreUsuario)
          .put("quantity", puntuacion)
          .put("date", fecha)
          .put("duracion", duracion)
          .toString
        val bodyBytes = datos.getBytes("UTF-8")

        connection.setRequestProperty("Content-Type", "application/json")
        connection.setRequestProperty("Content-Length", bodyBytes.length.toString)

        val wr = new OutputStreamWriter(connection.getOutputStream())
        wr.write(datos)
        wr.flush()
        println(connection.getResponseCode(), connection.getResponseMessage)
        wr.close()

        println("Quieres jugar de nuevo? y/n")
        val otraVez = scala.io.StdIn.readChar()

        if(otraVez == 'y')
        {
          programaTeclado(5, System.currentTimeMillis())
        }
        else  println("Adios " + nombreUsuario + "!")
      }

      case _ =>
      {
        //Establece el modo de juego introducido por el usuario (Automatico o Manual) devolviéndonos la posición a encontrar
        val pos_encontrar : Int = seleccionModoJuego(modoJuego, numFilas, numCol, dificultad, tablero)
        //Obtiene el valor de la casilla con la que vamos a realizar la jugada
        val color: Int = getElem(pos_encontrar, tablero)

        //Funcion que determina la accion que se va a realizar en funcion del valor que tenga la casilla (Bomba, Rompecabezas, TNT o encontrar camino)
        val tablero2: List[Int] = determinarAccion (tablero, pos_encontrar, size , numFilas, numCol, color, dificultad)

        //Una vez que se ha realizado la accion, realizamos una comprobacion mediante restarVidas, para ver si hay que restar vidas al usuario
        val vida2: Int = restarVidas(tablero2, vidas, pos_encontrar) //Si tablero2 contiene casillas con el valor -1 significa que desde dicha posicion se han borrado elementos

        //Calcula la puntuacion de la jugada actual y se lo suma a la puntuacion de la partida
        val puntuacion2 = puntuacion + calcularPuntuacion(tablero2, tablero(pos_encontrar))
        println("Puntuacion " + puntuacion2)

        //Reemplazamos las posiciones del tablero eliminadas generando nuevos colores
        val tablero3: List[Int] = reemplazarPosiciones(0,tablero2, numFilas, numCol, dificultad)
        mostrarTablero(tablero3,0,numFilas, numCol)

        //Llamada recursiva a jugar, con el tablero y el numero de vidas actualizado
        jugar(numFilas, numCol, dificultad, tablero3, modoJuego, vida2, puntuacion2, inicioEjecucion)
      }
    }
  }

  /**
   * Funcion que determina si posicion seleccionada es un bloque especial o se busca camino de forma normal
   * @param tablero
   * @param pos_encontrar
   * @param size
   * @param numFilas
   * @param numCol
   * @param color
   * @param dificultad
   * @return tablero
   */
  def determinarAccion(tablero: List[Int], pos_encontrar: Int, size: Int, numFilas: Int, numCol: Int, color: Int, dificultad: Int): List[Int] = {
    //Comprobamos si se corresponde con un bloque especial (Bomba, TNT o RC)
    if (tablero(pos_encontrar) == 66) insertarElementoPosicion(-1, pos_encontrar, realizarAccionBomba(tablero, 0, pos_encontrar, size, numCol))
    else if (tablero(pos_encontrar) == 84) insertarElementoPosicion(-1, pos_encontrar, realizarAccionTNT(tablero, 0, pos_encontrar, size, numCol, numFilas))
    else if (tablero(pos_encontrar) > 7 && tablero(pos_encontrar) < 14) insertarElementoPosicion(-1, pos_encontrar, realizarAccionRompecabezas(tablero, 0, pos_encontrar, size))
    else //No es un bloque especial, por tanto buscamos todas las casillas adyacentes a la introducida para eliminarlas
    {
      //Encuentra todas las casillas adyacentes a la posición del tablero introducida por el usuario
      val tablero2: List[Int] = encontrarCaminosAux(tablero, pos_encontrar, 0, numFilas, numCol, size, color, pos_encontrar)
      //Una vez que hemos eliminado todas las posiciones comprobamos si se puede generar una bomba, un rompecabezas o un TNT
      val tablero3: List[Int] = encontrarBomba(tablero2, pos_encontrar, numFilas, numCol)
      val tablero4: List[Int] = encontrarRompecabezasTNT(tablero3, pos_encontrar, numFilas, numCol, dificultad)
      tablero4 //Se devuelve el tablero4 que se corresponde con el final tras haber aplicado las distintas posibilidades de acciones sobre el tablero
    }
  }

  def calcularPuntuacion(tablero: List[Int], valorSeleccionado: Int): Int= {
    val posBorradas = contarPosicionesBorradas(tablero) - 1
    val puntuacion = posBorradas + (posBorradas / 10) //Por cada 10 bloques borrados se suma uno

    if (valorSeleccionado == 66)  puntuacion + 5
    else if (valorSeleccionado == 84) puntuacion + 10
    else if(valorSeleccionado > 7 && valorSeleccionado < 14) puntuacion + 15
    else puntuacion + 1     //Si no es bloque especial se cuenta asi misma
  }

/**
 * Funcion que determina si se le tiene que borrar una vida al jugador
 * Comprueba si existen posiciones en tablero eliminadas, cuyo valor sera -1
 * @param tablero
 * @param vidas
 * @param pos_encontrar
 * @return vida restante en función del movimiento realizado
 */
def restarVidas(tablero: List[Int], vidas: Int, pos_encontrar: Int): Int = {
  //Llama a la función contarPosicionesBorradas que recorrera el tablero devolviendo el numero de posiciones borradas
  //En caso de que unicamente se haya borrado una posicion, significa que se corresponde con la casilla introducida por el
  //usuario, y que por tanto NO se ha borrado ninguna posicion
  if (contarPosicionesBorradas(tablero) == 1 && tablero(pos_encontrar) < 7) vidas - 1 //No se ha borrado ninguna casilla
  else vidas //Se han borrado casillas, mantenemos la vida del usuario
}

/**
 * Define la dificultad ya que el usuario introduce un 1 o un 2
 * @param dificultad
 * @return dificultad (puede ser o 4 o 6)
 */
def definirDificultad(dificultad: Int): Int = {
  if (dificultad == 1) 4
  else 6
}

/**
 * Generamos valores aleatorios para el tablero
 * @param tablero
 * @param dificultad
 * @param size
 * @return tablero inicializado en rango entre 1 y la dificultad enviada
 */
def inicializarTablero(tablero: List[Int], dificultad: Int, size: Int): List[Int] = {
  size match {
    case 0 => tablero
    case _ => {
      //Generamos el color aleatorio
      val random = new Random(System.nanoTime())
      val numeroAleatorio: Int = random.nextInt(dificultad) + 1 //Generamos un numero de forma aleatoria
      numeroAleatorio :: inicializarTablero(tablero, dificultad, size - 1) //Introducimos el color generado en el tablero
    }
  }
}

/**
 * Muestra el tablero por pantalla
 * @param l
 * @param fila
 * @param N
 * @param M
 */
def mostrarTablero(l: List[Int], fila: Int, N: Int, M: Int): Unit = {
  if (l == Nil) {
    println("\n--------------")
  }
  else {
    if (fila == 0) println("\n--------------")
    if (fila % M == 0) print("\n|")
    if (l.head < 14 && l.head > 7) print("RC" + l.head % 7 + "|") //Imprime bloque especial RC, son los numeros que se encuentran en un rango entre 7-13
    else if (l.head > 14) print(l.head.toChar + "|")
    else if (l.head < 7) print(l.head + "|")
    mostrarTablero(l.tail, fila + 1, N, M)
  }
}

/**
 * Se encarga de encontrar todos los posibles caminos desde la posición del tablero que se corresponde con las coordenadas
 * introducidas por el usuario. Busca todas sus posiciones adyacentes del mismo color y establece su valor a -1
 * @param tablero
 * @param pos_encontrar
 * @param pos se incrementa en cada llamada recursiva
 * @param N
 * @param M
 * @param size
 * @param color
 * @param pos_original
 * @return tablero con todos los posibles caminos desde la posicion puestos a -1
 */
def encontrarCaminosAux(tablero: List[Int], pos_encontrar: Int, pos: Int, N: Int, M: Int, size: Int, color: Int, pos_original: Int): List[Int] = {
  if (pos + 1 == size) tablero //Si se sale del rango del tablero estamos en el caso base, ya que no nos quedan más casillas por explorar
  else {
    val nuevo_tablero: List[Int] = encontrarCaminos(tablero, pos_encontrar, pos, N, M, size, color, pos_original)
    if (nuevo_tablero(pos) == -1) //Si la posición forma parte del camino comprobamos si podemos seguir avanzando
    {
      encontrarCaminosAux(encontrarCaminos(nuevo_tablero, pos, pos, N, M, size, color, pos_original), pos_encontrar, pos + 1, N, M, size, color, pos_original)
    }
    else {
      encontrarCaminosAux(nuevo_tablero, pos_encontrar, pos + 1, N, M, size, color, pos_original)
    }
  }
}

/**
 * Función que encuentra todos los caminos desde una posicion hasta todas las casillas adyacentes del mismo color
 * @param tablero
 * @param pos_encontrar
 * @param pos
 * @param N            (filas)
 * @param M            (columnas)
 * @param size         (filas*columnas)
 * @param color
 * @param pos_original (cuando ya no podemos movernos mas desde un camino volvemos a la posicion original para encontrar todos los posibles caminos desde ella)
 * @return tablero con posiciones adyacentes puestas a -1
 */
def encontrarCaminos(tablero: List[Int], pos_encontrar: Int, pos: Int, N: Int, M: Int, size: Int, color: Int, pos_original: Int): List[Int] = {

  if (pos < 0 || pos >= size || !contiene(tablero, pos, size)) //Caso base, nos encontramos fuera de las dimensiones del tablero
  {
    tablero
  }
  else //Caso recursivo
  {
    //Dada la posicion actual en la que nos encontramos del tablero calculamos su fila siguiente y anterior, y su columna siguiente y anterior
    //Se usara para controlar los rangos del tablero y no salirnos de sus dimensiones
    val fila_siguiente: Int = ((pos + M) / M)
    val fila_anterior: Int = ((pos - M) / M)
    val col_siguiente: Int = (pos + 1) % M
    val col_anterior: Int = (pos - 1) % M

    //Comprobamos si la posicion que nos llega es la posicion a encontrar, si es del color a buscar o 1 y que se encuentre dentro de los rangos del tablero
    if (contiene(tablero, pos, size) && pos == pos_encontrar && (tablero(pos) == color || tablero(pos) == -1)) {
      //Borramos la casilla que se corresponde con las pos actual ya que cumple con los requisitos de adyacencia
      val nuevo_tablero: List[Int] = insertarElementoPosicion(-1, pos, tablero)
      //Comprobamos si la posicion actual tiene posiciones adyacentes del mismo color:
      if (col_siguiente < M && contiene(tablero, pos + 1, size) && col_siguiente != 0 && tablero(pos + 1) == color) // Derecha
      {
        //Avanzamos a la derecha
        //Realizamos la llamada recursiva actualizando el valor de la posicion a encontrar y la posicion actual al de la casilla a la derecha y asi ver si dicha casilla tambien tiene mas caminos adyacentes
        insertarElementoPosicion(-1, pos + 1, encontrarCaminos(nuevo_tablero, pos + 1, pos + 1, N, M, size, color, pos_original))
      }
      else if (col_anterior >= 0 && contiene(tablero, pos - 1, size) && col_anterior != M - 1 && tablero(pos - 1) == color) // Izquierda
      {
        //Avanzamos a la izquierda
        //Realizamos la llamada recursiva actualizando el valor de la posicion a encontrar y la posicion actual al de la casilla a la izquierda y asi ver si dicha casilla tambien tiene mas caminos adyacentes
        insertarElementoPosicion(-1, pos - 1, encontrarCaminos(nuevo_tablero, pos - 1, pos - 1, N, M, size, color, pos_original))
      }
      else if (fila_siguiente < N && contiene(tablero, pos + M, size) && fila_siguiente != 0 && tablero(pos + M) == color) // Abajo
      {
        //Avanzamos hacia abajo y eliminamos la posicion de abajo del tablero poniéndola a -1
        //Realizamos la llamada recursiva actualizando el valor de la posicion a encontrar y la posicion actual al de la casilla de abajo y asi ver si dicha casilla tambien tiene mas caminos adyacentes
        insertarElementoPosicion(-1, pos + M, encontrarCaminos(nuevo_tablero, pos + M, pos + M, N, M, size, color, pos_original))
      }
      else if (fila_anterior >= 0 && contiene(tablero, pos - M, size) && fila_anterior != N - 1 && tablero(pos - M) == color) // Arriba
      {
        //Avanzamos arriba
        //Realizamos la llamada recursiva actualizando el valor de la posicion a encontrar y la posicion actual al de la casilla de arriba y asi ver si dicha casilla tambien tiene mas caminos adyacentes
        insertarElementoPosicion(-1, pos - M, encontrarCaminos(nuevo_tablero, pos - M, pos - M, N, M, size, color, pos_original))
      }
      else //Si ninguna posicion de su alrededor es adyacente
      {
        //Decrementamos el tamaño del tablero y pasamos la posicion_original desde la que aun faltan mas caminos por recorrer, ya que desde el camino que ha entrado no tiene más opciones
        encontrarCaminos(nuevo_tablero, pos_original, pos_original, N, M, size - 1, color, pos_original)
      }
    }
    else //Si no cumple con uno de los requisitos del if, decrementamos la posicion del tablero
    {
      encontrarCaminos(tablero, pos_original, pos_original, N, M, size - 1, color, pos_original)
    }
  }
}

/**
 * Reemplaza las posiciones del tablero que han sido eliminadas
 * @param pos
 * @param tablero
 * @param N
 * @param M
 * @param dificultad
 * @return tablero con nuevos colores
 */
def reemplazarPosiciones(pos:Int, tablero:List[Int], N:Int, M:Int, dificultad:Int): List[Int] = {
  //Obtiene el numero de posiciones que aún siguen sin tener un color asignado del tablero
  val contador:Int = contarPosicionesBorradas(tablero)

  contador match
  {
    case 0 => tablero //Caso base
    case _ => {       //Caso recursivo
      //Llamamos a la funcion auxiliar
      val nuevo:List[Int] = reemplazarAux(0, tablero, N, M, dificultad, contador, N*M)
      //Si aún no se han reemplazado todas sus posiciones devovelvera un numero mayor que 0
      if (contarPosicionesBorradas(nuevo) > 0)
      {
        reemplazarPosiciones(0, nuevo, N, M, dificultad)  //LLamada recursiva
      }
      else
      {
        nuevo
      }
    }
  }
}

/**
 * Se sustituyen las posiciones marcadas por -1 por el valor de la casilla de arriba
 * si es que tiene casilla de arriba (no es fila 0) o si la posición de arriba no es -1.
 * @param pos
 * @param tablero
 * @param N
 * @param M
 * @param dificultad
 * @param contador
 * @param size
 * @return
 */
def reemplazarAux(pos: Int, tablero: List[Int], N: Int, M: Int, dificultad: Int, contador: Int, size:Int): List[Int] = {
  if(pos >= size) tablero //Caso base
  else  //Caso recursivo
  {
    if (tablero(pos) == -1)   //Si la posicion que llega esta eliminada
    {
      val filaActual: Int = pos / M;  //Calculamos la fila de la posicion actual
      val colActual: Int = pos % M;

      //Comprobamos que su POSICIÓN de ARRIBA NO esté ELIMINADA y que la posicion esté DENTRO de las DIMENSIONES del tablero
      if (contiene(tablero, pos - M, N * M) && pos - M >= 0 && filaActual > 0 && filaActual <= N && tablero(pos - M) != -1) //Si la posicion de arriba es distinta de -1 se la asignamos a la posicion que nos llega y se la quitamos a la de arriba
      {
        //Obtenemos el color de la posicion de arriba del tablero
        val elem: Int = tablero(pos - M)

        //Insertamos un -1 en la posicion de arriba del tablero
        val nuevo: List[Int] = insertarElementoPosicion(-1, pos-M, tablero)

        //Inserta el color en la posicion actual, continuamos realizando la llamada recursiva pasando a la siguiente posicion
        insertarElementoPosicion(elem, pos, reemplazarAux(pos + 1, nuevo, N, M, dificultad, contador, size))  //Recursividad
      }
      else if (filaActual == 0) //Si estas en la fila 0, no puedes coger el color de la posicion de arriba, tienes que generar uno
      {
        //Generamos color aleatorio
        val random = new Random(System.nanoTime())
        val color: Int = random.nextInt(dificultad) + 1
        //Introducimos el color en la posicion actual y continuamos con la llamada recursiva
        insertarElementoPosicion(color, pos, reemplazarAux(pos + 1, tablero, N, M, dificultad, contador, size))
      }
      else //La posicion de arriba sigue valiendo -1, hay que esperar a que tenga un color asignado antes de obtenerlo
      {
        //Avanzamos a la siguiente posicion del tablero
        reemplazarAux(pos + 1, tablero, N, M, dificultad, contador, size)
      }
    }
    else //Nos encontramos en una posicion que NO ha sido ELIMINADA
    {
      reemplazarAux(pos + 1, tablero, N, M, dificultad, contador, size)
    }
  }
}


/**
 * Comprueba si se puede generar una bomba en el tablero
 * @param tablero
 * @param pos_encontrar
 * @param N
 * @param M
 * @return tablero con bomba generada en caso de que uno de los contadores de filas y columnas sea igual a 5
 */
def encontrarBomba(tablero: List[Int], pos_encontrar: Int, N: Int, M:Int): List[Int] = {
  //Contador de columna
  val numCol: Int = pos_encontrar % M;
  //Obtiene una lista con los elementos de la columna buscada
  val listaCol: List[Int] = getColumna(0, tablero, numCol, M)
  //Recibe una lista con los elementos de la columna y cuenta el numero de apariciones puestas a -1 para ver si son 5, en cuyo caso se genererá una bomba
  val contCol:Int = contarPosicionesBorradas(listaCol)

  //Contador de fila
  val numFila: Int = pos_encontrar / M;
  //Obtiene una lista con los elementos de la fila buscada
  val listaFila: List[Int] = getFila(numFila, tablero, M)
  //Recibe una lista con los elementos de la fila y devuelve un contador el numero de apariciones puestas a -1 para ver si son 5, en cuyo caso se genererá una bomba
  val contFila:Int = contarPosicionesBorradas(listaFila)

  //Comparamos los valores de los contadores
  if (contCol != contFila)
  {
    //Insertamos las bombas en caso de que sea posible
    if(contCol == 5) insertarElementoPosicion(66, pos_encontrar, tablero)
    else if (contFila == 5) insertarElementoPosicion(66, pos_encontrar, tablero)
    else tablero
  }
  else
  {
    tablero
  }
}

/**
 * Determina si se crea un bloque especial de Rompecabezas o TNT, devolviendo el tablero con el bloque especial asignado
 * @param tablero
 * @param pos_encontrar
 * @param N
 * @param M
 * @param dificultad
 * @return tablero
 */
def encontrarRompecabezasTNT(tablero:List[Int], pos_encontrar:Int, N:Int, M:Int, dificultad:Int):List[Int] =
{
  //Obtiene el numero de posiciones borradas del tablero
  val cont:Int = contarPosicionesBorradas(tablero)

  if (cont == 6) //Si el indice vale 6 es el TNT
  {
    println("Encuentra TNT")
    insertarElementoPosicion(84, pos_encontrar, tablero)
  }
  else if (cont >= 7) //Si el indice es mayor de o igual de 7 introducimos un RC
  {
    println("Encuentra rompecabezas")
    val color: Int = Random.nextInt(dificultad) + 1
    insertarElementoPosicion(7 + color, pos_encontrar, tablero)
  }
  else
  {
    tablero
  }
}



/**
 * Funcion que lleva a cabo la accion del rompecabezas de eliminar todas las casillas que tengan su color
 * @param tablero
 * @param pos
 * @param pos_encontrar
 * @param size
 * @return tablero con las casillas del color del RC eliminadas
 */
def realizarAccionRompecabezas(tablero:List[Int], pos:Int, pos_encontrar:Int, size:Int): List[Int] =
{
  //Calculamos el color a borrar del tablero
  val colorBorrar: Int = tablero(pos_encontrar) % 7
  if (pos >= size) insertarElementoPosicion(-1, pos_encontrar, tablero) //Caso base
  else { //Caso recursivo
    //Si la posición tiene el color a borrar introducimos un -1 en dicha posicion
    if (tablero(pos) == colorBorrar) insertarElementoPosicion(-1, pos, realizarAccionRompecabezas(tablero, pos + 1, pos_encontrar, size))
    else insertarElementoPosicion(tablero(pos), pos, realizarAccionRompecabezas(tablero, pos + 1, pos_encontrar, size))
  }
}


/**
 * Funcion que lleva a cabo la accion de la bomba de eliminar todas las casillas de su misma fila y columna
 * @param tablero
 * @param pos
 * @param pos_encontrar
 * @param size
 * @param numCol
 * @return tablero tras explotar una bomba
 */
def realizarAccionBomba(tablero: List[Int], pos: Int, pos_encontrar: Int, size: Int, numCol: Int): List[Int] =
{
  if(pos >= size) insertarElementoPosicion(-1, pos_encontrar, tablero)  //Caso base
  else  //Caso recursivo
  {
    val filaActual: Int = pos / numCol
    val colActual : Int = pos % numCol

    //Numero de fila y columna a eliminar
    val filaBorrar: Int = pos_encontrar / numCol
    val colBorrar: Int = pos_encontrar % numCol

    //Si la fila o columna de la posicion actual se corresponde con la de fila y columna a borrar se elimina dicha casilla del tablero
    if(filaActual == filaBorrar || colActual == colBorrar)  insertarElementoPosicion(-1, pos, realizarAccionBomba(tablero, pos + 1, pos_encontrar, size, numCol))
    else insertarElementoPosicion(tablero(pos), pos, realizarAccionBomba(tablero, pos + 1, pos_encontrar, size, numCol))
  }
}




/**
 * Funcion que lleva a cabo la accion del TNT de eliminar todas las casillas en un radio de 4 posiciones
 * @param tablero
 * @param pos
 * @param pos_encontrar
 * @param size
 * @param numCol
 * @param numFilas
 * @return tablero casillas eliminadas
 */
def realizarAccionTNT(tablero: List[Int], pos: Int, pos_encontrar: Int, size: Int, numCol: Int, numFilas : Int): List[Int] =
{
  if(pos >= size) insertarElementoPosicion(-1, pos_encontrar, tablero)
  else
  {
    val filaActual: Int = pos / numCol
    val colActual: Int = pos % numCol

    //Calculamos los limites dentro del radio 4 de casillas que va a explotar el TNT
    val limiteDerecho: Int = if((colActual + 4) < numCol) colActual + 4 else numCol - 1
    val limiteIzquierdo: Int = if((colActual - 4) < 0) 0 else filaActual - 4
    val limiteArriba: Int = if((filaActual - 4) < 0) 0 else colActual - 4
    val limiteAbajo: Int = if((filaActual + 4) < numFilas) filaActual + 4 else numFilas - 1

    //Si la posicion se encuentra dentro del radio, se elimina
    if (colActual < limiteDerecho && colActual > limiteIzquierdo && filaActual < limiteAbajo && filaActual > limiteArriba)
      insertarElementoPosicion(-1, pos, realizarAccionTNT(tablero, pos + 1, pos_encontrar, size, numCol, numFilas))
    else //La posicion esta FUERA del radio
      insertarElementoPosicion(tablero(pos), pos, realizarAccionTNT(tablero, pos + 1, pos_encontrar, size, numCol, numFilas))
  }
}


/**
 * Calcula el numero de apariciones de el valor -1 (que se corresponde con una posición eliminada)
 * @param listaFila
 * @return contador
 */
def contarPosicionesBorradas(listaFila: List[Int]): Int = {
  listaFila match {
    case Nil => 0
    case head :: Nil => {
      if (head == -1) 1 //Si la cabeza es -1 esa posicion ha sido borrada, devolvemos un 1 para la suma de las posiciones borradas
      else 0
    }
    case head :: tail => {
      if (head == -1) 1 + contarPosicionesBorradas(tail) //Si la cabeza es -1 esa posicion ha sido borrada
      else 0 + contarPosicionesBorradas(tail) //En cualquier otro caso la posicion no ha sido eliminada
    }
  }
}

/**
 * Comprueba si la posicion esta dentro del rango del tablero
 * @param tablero
 * @param pos
 * @param size
 * @return true o false
 */
def contiene(tablero: List[Int], pos: Int, size: Int): Boolean = {
  if (pos < 0) false
  else if (pos >= size) false
  else true
}


/**
 * Obtiene elemento que se encuentra en la posicion index
 * @param index
 * @param matriz
 * @return elemento que se encuentra en la posicion index
 */
def getElem(index: Int, matriz: List[Int]): Int = matriz match {
  case Nil => -1
  case head :: Nil =>
    head
  case head :: tail =>
    if (index < 0) -1
    else if (index == 0) head //Si el index es 0 estamos en la posición buscada
    else getElem(index - 1, tail)
}

/**
 * Inserta un elemento en la pos dada una lista
 * @param e
 * @param pos
 * @param lista
 * @return lista con el elemento insertado en pos
 */
def insertarElementoPosicion(e: Int, pos: Int, lista: List[Int]): List[Int] = {
  lista match {
    case Nil => e :: Nil //Si la lista esta vacia da igual ccuando insertarlo
    case _ => pos match {
      case 0 => e :: lista.tail //Es la cabeza de la lista
      case _ => lista.head :: insertarElementoPosicion(e, (pos - 1), lista.tail) //Pos - 1 ya que nuestro base es buscar caso 0
    }
  }
}

/**
 * Obtiene los valores de la columna numero index
 * @param index
 * @param matriz
 * @param col
 * @param numCol
 * @return lista con los elementos de la columna
 */
def getColumna(index: Int, matriz: List[Int], col: Int, numCol: Int): List[Int] = {
  matriz match {
    case Nil => Nil
    case head :: Nil => {
      if (index % numCol == col) head :: Nil //El elemento forma parte de la columna buscada
      else Nil
    }
    case head :: tail => {
      if (index % numCol == col) head :: getColumna(index + 1, tail, col, numCol) //El elemento forma parte de la columna buscada
      else getColumna(index + 1, tail, col, numCol)
    }
  }
}

/**
 * Funcion que calcula la longitud de un array
 * @param array
 * @return longitud
 */
def obtenerLongitud(array: Array[String]): Int = {
  array match {
    case Array() => 0
    case _ => 1 + obtenerLongitud(array.tail)
  }
}

/**
 * Obtiene los valores de la fila, utilizado para encontrar bomba
 * @param fila
 * @param matriz
 * @param N
 * @return lista con los elementos de la fila
 */
def getFila(fila: Int, matriz: List[Int], N: Int): List[Int] = fila match {
  case 0 => toma(N, matriz) //Si nos encontramos en la primera fila
  case _ => getFila((fila - 1), dejar(matriz.length - N, matriz), N)
}

/**
 * Saca los n primeros elementos de la lista
 * @param n
 * @param l
 * @return lista
 */
def toma(n: Int, l: List[Int]): List[Int] = l match {
  case Nil => Nil
  case head :: Nil => head :: Nil
  case head :: tail =>
    if (n <= 0) Nil
    else head :: toma(n - 1, tail)
}

/**
 * Saca los n ultimos elementos de la lista
 * @param n
 * @param l
 * @return lista
 */
def dejar(n: Int, l: List[Int]): List[Int] = l match {
  case Nil => Nil
  case head :: Nil => head :: Nil
  case head :: tail =>
    if (tail.length <= n - 1) head :: dejar(n, tail) //Si la longitud de N es mayor o igual que el numero de elementos restantes concatenamos los elementos
    else dejar(n, tail)
}

}

