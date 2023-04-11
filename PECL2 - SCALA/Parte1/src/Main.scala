
import Main.contarPosicionesBorradas

import java.security.SecureRandom
import scala.math.ceil
import scala.util.Random

object Main {

  def main(args: Array[String]): Unit =
  {
    val vidas : Int = 5
    if (obtenerLongitud(args) > 0) {  //Si se ha llamado al programa por comandos
      val modoJuego: Array[Char] = args(0).toCharArray
      programaConsola(obtenerLongitud(args), modoJuego(0), args(1).toInt, args(2).toInt, args(3).toInt, vidas)
    }
    else  //Si no se ha llamado al programa por comandos
    {
      programaTeclado(vidas)
    }
  }

  /**
   * Recibe los datos introducidos por el usuario por consola
   * @param args
   * @param modoJuego
   * @param dificultad
   * @param numFilas
   * @param numCol
   * @param vidas
   */
  def programaConsola(args:Int, modoJuego:Int, dificultad:Int, numFilas:Int, numCol:Int, vidas:Int): Unit = {
    if (args == 5) {
      println("Modo juego " + modoJuego)
      val limiteNum: Int = definirDificultad(dificultad)
      val tablero: List[Int] = inicializarTablero(Nil, limiteNum, numFilas*numCol)
      mostrarTablero(tablero, 0, numFilas, numCol)
      seleccionModoJuego(modoJuego.toChar, numFilas, numCol, limiteNum, tablero, vidas)
    }
    else if (args < 5) throw new Error("Faltan argumentos en la llamada ")
    else if (args > 5) throw new Error("Sobran argumentos en la llamdad")
  }

  /**
   * Los datos de las variables son introducidos por el usuario a través del teclado
   * @param vidas
   */
  def programaTeclado(vidas:Int): Unit = {
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
    seleccionModoJuego(modoJuego, numFilas, numCol, limiteNum, tablero, vidas)
  }

  /**
   * Automatico o manual, en cada caso se llamará a una función distinta
   * @param modoJuego
   * @param numFilas
   * @param numCol
   * @param dificultad
   * @param tablero
   * @param vidas
   */
  def seleccionModoJuego(modoJuego: Char, numFilas: Int, numCol: Int, dificultad: Int, tablero: List[Int], vidas: Int): Unit = {
    if (modoJuego == 'a' || modoJuego == 'A') jugarAutomatico(numFilas, numCol, dificultad, modoJuego, tablero, vidas)
    else if (modoJuego == 'm' || modoJuego == 'M') jugarManual(numFilas, numCol, dificultad, modoJuego, tablero, vidas)
    else throw new Error("Modo de juego incorrecto")
  }

  /**
   * Genera coordenas de forma aleatoria
   * @param numFilas
   * @param numCol
   * @param dificultad
   * @param modoJuego
   * @param tablero
   * @param vidas
   */
  def jugarAutomatico(numFilas: Int, numCol: Int, dificultad: Int, modoJuego: Char, tablero: List[Int], vidas : Int): Unit = {
    val random = new Random(System.nanoTime())
    val coordX: Int = random.nextInt(numFilas)
    val coordY: Int = random.nextInt(numCol)
    println("Coordenada x = " + coordX + " Coordenada Y = " + coordY)

    jugar(numFilas, numCol, dificultad, tablero, modoJuego, coordX, coordY, vidas)
  }

  /**
   * Usuario introduce las coordenadas de forma manual por teclado
   * @param numFilas
   * @param numCol
   * @param dificultad
   * @param modoJuego
   * @param tablero
   * @param vidas
   */
  def jugarManual(numFilas: Int, numCol: Int, dificultad: Int, modoJuego: Char, tablero: List[Int], vidas: Int): Unit = {
    println("Introduce la coordenada X de la posicion a borrar : ")
    val coordX: Int = scala.io.StdIn.readInt()

    println("Introduce la coordenada Y de la posicion a borrar : ")
    val coordY: Int = scala.io.StdIn.readInt()

    jugar(numFilas, numCol, dificultad, tablero, modoJuego, coordX, coordY, vidas)
  }

  /**
   * La recursividad del juego que se lleva a cabo hasta que se acaban las vidas del jugador
   * @param numFilas
   * @param numCol
   * @param dificultad
   * @param tablero
   * @param modoJuego
   * @param coordX
   * @param coordY
   * @param vidas
   */
  def jugar(numFilas: Int, numCol: Int, dificultad: Int, tablero: List[Int], modoJuego: Char, coordX: Int, coordY: Int, vidas: Int): Unit = {
    val size: Int = numCol * numFilas
    println("VIDAS " + vidas)
    vidas match
    {
      case 0 => println("Has perdido")
      case _ =>
      {
        val pos_encontrar: Int = coordX * numCol + coordY
        val color: Int = getElem(pos_encontrar, tablero)

        println("Longitud del camino = " + contarPosicionesBorradas(encontrarCaminosAux(tablero, pos_encontrar, 0, numFilas, numCol, size, tablero(pos_encontrar), pos_encontrar)))
        val tablero2: List[Int] = determinarAccion (tablero, pos_encontrar, size , numFilas, numCol, color, dificultad)

        val tablero3: List[Int] = reemplazarPosiciones(0,tablero2, numFilas, numCol, dificultad)
        mostrarTablero(tablero3,0,numFilas, numCol)

        seleccionModoJuego(modoJuego, numFilas, numCol, dificultad, tablero3, restarVidas(tablero2, vidas, pos_encontrar))
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
   * @return tablero resultante de aplicar todas las acciones posibles desde la posicion de las coordenadas
   */
  def determinarAccion(tablero : List[Int], pos_encontrar: Int, size : Int, numFilas: Int, numCol: Int, color: Int, dificultad: Int): List[Int] =
  {
    //Comprobamos si se corresponde con un bloque especial
    if(tablero(pos_encontrar) == 66)  insertarElementoPosicion(-1, pos_encontrar, realizarAccionBomba(tablero, 0, pos_encontrar, size, numCol))
    else if(tablero(pos_encontrar) == 84) insertarElementoPosicion(-1, pos_encontrar, realizarAccionTNT(tablero, 0, pos_encontrar, size, numCol, numFilas))
    else if(tablero(pos_encontrar) > 7 && tablero(pos_encontrar) < 14)  insertarElementoPosicion(-1, pos_encontrar, realizarAccionRompecabezas(tablero, 0, pos_encontrar, size))
    else  //No es un bloque especial
    {
      val tablero2: List[Int] = encontrarCaminosAux(tablero, pos_encontrar, 0, numFilas, numCol, size, color, pos_encontrar)
      val tablero3: List[Int] = encontrarBomba(tablero2, pos_encontrar, numFilas, numCol)
      val tablero4: List[Int] = encontrarRompecabezasTNT(tablero3, pos_encontrar, numFilas, numCol, dificultad)
      tablero4  //Se devuelve el tablero4 que se corresponde con el final tras haber aplicado las distintas posibilidades de acciones sobre el tablero
    }
  }

  /**
   * Funcion que determina si se le tiene que borrar una vida al jugador
   * @param tablero
   * @param vidas
   * @param pos_encontrar
   * @return vida restante en función del movimiento realizado
   */
  def restarVidas(tablero: List[Int], vidas: Int, pos_encontrar:Int): Int =
  {
    if (contarPosicionesBorradas(tablero) == 1 && tablero(pos_encontrar) < 7 )
    {
      vidas - 1
    }
    else vidas
  }

  /**
   * Define la dificultad
   * @param dificultad
   * @return dificultad (puede ser o 4 o 6)
   */
  def definirDificultad(dificultad: Int): Int =
  {
    if (dificultad == 1) 4
    else 6
  }

  /**
   *
   * @param tablero
   * @param dificultad
   * @param size
   * @return tablero inicializado en rango entre 1 y la dificultad enviada
   */
  def inicializarTablero(tablero: List[Int], dificultad: Int, size: Int): List[Int] = {
    size match {
      case 0 => tablero
      case _ => {
        val random = new Random(System.nanoTime())
        val numeroAleatorio: Int = random.nextInt(dificultad) + 1
        numeroAleatorio :: inicializarTablero(tablero, dificultad, size - 1)
      }
    }
  }

  def mostrarTablero(l: List[Int], fila: Int, N:Int, M:Int): Unit = {
      if (l == Nil)
      {
         println("\n--------------")
      }
      else
      {
        if(fila == 0) println("\n--------------")
        if (fila % M == 0) print("\n|")
        if(l.head < 14 && l.head > 7 )  print("RC" + l.head % 7 + "|")    //Imprime bloque especial RC
        else if (l.head > 14) print(l.head.toChar + "|")
        else if(l.head < 7) print(l.head + "|")
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
  def encontrarCaminosAux(tablero: List[Int], pos_encontrar: Int, pos: Int, N: Int, M: Int, size: Int, color: Int, pos_original: Int): List[Int] =
  {
    if(pos + 1 == size) tablero   //Si se sale del rango del tablero estamos en el caso base, ya que no nos quedan más casillas por explorar
    else
    {
      val nuevo_tablero: List[Int] = encontrarCaminos(tablero, pos_encontrar, pos, N, M, size, color, pos_original)
      if(nuevo_tablero(pos) == -1)  //Si la siguiente posición a la que se pasa forma parte del camino comprobamos si podemos seguir avanzando
      {
        encontrarCaminosAux(encontrarCaminos(nuevo_tablero, pos, pos, N, M, size, color, pos_original), pos_encontrar, pos + 1, N, M, size, color, pos_original)
      }
      else
      {
        encontrarCaminosAux(nuevo_tablero, pos_encontrar, pos + 1, N, M, size, color, pos_original)
      }
    }
  }

  /**
   * Función que encuentra todos los caminos desde una posicion hasta todas las casillas adyacentes del mismo color
   * @param tablero
   * @param pos_encontrar
   * @param pos
   * @param N (filas)
   * @param M (columnas)
   * @param size (filas*columnas)
   * @param color
   * @param pos_original (cuando ya no podemos movernos mas desde un camino volvemos a la posicion original para encontrar todos los posibles caminos desde ella)
   * @return tablero con posiciones adyacentes puestas a -1
   */
  def encontrarCaminos(tablero: List[Int], pos_encontrar: Int, pos: Int, N: Int, M: Int, size: Int, color: Int, pos_original: Int): List[Int] = {
    if (pos < 0 || pos >= size || !contiene(tablero, pos, size))
    {
      tablero
    }
    else
    {
      val fila_siguiente: Int = ((pos + M) / M)
      val fila_anterior: Int = ((pos - M) / M)

      val col_siguiente: Int = (pos + 1) % M
      val col_anterior: Int =  (pos - 1) % M

      if (contiene(tablero, pos, size) && pos == pos_encontrar && (tablero(pos) == color || tablero(pos) == -1))
      {
        val nuevo_tablero: List[Int] = insertarElementoPosicion(-1, pos, tablero)

        if (col_siguiente < M && contiene(tablero, pos + 1, size) && col_siguiente != 0 && tablero(pos + 1) == color) // Derecha
        {
          insertarElementoPosicion(-1, pos + 1, encontrarCaminos(nuevo_tablero, pos + 1, pos + 1, N, M, size, color, pos_original))
        }
        else if (col_anterior >= 0 && contiene(tablero, pos - 1, size) && col_anterior != M - 1 && tablero(pos - 1) == color) // Izquierda
        {
          insertarElementoPosicion(-1, pos - 1, encontrarCaminos(nuevo_tablero, pos - 1, pos - 1, N, M, size, color, pos_original))
        }
        else if (fila_siguiente < N && contiene(tablero, pos + M, size) && fila_siguiente != 0 && tablero(pos + M) == color) // Abajo
        {
          insertarElementoPosicion(-1, pos + M, encontrarCaminos(nuevo_tablero, pos + M, pos + M, N, M, size, color, pos_original))
        }
        else if (fila_anterior >= 0 && contiene(tablero, pos - M, size) && fila_anterior != N - 1 && tablero(pos - M) == color) // Arriba
        {
          insertarElementoPosicion(-1, pos - M, encontrarCaminos(nuevo_tablero, pos - M, pos - M, N, M, size, color, pos_original))
        }
        else      //Si ninguna posicion de su alrededor es adyacente
        {
          encontrarCaminos(nuevo_tablero, pos_original, pos_original, N, M, size - 1, color, pos_original)
        }
      }
      else
      {
        encontrarCaminos(tablero, pos_original, pos_original, N, M, size - 1, color, pos_original)
      }
    }
  }


  /**
   * Reemplaza las posiciones del tablero que han sido eliminadas
   * Llamada recursiva al metodo auxiliar de reemplazar posiciones, el cual nos devolverá un contador igual a 0
   * cuando todas las posiciones del tablero hayan sido reemplazadas y no queden valores a -1.
   * @param pos
   * @param tablero
   * @param N
   * @param M
   * @param dificultad
   * @return tablero con nuevos colores generados en las posiciones eliminadas
   */
  def reemplazarPosiciones(pos:Int, tablero:List[Int], N:Int, M:Int, dificultad:Int): List[Int] = {
    val contador:Int = contarPosicionesBorradas(tablero)
    contador match
    {
      case 0 => tablero
      case _ =>
      {
        val nuevo:List[Int] = reemplazarAux(0, tablero, N, M, dificultad, contador, N*M)
        if (contarPosicionesBorradas(nuevo) > 0) reemplazarPosiciones(0, nuevo, N, M, dificultad)
        else nuevo
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
   * @return tablero con posiciones reemplazadas
   */
  def reemplazarAux(pos: Int, tablero: List[Int], N: Int, M: Int, dificultad: Int, contador: Int, size:Int): List[Int] = {
    size match
    {
      case 0 => tablero
      case _ =>
      {
        if (tablero(pos) == -1)
        {
          val filaActual: Int = pos / M;

          if (contiene(tablero, pos - M, N * M) && pos - M >= 0 && filaActual > 0 && filaActual <= N && tablero(pos - M) != -1) //Si la posicion de arriba es distinta de -1 se la asignamos a la posicion que nos llega y se la quitamos a la de arriba
          {
            val elem: Int = tablero(pos - M)
            val nuevo: List[Int] = insertarElementoPosicion(-1, pos-M, tablero)
            insertarElementoPosicion(elem, pos, reemplazarAux(pos + 1, nuevo, N, M, dificultad, contador, size - 1))
          }
          else if (filaActual == 0) //Si estas en la fila 0
          {
            val random = new Random(System.nanoTime())
            val color: Int = random.nextInt(dificultad) + 1
            insertarElementoPosicion(color, pos, reemplazarAux(pos + 1, tablero, N, M, dificultad, contador, size - 1))
          }
          else
          {
            reemplazarAux(pos + 1, tablero, N, M, dificultad, contador, size - 1)
          }
        }
        else
        {
          reemplazarAux(pos + 1, tablero, N, M, dificultad, contador, size - 1)
        }
      }
    }
  }

  /**
   *
   * @param tablero
   * @param pos_encontrar
   * @param N
   * @param M
   * @return tablero con bomba generada en caso de que uno de los contadores de filas y columnas sea igual a 5
   */
  def encontrarBomba(tablero: List[Int], pos_encontrar: Int, N: Int, M:Int): List[Int] = {
    //Contador de columna
    val numCol: Int = pos_encontrar % M;
    val listaCol: List[Int] = getColumna(0, tablero, numCol, M) //Obtiene una lista con los elementos de la columna buscada
    val contCol:Int = contarPosicionesBorradas(listaCol)  //Recibe una lista con los elementos de la columna y cuenta el numero de apariciones puestas a -1 para ver si son 5, en cuyo caso se genererá una bomba

    //Contador de fila
    val numFila: Int = pos_encontrar / M;
    val listaFila: List[Int] = getFila(numFila, tablero, M) //Obtiene una lista con los elementos de la fila buscada
    val contFila:Int = contarPosicionesBorradas(listaFila)  //Recibe una lista con los elementos de la fila y devuelve un contador el numero de apariciones puestas a -1 para ver si son 5, en cuyo caso se genererá una bomba

    //Comparamos los valores de los contadores
    if (contCol != contFila)
    {
      if(contCol == 5) insertarElementoPosicion(66, pos_encontrar, tablero)
      else if (contFila == 5) insertarElementoPosicion(66, pos_encontrar, tablero)
      else tablero
    }
    else tablero
  }

  /**
   * Determina si se crea un bloque especial de Rompecabezas o TNT, devolviendo el tablero con el bloque especial asignado
   * @param tablero
   * @param pos_encontrar
   * @param N
   * @param M
   * @param dificultad
   * @return tablero con el bloque especial generado
   */
  def encontrarRompecabezasTNT(tablero:List[Int], pos_encontrar:Int, N:Int, M:Int, dificultad:Int):List[Int] =
  {

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
    val colorBorrar : Int = tablero(pos_encontrar) % 7
    size match
    {
      case 0 =>  insertarElementoPosicion(-1, pos_encontrar, tablero)
      case _ =>
      {
        if(tablero(pos) == colorBorrar) insertarElementoPosicion(-1, pos, realizarAccionRompecabezas(tablero, pos + 1, pos_encontrar, size - 1))
        else insertarElementoPosicion(tablero(pos), pos, realizarAccionRompecabezas(tablero, pos + 1, pos_encontrar, size - 1))
      }
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
    size match
    {
      case 0 =>  insertarElementoPosicion(-1, pos_encontrar, tablero)
      case _ =>
      {
        val filaActual: Int = pos / numCol
        val colActual : Int = pos % numCol

        val filaBorrar: Int = pos_encontrar / numCol
        val colBorrar: Int = pos_encontrar % numCol

        if(filaActual == filaBorrar || colActual == colBorrar) insertarElementoPosicion(-1, pos, realizarAccionBomba(tablero, pos + 1, pos_encontrar, size - 1, numCol))
        else insertarElementoPosicion(tablero(pos), pos, realizarAccionBomba(tablero, pos + 1, pos_encontrar, size - 1, numCol))
      }
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
   * @return tablero que elimina todas las casillas en un radio de 4 posiciones
   */
  def realizarAccionTNT(tablero: List[Int], pos: Int, pos_encontrar: Int, size: Int, numCol: Int, numFilas : Int): List[Int] =
  {
    size match
    {
      case 0  => insertarElementoPosicion(-1, pos_encontrar, tablero)
      case _ =>
      {
        val filaActual: Int = pos / numCol
        val colActual: Int = pos % numCol

        val limiteDerecho: Int = if((colActual + 4) < numCol) colActual + 4 else numCol - 1
        val limiteIzquierdo: Int = if((colActual - 4) < 0) 0 else filaActual - 4
        val limiteArriba: Int = if((filaActual - 4) < 0) 0 else colActual - 4
        val limiteAbajo: Int = if((filaActual + 4) < numFilas) filaActual + 4 else numFilas - 1

        if (colActual < limiteDerecho && colActual > limiteIzquierdo && filaActual < limiteAbajo && filaActual > limiteArriba)
          insertarElementoPosicion(-1, pos, realizarAccionTNT(tablero, pos + 1, pos_encontrar, size - 1, numCol, numFilas))

        else
          insertarElementoPosicion(tablero(pos), pos, realizarAccionTNT(tablero, pos + 1, pos_encontrar, size - 1, numCol, numFilas))
      }
    }
  }

  /**
   * Calcula el numero de apariciones de el valor -1 (que se corresponde con una posición eliminada)
   * @param listaFila
   * @return contador
   */
  def contarPosicionesBorradas(listaFila:List[Int]): Int = {
    listaFila match {
      case Nil => 0
      case head::Nil => {
        if (head == -1) 1
        else 0
      }
      case head::tail => {
        if (head == -1) 1 + contarPosicionesBorradas(tail)
        else  0 + contarPosicionesBorradas(tail)
      }
    }
  }

  //Comprueba si cierta posicion esta dentro del rango del tablero
  def contiene(tablero: List[Int], pos: Int, size: Int): Boolean =
  {
    if (pos < 0) false
    else if (pos >= size) false
    else true
  }

  //Obtiene elemento que se encuentra en la posicion index de la matriz
  def getElem(index: Int, matriz: List[Int]): Int = matriz match {
    case Nil => -1
    case head :: Nil =>
      head
    case head :: tail =>
      if (index < 0) -1
      else if (index == 0) head
      else getElem(index - 1, tail)
  }


  //Inserta un color en la posicion deseada
  def insertarElementoPosicion(e: Int, pos: Int, lista: List[Int]): List[Int] = {
    lista match
    {
      case Nil => e :: Nil //Si la lista esta vacia da igual ccuando insertarlo
      case _ => pos match
      {
        case 0 => e :: lista.tail //Es la cabeza de la lista
        case _ => lista.head :: insertarElementoPosicion(e, (pos - 1), lista.tail) //Pos - 1 ya que nuestro base es buscar caso 0
      }
    }
  }

  //Obtiene los valores de la columna index
  def getColumna(index: Int, matriz: List[Int], col:Int, numCol:Int): List[Int] = {
    matriz match {
      case Nil => Nil
      case head::Nil =>
      {
        if (index % numCol == col)  head::Nil
        else Nil
      }
      case head :: tail =>
      {
        if (index % numCol == col) head :: getColumna(index + 1, tail, col, numCol)
        else getColumna(index + 1, tail, col, numCol)
      }
    }
  }

  //Funcion que calcula la longitud de un array
  def obtenerLongitud(array: Array[String]): Int =
  {
    if (array.isEmpty) 0
    else 1 + obtenerLongitud(array.tail)
  }

  //Obtiene los valores de la fila, utilizado para encontrar bomba
  def getFila(fila: Int, matriz: List[Int], N:Int): List[Int] = fila match {
    case 0 => toma(N, matriz) //Si nos encontramos en la primera fila
    case _ => getFila((fila - 1), dejar(matriz.length - N, matriz), N)
  }

  //Saca los n primeros elementos de la lista
  def toma(n: Int, l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case head :: Nil => head::Nil
    case head :: tail =>
      if (n <= 0) Nil
      else head :: toma(n - 1, tail)
  }

  //Saca los n ultimos elementos de la lista
  def dejar(n: Int, l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case head :: Nil => head :: Nil
    case head :: tail =>
      if (tail.length <= n - 1) head :: dejar(n, tail) //Si la longitud de N es mayor o igual que el numero de elementos restantes concatenamos los elementos
      else dejar(n, tail)
  }

}
