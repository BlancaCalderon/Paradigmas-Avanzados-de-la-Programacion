package codigo

import scala.util.Random

class funcionesTablero {
  def seleccionModoJuego(modoJuego: Char, numFilas: Int, numCol: Int, dificultad: Int, tablero: List[Int], vidas: Int): Unit = {
    if (modoJuego == 'a' || modoJuego == 'A') jugarAutomatico(numFilas, numCol, dificultad, modoJuego, tablero, vidas)
    else if (modoJuego == 'm' || modoJuego == 'M') jugarManual(numFilas, numCol, dificultad, modoJuego, tablero, vidas)
    else throw new Error("Modo de juego incorrecto")
  }

  def jugarAutomatico(numFilas: Int, numCol: Int, dificultad: Int, modoJuego: Char, tablero: List[Int], vidas: Int): Unit = {


    /*val random = new Random(System.nanoTime())
    val coordX: Int = random.nextInt(numFilas)
    val coordY: Int = random.nextInt(numCol)
    println("Coordenada x = " + coordX + " Coordenada Y = " + coordY)*/

    val pos: Int = conseguirMejorJugada(tablero, 0, numFilas, numCol, numFilas * numCol, dificultad, 0)
    println("Posicion optima a borrar " + pos)
    jugar(numFilas, numCol, dificultad, tablero, modoJuego, pos, vidas)
  }

  //Funcion que calcula cual seria la mejor posicion a borrar (mayor longitud de camino)
  def conseguirMejorJugada(tablero: List[Int], pos: Int, numFilas: Int, numCol: Int, size: Int, dificultad: Int, mejorPos: Int): Int = {
    val tablero_aux: List[Int] = tablero
    if (pos == size) mejorPos
    else {
      if (!contiene(tablero, pos, size)) mejorPos
      else {
        val longActual: Int = calcularLongitud(tablero, pos, numFilas, numCol, size, dificultad)
        val mejorLong: Int = calcularLongitud(tablero, mejorPos, numFilas, numCol, size, dificultad)

        if (contiene(tablero, pos + 1, size)) {
          if (longActual > mejorLong)
            conseguirMejorJugada(tablero, pos + 1, numFilas, numCol, size, dificultad, pos)
          else
            conseguirMejorJugada(tablero, pos + 1, numFilas, numCol, size, dificultad, mejorPos)
        }
        else {
          if (longActual > mejorLong) pos
          else mejorPos
        }
      }
    }
  }

  //Devuelve longitud de un camino teniendo en cuenta si la posicion corresponde a un bloque especial
  def calcularLongitud(tablero: List[Int], pos: Int, numFilas: Int, numCol: Int, size: Int, dificultad: Int): Int = {
    //Si posicion tiene un bloque especial
    if (tablero(pos) > 6) contadorBorrar(borrarSeleccion(tablero, pos, size, tablero(pos), numFilas, numCol, dificultad)) //Calcula casillas que se borraran
    else contadorBorrar(encontrarCaminosAux(tablero, pos, 0, numFilas, numCol, size, tablero(pos), pos)) //Si no es un bloque especial calcula camino normal
  }

  def jugarManual(numFilas: Int, numCol: Int, dificultad: Int, modoJuego: Char, tablero: List[Int], vidas: Int): Unit = {
    println("Introduce la coordenada X de la posicion a borrar : ")
    val coordX: Int = scala.io.StdIn.readInt()

    println("Introduce la coordenada Y de la posicion a borrar : ")
    val coordY: Int = scala.io.StdIn.readInt()

    val pos_encontrar: Int = coordX * numCol + coordY
    jugar(numFilas, numCol, dificultad, tablero, modoJuego, pos_encontrar, vidas)
  }

  //Bucle del juego que se lleva a cabo hasta que se acaban las vidas del jugador
  def jugar(numFilas: Int, numCol: Int, dificultad: Int, tablero: List[Int], modoJuego: Char, pos_encontrar: Int, vidas: Int): Unit = {
    val size: Int = numCol * numFilas
    println("VIDAS " + vidas)

    vidas match {
      case 0 => println("Has perdido")
      case _ => {
        val color: Int = getElem(pos_encontrar, tablero)

        println("Longitud del camino = " + contadorBorrar(encontrarCaminosAux(tablero, pos_encontrar, 0, numFilas, numCol, size, tablero(pos_encontrar), pos_encontrar))) //contarLongitudCamino(tablero, pos_encontrar,0, numFilas, numCol, size, color, pos_encontrar))
        val tablero2: List[Int] = borrarSeleccion(tablero, pos_encontrar, size, numFilas, numCol, color, dificultad)
        mostrarTablero(tablero2, 0, numFilas, numCol)

        val vida2: Int = restarVidas(tablero2, vidas, pos_encontrar)

        val tablero3: List[Int] = reemplazarPosiciones(0, tablero2, numFilas, numCol, dificultad)
        mostrarTablero(tablero3, 0, numFilas, numCol)

        seleccionModoJuego(modoJuego, numFilas, numCol, dificultad, tablero3, vida2)

      }
    }
  }

  //Funcion que determina si posicion seleccionada es un bloque especial o se busca camino de forma normal
  def borrarSeleccion(tablero: List[Int], pos_encontrar: Int, size: Int, numFilas: Int, numCol: Int, color: Int, dificultad: Int): List[Int] = {
    if (tablero(pos_encontrar) == 66) insertarElementoPosicion(-1, pos_encontrar, realizarAccionBomba(tablero, 0, pos_encontrar, size, numCol))
    else if (tablero(pos_encontrar) == 84) insertarElementoPosicion(-1, pos_encontrar, realizarAccionTNT(tablero, 0, pos_encontrar, size, numCol, numFilas))
    else if (tablero(pos_encontrar) > 7 && tablero(pos_encontrar) < 14) insertarElementoPosicion(-1, pos_encontrar, realizarAccionRompecabezas(tablero, 0, pos_encontrar, size))
    else {
      val tablero2: List[Int] = encontrarCaminosAux(tablero, pos_encontrar, 0, numFilas, numCol, size, color, pos_encontrar)
      val tablero3: List[Int] = encontrarBomba(tablero2, pos_encontrar, numFilas, numCol)
      val tablero4: List[Int] = encontrarRompecabezasTNT(tablero3, pos_encontrar, numFilas, numCol, dificultad)
      tablero4
    }
  }

  //Funcion que determina si se le tiene que borrar una vida al jugador
  def restarVidas(tablero: List[Int], vidas: Int, pos_encontrar: Int): Int = {
    if (contadorBorrar(tablero) == 1 && tablero(pos_encontrar) < 7)
      vidas - 1

    else vidas
  }

  def definirDificultad(dificultad: Int): Int = {
    if (dificultad == 1) 4
    else 6
  }

  def inicializarTablero(tablero: List[Int], dificultad: Int, size: Int): List[Int] = {
    size match {
      case 0 => tablero

      case _ => {
        //val secureRandom = new Random(System.nanoTime())
        val random = new Random(System.nanoTime())
        val numeroAleatorio: Int = random.nextInt(dificultad) + 1
        numeroAleatorio :: inicializarTablero(tablero, dificultad, size - 1)
      }
    }
  }

  def mostrarTablero(l: List[Int], fila: Int, N: Int, M: Int): Unit = {
    if (l == Nil) {
      println("\n--------------")

    }
    else {
      if (fila == 0) println("\n--------------")
      if (fila % M == 0) print("\n|")
      if (l.head < 14 && l.head > 7) print("RC" + l.head % 7 + "|") //Imprime bloque especial RC
      else if (l.head > 14) print(l.head.toChar + "|")
      else if (l.head < 7) print(l.head + "|")
      mostrarTablero(l.tail, fila + 1, N, M)
    }
  }

  def encontrarCaminosAux(tablero: List[Int], pos_encontrar: Int, pos: Int, N: Int, M: Int, size: Int, color: Int, pos_original: Int): List[Int] = {
    if (pos + 1 == size) tablero
    else {
      val nuevo_tablero: List[Int] = encontrarCaminos(tablero, pos_encontrar, pos, N, M, size, color, pos_original)
      if (nuevo_tablero(pos) == -1) {
        encontrarCaminosAux(encontrarCaminos(nuevo_tablero, pos, pos, N, M, size, color, pos_original), pos_encontrar, pos + 1, N, M, size, color, pos_original)
      }
      else {
        encontrarCaminosAux(nuevo_tablero, pos_encontrar, pos + 1, N, M, size, color, pos_original)
      }
    }
  }

  //Función que encuentra todos los caminos desde una posicion hasta todas las casillas adyacentes del mismo color
  def encontrarCaminos(tablero: List[Int], pos_encontrar: Int, pos: Int, N: Int, M: Int, size: Int, color: Int, pos_original: Int): List[Int] = {
    if (pos < 0 || pos >= size || !contiene(tablero, pos, size)) {
      tablero
    }
    else {
      val fila_siguiente: Int = ((pos + M) / M)
      val fila_anterior: Int = ((pos - M) / M)

      val col_siguiente: Int = (pos + 1) % M
      val col_anterior: Int = (pos - 1) % M

      //println("COLOR ACTUAL de pos ",pos," -> ", color)
      if (contiene(tablero, pos, size) && pos == pos_encontrar && (tablero(pos) == color || tablero(pos) == -1)) //tablero.isDefinedAt(pos)
      {
        //println("Entraaaa ", pos)
        val nuevo_tablero: List[Int] = insertarElementoPosicion(-1, pos, tablero)
        if (col_siguiente < M && contiene(tablero, pos + 1, size) && col_siguiente != 0 && tablero(pos + 1) == color) // Derecha
        {
          //println("DERERCHAAA desde ", pos, "a", pos + 1)
          insertarElementoPosicion(-1, pos + 1, encontrarCaminos(nuevo_tablero, pos + 1, pos + 1, N, M, size, color, pos_original))
        }
        else if (col_anterior >= 0 && contiene(tablero, pos - 1, size) && col_anterior != M - 1 && tablero(pos - 1) == color) // Izquierda
        {
          //println("IZQUIERDAAA desde ", pos, "a", pos - 1)
          insertarElementoPosicion(-1, pos - 1, encontrarCaminos(nuevo_tablero, pos - 1, pos - 1, N, M, size, color, pos_original))
        }
        else if (fila_siguiente < N && contiene(tablero, pos + M, size) && fila_siguiente != 0 && tablero(pos + M) == color) // Abajo
        {
          //println("ABAJOOO desde ", pos, "a", pos + M)
          insertarElementoPosicion(-1, pos + M, encontrarCaminos(nuevo_tablero, pos + M, pos + M, N, M, size, color, pos_original))
        }
        else if (fila_anterior >= 0 && contiene(tablero, pos - M, size) && fila_anterior != N - 1 && tablero(pos - M) == color) // Arriba
        {
          //println("ARRIBAAA desde ", pos, "a", pos - M)
          insertarElementoPosicion(-1, pos - M, encontrarCaminos(nuevo_tablero, pos - M, pos - M, N, M, size, color, pos_original))
        }
        else {
          encontrarCaminos(nuevo_tablero, pos_original, pos_original, N, M, size - 1, color, pos_original)
        }
      }
      else {
        encontrarCaminos(tablero, pos_original, pos_original, N, M, size - 1, color, pos_original)
      }
    }
  }

  //Funcion que cuenta el numero de casillas a borrar desde una posicion
  def contarLongitudCamino(tablero: List[Int], pos_encontrar: Int, pos: Int, N: Int, M: Int, size: Int, color: Int, pos_original: Int): Int = {
    size match {
      case 0 => 1
      case _ =>
        if (pos < 0 || pos >= size || !contiene(tablero, pos, size)) {
          1
        }
        else {
          val fila_siguiente: Int = ((pos + M) / M)
          val fila_anterior: Int = ((pos - M) / M)

          val col_siguiente: Int = (pos + 1) % M
          val col_anterior: Int = (pos - 1) % M


          //println("COLOR ACTUAL de pos ",pos," -> ", color)
          if (contiene(tablero, pos, size) && pos == pos_encontrar && (tablero(pos) == color || tablero(pos) == -1)) //tablero.isDefinedAt(pos)
          {
            //println("Entraaaa ", pos)
            val nuevo_tablero: List[Int] = insertarElementoPosicion(-1, pos, tablero)
            if (col_siguiente < M && contiene(tablero, pos + 1, size) && col_siguiente != 0 && tablero(pos + 1) == color) // Derecha
            {
              //println("DERERCHAAA desde ", pos, "a", pos + 1)
              1 + contarLongitudCamino(nuevo_tablero, pos + 1, pos + 1, N, M, size, color, pos_original)
            }
            else if (col_anterior >= 0 && contiene(tablero, pos - 1, size) && col_anterior != M - 1 && tablero(pos - 1) == color) // Izquierda
            {
              //println("IZQUIERDAAA desde ", pos, "a", pos - 1)
              1 + contarLongitudCamino(nuevo_tablero, pos - 1, pos - 1, N, M, size, color, pos_original)
            }
            else if (fila_siguiente < N && contiene(tablero, pos + M, size) && fila_siguiente != 0 && tablero(pos + M) == color) // Abajo
            {
              //println("ABAJOOO desde ", pos, "a", pos + M)
              1 + contarLongitudCamino(nuevo_tablero, pos + M, pos + M, N, M, size, color, pos_original)
            }
            else if (fila_anterior >= 0 && contiene(tablero, pos - M, size) && fila_anterior != N - 1 && tablero(pos - M) == color) // Arriba
            {
              //println("ARRIBAAA desde ", pos, "a", pos - M)
              1 + contarLongitudCamino(nuevo_tablero, pos - M, pos - M, N, M, size, color, pos_original)
            }
            else contarLongitudCamino(nuevo_tablero, pos_original, pos_original, N, M, size - 1, color, pos_original)
          }
          else {
            contarLongitudCamino(tablero, pos_original, pos_original, N, M, size - 1, color, pos_original)
          }
        }
    }
  }

  /**
   * Reemplaza las posiciones del tablero que han sido eliminadas
   *
   * @param pos
   * @param tablero
   * @param N
   * @param M
   * @param dificultad
   * @return tablero con nuevos colores
   */
  def reemplazarPosiciones(pos: Int, tablero: List[Int], N: Int, M: Int, dificultad: Int): List[Int] = {
    val contador: Int = contadorBorrar(tablero)
    //println(contadorBorrar(tablero))
    contador match {
      case 0 => tablero
      case _ => {
        val nuevo: List[Int] = reemplazarAux(0, tablero, N, M, dificultad, contador, N * M)
        //println("CONTADOR NUEVO " + contadorBorrar(nuevo))
        //mostrarTablero(nuevo,0, 4, 4)
        if (contadorBorrar(nuevo) > 0) {
          reemplazarPosiciones(0, nuevo, N, M, dificultad)
        }
        else {
          nuevo
        }
      }
    }
  }

  def reemplazarAux(pos: Int, tablero: List[Int], N: Int, M: Int, dificultad: Int, contador: Int, size: Int): List[Int] = {
    if (pos >= size) tablero
    else {
      if (tablero(pos) == -1) {
        //println("Pos "+ pos+" = -1")
        val filaActual: Int = pos / M;
        val colActual: Int = pos % M;

        if (contiene(tablero, pos - M, N * M) && pos - M >= 0 && filaActual > 0 && filaActual <= N && tablero(pos - M) != -1) //Si la posicion de arriba es distinta de -1 se la asignamos a la posicion que nos llega y se la quitamos a la de arriba
        {
          //println("Entro a insertar posicion en " +  pos)
          val elem: Int = tablero(pos - M)
          val nuevo: List[Int] = insertarElementoPosicion(-1, pos - M, tablero)
          //println("Elemento pos - M = ", pos - M, nuevo(pos - M))
          insertarElementoPosicion(elem, pos, reemplazarAux(pos + 1, nuevo, N, M, dificultad, contador, size))
        }
        else if (filaActual == 0) //Si estas en la fila 0
        {
          val random = new Random(System.nanoTime())
          val color: Int = random.nextInt(dificultad) + 1
          insertarElementoPosicion(color, pos, reemplazarAux(pos + 1, tablero, N, M, dificultad, contador, size))
        }
        else {
          reemplazarAux(pos + 1, tablero, N, M, dificultad, contador, size)
        }
      }
      else {
        //println("Pos "+ pos+" = color")
        reemplazarAux(pos + 1, tablero, N, M, dificultad, contador, size)
      }
    }
  }


  def encontrarBomba(tablero: List[Int], pos_encontrar: Int, N: Int, M: Int): List[Int] = {
    //Contador de columna
    val numCol: Int = pos_encontrar % M;
    val listaCol: List[Int] = getColumna(0, tablero, numCol, M)
    //println("Lista columnas", listaCol, "Num col", numCol)
    val contCol: Int = contadorBorrar(listaCol)

    //Contador de fila
    val numFila: Int = pos_encontrar / M;
    val listaFila: List[Int] = getFila(numFila, tablero, M)
    //println("Lista Fila", listaFila)
    val contFila: Int = contadorBorrar(listaFila)

    //Comparamos los valores de los contadores
    if (contCol != contFila) {
      if (contCol == 5) insertarElementoPosicion(66, pos_encontrar, tablero)
      else if (contFila == 5) insertarElementoPosicion(66, pos_encontrar, tablero)
      else tablero
    }
    else {
      tablero
    }
  }

  /**
   * Determina si se crea un bloque especial de Rompecabezas o TNT, devolviendo el tablero con el bloque especial asignado
   *
   * @param tablero
   * @param pos_encontrar
   * @param N
   * @param M
   * @param dificultad
   * @return tablero
   */
  def encontrarRompecabezasTNT(tablero: List[Int], pos_encontrar: Int, N: Int, M: Int, dificultad: Int): List[Int] = {

    val cont: Int = contadorBorrar(tablero)

    if (cont == 6) //Si el indice vale 6 es el TNT
    {
      println("Encuentra TNT")
      insertarElementoPosicion(84, pos_encontrar, tablero)
    }
    else if (cont >= 7) //Si el indice es mayor de o igual de 7 introducimos un RC
    {
      println("Encuentra rompecabezas")
      val secureRandom = new Random(System.nanoTime())
      val color: Int = Random.nextInt(dificultad) + 1
      insertarElementoPosicion(7 + color, pos_encontrar, tablero)
    }
    else {
      tablero
    }
  }

  //Funcion que lleva a cabo la accion del rompecabezas de eliminar todas las casillas que tengan su color
  def realizarAccionRompecabezas(tablero: List[Int], pos: Int, pos_encontrar: Int, size: Int): List[Int] = {
    val colorBorrar: Int = tablero(pos_encontrar) % 7
    if (pos >= size - 1) insertarElementoPosicion(-1, pos_encontrar, tablero)
    else {
      if (tablero(pos) == colorBorrar) insertarElementoPosicion(-1, pos, realizarAccionRompecabezas(tablero, pos + 1, pos_encontrar, size))
      else insertarElementoPosicion(tablero(pos), pos, realizarAccionRompecabezas(tablero, pos + 1, pos_encontrar, size))
    }
  }


  //Funcion que lleva a cabo la accion de la bomba de eliminar todas las casillas de su misma fila y columna
  def realizarAccionBomba(tablero: List[Int], pos: Int, pos_encontrar: Int, size: Int, numCol: Int): List[Int] = {
    if (pos >= size - 1) insertarElementoPosicion(-1, pos_encontrar, tablero)
    else {
      val filaActual: Int = pos / numCol
      val colActual: Int = pos % numCol

      val filaBorrar: Int = pos_encontrar / numCol
      val colBorrar: Int = pos_encontrar % numCol

      if (filaActual == filaBorrar || colActual == colBorrar) insertarElementoPosicion(-1, pos, realizarAccionBomba(tablero, pos + 1, pos_encontrar, size, numCol))
      else insertarElementoPosicion(tablero(pos), pos, realizarAccionBomba(tablero, pos + 1, pos_encontrar, size, numCol))
    }
  }


  //Funcion que lleva a cabo la accion del TNT de eliminar todas las casillas en un radio de 4 casillas
  def realizarAccionTNT(tablero: List[Int], pos: Int, pos_encontrar: Int, size: Int, numCol: Int, numFilas: Int): List[Int] = {
    if (pos >= size - 1) insertarElementoPosicion(-1, pos_encontrar, tablero)
    else {
      val filaActual: Int = pos / numCol
      val colActual: Int = pos % numCol

      val limiteDerecho: Int = if ((colActual + 4) < numCol) colActual + 4 else numCol - 1
      val limiteIzquierdo: Int = if ((colActual - 4) < 0) 0 else filaActual - 4
      val limiteArriba: Int = if ((filaActual - 4) < 0) 0 else colActual - 4
      val limiteAbajo: Int = if ((filaActual + 4) < numFilas) filaActual + 4 else numFilas - 1

      if (colActual < limiteDerecho && colActual > limiteIzquierdo && filaActual < limiteAbajo && filaActual > limiteArriba)
        insertarElementoPosicion(-1, pos, realizarAccionTNT(tablero, pos + 1, pos_encontrar, size, numCol, numFilas))

      else
        insertarElementoPosicion(tablero(pos), pos, realizarAccionTNT(tablero, pos + 1, pos_encontrar, size, numCol, numFilas))
    }
  }


  def contadorBorrar(listaFila: List[Int]): Int = {
    listaFila match {
      case Nil => 0
      case head :: Nil => {
        if (head == -1) 1
        else 0
      }
      case head :: tail => {
        if (head == -1) 1 + contadorBorrar(tail)
        else 0 + contadorBorrar(tail)
      }
    }
  }

  //Comprueba si cierta posicion esta dentro del rango del tablero
  def contiene(tablero: List[Int], pos: Int, size: Int): Boolean = {
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


  def insertarElementoPosicion(e: Int, pos: Int, lista: List[Int]): List[Int] = {
    lista match {
      case Nil => e :: Nil //Si la lista esta vacia da igual ccuando insertarlo
      case _ => pos match {
        case 0 => e :: lista.tail //Es la cabeza de la lista
        case _ => lista.head :: insertarElementoPosicion(e, pos - 1, lista.tail) //Pos - 1 ya que nuestro base es buscar caso 0
      }
    }
  }

  //Obtiene los valores de la columna index
  def getColumna(index: Int, matriz: List[Int], col: Int, numCol: Int): List[Int] = {
    matriz match {
      case Nil => Nil
      case head :: Nil => {
        if (index % numCol == col) head :: Nil
        else Nil
      }
      case head :: tail => {
        if (index % numCol == col) head :: getColumna(index + 1, tail, col, numCol)
        else getColumna(index + 1, tail, col, numCol)
      }
    }
  }

  //Funcion que calcula la longitud de un array
  def obtenerLongitud(array: Array[String]): Int = {
    if (array.isEmpty) 0
    else 1 + obtenerLongitud(array.tail)
  }

  //Obtiene los valores de la fila, utilizado para encontrar bomba
  def getFila(fila: Int, matriz: List[Int], N: Int): List[Int] = matriz match {
    case Nil => Nil
    case head :: Nil => Nil
    case head :: tail =>
      if (fila == 0) toma(N, matriz)
      else if (fila < 0) Nil
      else
        getFila((fila - 1), dejar(matriz.length - N, matriz), N)
  }

  //Saca los n primeros elementos de la lista
  def toma(n: Int, l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case head :: Nil => head :: Nil
    case head :: tail =>
      if (n <= 0) Nil
      else head :: toma(n - 1, tail)
  }

  //Saca los n ultimos elementos de la lista
  def dejar(n: Int, l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case head :: Nil => l
    case head :: tail =>
      if (tail.length <= n - 1) l
      else dejar(n, tail)
  }

  def concatenar(tablero: List[Int], e: Int): List[Int] = {
    tablero match {
      case Nil => e :: Nil
      case head :: tail =>
        head match {
          case _ if (e <= head) => e :: tablero //If else con granularidad especifica
          case _ => head :: concatenar(tail, e)
        }
    }
  }
}

