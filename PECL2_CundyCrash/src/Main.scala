
import Main.contadorBorrar

import java.security.SecureRandom
import scala.math.ceil
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    val tablero: List[Int] = List(1, 2, 3, 4, 1, 1, 1, 1, 2, 3, 2, 1, 1, 1, 2, 2)
    mostrarTablero(tablero, 0, 4, 4)
    val color: Int = getElem(4, tablero)
    val tablero2: List[Int] = encontrarCaminos(tablero, 4, 0, 4, 4, 16, color, 4)
    mostrarTablero(tablero2, 0, 4, 4)
    val reemplazado:List[Int] = reemplazarPosiciones(0, tablero2, 4, 4, 4)
    mostrarTablero(reemplazado, 0, 4, 4)
  }

  def inicializarTablero(tablero:List[Int], dificultad:Int, size:Int): List[Int] = {
    size match
    {
      case 0 => tablero

      case _ =>
      {
        val secureRandom = new Random(System.nanoTime())
        val numeroAleatorio:Int = Random.nextInt(dificultad) + 1
        val nueva:List[Int] = concatenar(tablero, numeroAleatorio)
        inicializarTablero(nueva, dificultad, size - 1)
      }
    }
  }

  def concatenar(tablero:List[Int], e:Int): List[Int] = {
    tablero match
    {
      case Nil => e :: Nil
      case head :: tail =>
        head match
        {
          case _ if (e <= head) => e :: tablero //If else con granularidad especifica
          case _ => head :: concatenar(tail, e)
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
        print(l.head + "|")
        mostrarTablero(l.tail, fila + 1, N, M)
      }
  }


  def encontrarCaminos(tablero: List[Int], pos_encontrar: Int, pos: Int, N: Int, M: Int, size: Int, color: Int, pos_original: Int): List[Int] = {
    size match {
      case 0 => tablero
      case _ =>
        if (pos < 0 || pos >= size || !tablero.isDefinedAt(pos)) {
          tablero
        }
        else {
          val fila_siguiente: Int = ((pos + M) / M)
          val fila_anterior: Int = ((pos - M) / M)

          val col_siguiente: Int = (pos + 1) % M
          val col_anterior: Int = if (pos % M == 0) M - 1 else (pos - 1) % M


          //println("COLOR ACTUAL de pos ",pos," -> ", color)
          if (contiene(tablero, pos, size) && pos == pos_encontrar && (tablero(pos) == color || tablero(pos) == -1)) //tablero.isDefinedAt(pos)
          {
            println("Entraaaa ", pos)
            val nuevo_tablero: List[Int] = insertarElementoPosicion(-1, pos, tablero)
            if (col_siguiente < M && contiene(tablero, pos + 1, size) && col_siguiente != 0 && tablero(pos + 1) == color) // Derecha
            {
              println("DERERCHAAA desde ", pos, "a", pos + 1)
              insertarElementoPosicion(-1, pos + 1, encontrarCaminos(nuevo_tablero, pos + 1, pos + 1, N, M, size, color, pos_original))
            }
            else if (col_anterior >= 0 && contiene(tablero, pos - 1, size) && col_anterior != M - 1 && tablero(pos - 1) == color) // Izquierda
            {
              println("IZQUIERDAAA desde ", pos, "a", pos - 1)
              insertarElementoPosicion(-1, pos - 1, encontrarCaminos(nuevo_tablero, pos - 1, pos - 1, N, M, size, color, pos_original))

            }
            else if (fila_siguiente < N && contiene(tablero, pos + M, size) && fila_siguiente != 0 && tablero(pos + M) == color) // Abajo
            {
              println("ABAJOOO desde ", pos, "a", pos + M)
              insertarElementoPosicion(-1, pos + M, encontrarCaminos(nuevo_tablero, pos + M, pos + M, N, M, size, color, pos_original))
            }
            else if (fila_anterior >= 0 && contiene(tablero, pos - M, size) && fila_anterior != N - 1 && tablero(pos - M) == color) // Arriba
            {
              println("ARRIBAAA desde ", pos, "a", pos - M)
              insertarElementoPosicion(-1, pos - M, encontrarCaminos(nuevo_tablero, pos - M, pos - M, N, M, size, color, pos_original))
            }
            else {
              insertarElementoPosicion(nuevo_tablero(pos), pos, encontrarCaminos(nuevo_tablero, pos_original, pos_original, N, M, size - 1, color, pos_original))
            }
          }
          else {
            insertarElementoPosicion(tablero(pos), pos, encontrarCaminos(tablero, pos_original, pos_original, N, M, size - 1, color, pos_original))
          }
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
    val contador:Int = contadorBorrar(tablero)
    println(contadorBorrar(tablero))
    contador match {
      case 0 => {
        println(contadorBorrar(tablero))
        if (contadorBorrar(tablero) == 0) {
          tablero
        }
        else
        {
          reemplazarPosiciones(0, tablero, N, M, dificultad)
          //  reemplanzarAux(0, tablero, N, M, dificultad, contadorBorrar(tablero))
        }
      }
      case _ => {
        val nuevo:List[Int] = reemplazarAux(0, tablero, N, M, dificultad, contador, N*M)
        println("CONTADOR NUEVO " + contadorBorrar(nuevo))
        mostrarTablero(nuevo,0, 4, 4)
        if (contadorBorrar(nuevo) > 0)
        {
          reemplazarPosiciones(0, nuevo, N, M, dificultad)
        }
        else
        {
            nuevo
        }
      }
    }
  }

  def reemplazarAux(pos: Int, tablero: List[Int], N: Int, M: Int, dificultad: Int, contador: Int, size:Int): List[Int] = {
    size match {
      case 0 => tablero
      case _ =>  {
        println(pos)
        if (tablero(pos) == -1) {
          println("Pos "+ pos+" = -1")
          val filaActual: Int = pos / M;
          val colActual: Int = pos - filaActual * M;
          if (contiene(tablero, pos - M, N * M) && pos - M >= 0 && filaActual > 0 && filaActual <= N && tablero(pos - M) != -1) //Si la posicion de arriba es distinta de -1 se la asignamos a la posicion que nos llega y se la quitamos a la de arriba
          {
            println("Entro a insertar posicion en " +  pos)
            val elem: Int = tablero(pos - M)
            val nuevo: List[Int] = insertarElementoPosicion(-1, pos-M, tablero)
            println("Elemento pos - M = ", pos - M, nuevo(pos - M))
            insertarElementoPosicion(elem, pos, reemplazarAux(pos + 1, nuevo, N, M, dificultad, contador, size - 1))
          }
          else if (filaActual == 0) //Si estas en la fila 0
          {
            val secureRandom = new Random(System.nanoTime())
            val color: Int = Random.nextInt(dificultad) + 1
            insertarElementoPosicion(color, pos, reemplazarAux(pos + 1, tablero, N, M, dificultad, contador, size - 1))
          }
          else
          {

            reemplazarAux(pos + 1, tablero, N, M, dificultad, contador, size - 1)
          }
        } else
        {
          println("Pos "+ pos+" = color")
          reemplazarAux(pos + 1, tablero, N, M, dificultad, contador, size - 1)
        }
      }
    }
  }
  def encontrarBomba(tablero: List[Int], pos_encontrar: Int, N: Int, M:Int): List[Int] = {
    //Contador de columna
    val numCol: Int = pos_encontrar % M;
    val listaCol: List[Int] = getColumna(0, tablero, numCol, M)
    println("Lista columnas", listaCol, "Num col", numCol)
    val contCol:Int = contadorBorrar(listaCol)

    //Contador de fila
    val numFila: Int = pos_encontrar / M;
    val listaFila: List[Int] = getFila(numFila, tablero, M)
    println("Lista Fila", listaFila)
    val contFila:Int = contadorBorrar(listaFila)

    //Comparamos los valores de los contadores
    if (contCol != contFila)
    {
      if(contCol == 5) insertarElementoPosicion(66, pos_encontrar, tablero)
      else if (contFila == 5) insertarElementoPosicion(66, pos_encontrar, tablero)
      else tablero
    } else
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
  def encontrarRompecabezasTNT(tablero:List[Int], pos_encontrar:Int, N:Int, M:Int, dificultad:Int):List[Int] = {

    val cont:Int = contadorBorrar(tablero)

    if (cont == 6) //Si el indice vale 6 es el TNT
    {
      insertarElementoPosicion(84, pos_encontrar, tablero)
    }
    else if (cont >= 7) //Si el indice es mayor de o igual de 7 introducimos un RC
    {
      val secureRandom = new Random(System.nanoTime())
      val color: Int = Random.nextInt(dificultad) + 1
      insertarElementoPosicion(color, pos_encontrar, tablero)
    }
    else
    {
      tablero
    }
  }

  def contadorBorrar(listaFila:List[Int]): Int = {
    listaFila match {
      case Nil => 0
      case head::Nil => {
        if (head == -1) 1
        else 0
      }
      case head::tail => {
        if (head == -1) 1 + contadorBorrar(tail)
        else  0 + contadorBorrar(tail)
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

  //Obtiene los valores de la fila, utilizado para encontrar bomba
  def getFila(fila: Int, matriz: List[Int], N:Int): List[Int] = matriz match {
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
    case head :: Nil => head::Nil
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
  //Une los tableros
  def unirCaminos(pos: Int, tablero: List[Int], derecha: List[Int], izquierda: List[Int], arriba: List[Int], abajo: List[Int]): List[Int] = {
    tablero match
    {
      case Nil => tablero

      case head :: Nil =>
        if (getElem(pos, derecha) == -1 || getElem(pos, izquierda) == -1 || getElem(pos, arriba) == -1 || getElem(pos, abajo) == -1) {
          -1::Nil
        }
        else
        {
          head::Nil
        }

      case head :: tail =>
      {
        if (getElem(pos, derecha) == -1 || getElem(pos, izquierda) == -1 || getElem(pos, arriba) == -1 || getElem(pos, abajo) == -1)
        {
          -1::unirCaminos(pos+1, tail, derecha, izquierda, arriba, abajo)
        } else {
          head::unirCaminos(pos+1, tail, derecha, izquierda, arriba, abajo)
        }
      }
    }
  }

  //Une los tableros
  def unirTableros(pos: Int, tablero: List[Int], nuevo: List[Int]): List[Int] = {
    tablero match {
      case Nil => Nil

      case head :: Nil =>
        if (getElem(pos, nuevo) == -1) {
          println(pos, " de la lista ", nuevo)
          -1 :: Nil
        }
        else {
          head :: Nil
        }

      case head :: tail => {
        if (getElem(pos, nuevo) == -1) {
          -1 :: unirTableros(pos + 1, tail, nuevo)
        } else {
          head :: unirTableros(pos + 1, tail, nuevo)
        }
      }
    }
  }

}
