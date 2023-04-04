import Main.{encontrarCamino, getElem}

import java.security.SecureRandom
import scala.math.ceil
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello world!")
    val tablero:List[Int] = inicializarTablero(Nil, 4, 50)
    mostrarTablero(tablero,0, 10,5)
    val nuevo:List[Int] = encontrarCaminoAux(tablero, 4, 4, 10, 5, 50, true, getElem(4, tablero))
    val arriba = List(1,2,-1,-1,5,6,7,8)
    val tablero1 = List(1,2,3,4,5,6,7,8)
    val abajo = List(-1,2,-1,-1,5,6,-1,-1)
    //println(unirTableros(0,tablero1, arriba))
    mostrarTablero(nuevo,0, 10,5)
    println(encontrarBomba(nuevo, 4, 10, 5))
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


  def encontrarCaminoAux(tablero: List[Int], pos_encontrar:Int, pos:Int, N:Int, M:Int, size:Int, encontrado:Boolean,color:Int): List[Int] = {
    size match {
      case 0 => tablero
      case _ =>
      {
        val nuevo: List[Int] = encontrarCamino(tablero, 4, 4, 10, 5, size, true, getElem(4, tablero))
        println(nuevo)
        encontrarCaminoAux(nuevo, 4, 4, 10, 5, size - 1, true, getElem(4, tablero))
      }
    }

  }

  def encontrarCamino(tablero: List[Int], pos_encontrar: Int, pos: Int, N: Int, M: Int, size: Int, encontrado: Boolean, color: Int): List[Int] = {
    val cambiadoDer: Boolean = false
    val cambiadoIzq: Boolean = false
    val cambiadoAbajo: Boolean = false
    val cambiadoArriba: Boolean = false

    if (size < 0) {
      tablero

    }
    else {
      println(getElem(pos, tablero), " == ", color)
      if (getElem(pos, tablero) == color) //&& pos != pos_encontrar)
      {
        println("Entra", pos)
        val nuevo: List[Int] = insertarElementoPosicion(-1, pos, tablero)

        val derecha: List[Int] = nuevo
        val izquierda: List[Int] = nuevo
        val abajo: List[Int] = nuevo
        val arriba: List[Int] = nuevo

        val fila_siguiente: Int = ((pos + M) / M)
        val fila_anterior: Int = ((pos - M) / M)

        val col_siguiente: Int = (pos + 1) % M
        val col_anterior: Int = (pos - 1) % M
        println("Pos= ", pos, "Col sig= ", col_siguiente, " Col anterior= ", col_anterior)
        if (pos + 1 < size && col_siguiente < M && col_siguiente > 0) {
          val cambiadoDer: Boolean = true
          println("Derecha ", pos)
          val derecha: List[Int] = encontrarCamino(nuevo, pos_encontrar, pos + 1, N, M, size - 1, true, color) //Derecha
          return derecha
        }

        if (pos - 1 > 0 && col_anterior >= 0 && col_anterior < M - 1) {
          println("Izquierda")
          val cambiadoIzq: Boolean = true
          val izquierda: List[Int] = encontrarCamino(nuevo, pos_encontrar, pos - 1, N, M, size - 1, true, color) //Izquierda
          return izquierda
        }

        if (pos + M < size && fila_siguiente < N) {
          println("Abajo")
          val cambiadoAbajo: Boolean = true
          val abajo: List[Int] = encontrarCamino(nuevo, pos_encontrar, pos + M, N, M, size - 1, true, color) //Abajo
          return abajo
        }

        if (pos - M > 0 && fila_anterior > 0) {
          println("Arriba")
          val cambiadoArriba: Boolean = true
          val arriba: List[Int] = encontrarCamino(nuevo, pos_encontrar, pos - M, N, M, size - 1, true, color) //Arriba
          return arriba
        }

        if (cambiadoDer) derecha
        else if (cambiadoIzq) izquierda
        else if (cambiadoAbajo) abajo
        else if (cambiadoArriba) arriba
        else nuevo

        /*else
        {
          //nuevo = encontrarCamino(tablero, pos_encontrar, pos + 1, N, M, size - 1, true, color)
          nuevo
        }*/

      }
      else {
        if (pos == pos_encontrar) {
          val nuevo: List[Int] = insertarElementoPosicion(-1, pos, tablero)
          encontrarCamino(nuevo, pos_encontrar, pos, N, M, size - 1, true, color)
        }
        else {
          encontrarCamino(tablero, pos_encontrar, pos + 1, N, M, size - 1, true, color)
        }
      }
    }
  }

  def encontrarBomba(tablero: List[Int], pos_encontrar: Int, N: Int, M:Int): List[Int] = {
    //Contador de columna
    val numCol: Int = pos_encontrar % M;
    val listaCol: List[Int] = getColumna(0, tablero, numCol, M)
    println("Lista columnas", listaCol, "Num col", numCol)
    val contCol:Int = contadorBomba(listaCol)

    //Contador de fila
    val numFila: Int = pos_encontrar / M;
    val listaFila: List[Int] = getFila(numFila, tablero, M)
    println("Lista Fila", listaFila)
    val contFila:Int = contadorBomba(listaFila)

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
  def contadorBomba(listaFila:List[Int]): Int = {
    listaFila match {
      case Nil => 0
      case head::Nil => {
        if (head == -1) 1
        else 0
      }
      case head::tail => {
        if (head == -1) 1 + contadorBomba(tail)
        else  0 + contadorBomba(tail)
      }

    }
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
