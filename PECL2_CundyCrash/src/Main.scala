import java.security.SecureRandom
import scala.math.ceil
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello world!")
    val tablero:List[Int] = inicializarTablero(Nil, 4, 50)
    mostrarTablero(tablero,0, 10,5)
    val nuevo:List[Int] = encontrarCamino(tablero, 4, 0, 10, 5, 50, true, getElem(4, tablero))

    mostrarTablero(nuevo,0, 10,5)
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

  def encontrarCamino(tablero: List[Int], pos_encontrar:Int, pos:Int, N:Int, M:Int, size:Int, encontrado:Boolean,color:Int): List[Int] = {
    val cambiadoDer: Boolean = false
    val cambiadoIzq: Boolean = false
    val cambiadoAbajo: Boolean = false
    val cambiadoArriba: Boolean = false

    if (size < 0)
    {
      tablero

    }
    else
    {
      println(getElem(pos, tablero), " == ", color)
      if (getElem(pos, tablero) == color)//&& pos != pos_encontrar)
      {
        println("Entra", pos)
        val nuevo:List[Int]  = insertarElementoPosicion(-1, pos, tablero)

        val derecha: List[Int] = nuevo
        val izquierda: List[Int] = nuevo
        val abajo:List[Int] = nuevo
        val arriba:List[Int] = nuevo

        val fila_siguiente: Int = ((pos + M) / M)
        val fila_anterior: Int = ((pos - M) / M)

        val col_siguiente: Int = (pos + 1) % M
        val col_anterior: Int = (pos - 1)  % M
        println("Pos= ", pos, "Col sig= ", col_siguiente, " Col anterior= ", col_anterior)
        if (pos + 1 < size && col_siguiente < M && col_siguiente > 0)
        {
          val cambiadoDer: Boolean = true
          println("Derecha ", pos)
          val derecha:List[Int] = encontrarCamino(nuevo, pos_encontrar, pos + 1, N, M, size - 1, true, color) //Derecha
          return derecha
        }

        if (pos - 1 > 0 && col_anterior >= 0 && col_anterior < M - 1)
        {
          println("Izquierda")
          val cambiadoIzq: Boolean = true
          val izquierda:List[Int] = encontrarCamino(nuevo, pos_encontrar, pos - 1, N, M, size - 1, true, color) //Izquierda
          return izquierda
        }

        if (pos + M < size && fila_siguiente < N)
        {
          println("Abajo")
          val cambiadoAbajo: Boolean = true
          val abajo:List[Int] = encontrarCamino(nuevo, pos_encontrar, pos + M, N, M, size - 1, true, color) //Abajo
          return abajo
        }

        if (pos - M > 0 && fila_anterior > 0)
        {
          println("Arriba")
          val cambiadoArriba: Boolean = true
          val arriba:List[Int] = encontrarCamino(nuevo, pos_encontrar, pos - M, N, M, size - 1, true, color) //Arriba
          return arriba
        }

        if(cambiadoDer) derecha
        else if(cambiadoIzq) izquierda
        else if(cambiadoAbajo) abajo
        else if(cambiadoArriba) arriba
        else nuevo

        /*else
        {
          //nuevo = encontrarCamino(tablero, pos_encontrar, pos + 1, N, M, size - 1, true, color)
          nuevo
        }*/

      }
      else
      {
        if (pos == pos_encontrar)
        {
          val nuevo:List[Int]  = insertarElementoPosicion(-1, pos, tablero)
          encontrarCamino(nuevo, pos_encontrar, pos, N, M, size - 1, true, color)
        }
        else
        {
          encontrarCamino(tablero, pos_encontrar, pos + 1, N, M, size - 1, true, color)
        }
      }
    }
  }

  //Obtiene elemento que se encuentra en la posicion index de la matriz
  def getElem(index: Int, matriz: List[Int]): Int = matriz match {
    case Nil => -1

    case head :: Nil =>
      if (index == 0) -1
      else -1

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

}
