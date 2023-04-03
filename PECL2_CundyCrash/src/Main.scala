import java.security.SecureRandom
import scala.math.ceil
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello world!")
    val tablero:List[Int] = inicializarTablero(Nil, 4, 20)
    mostrarTablero(tablero,0, 4,5)
    val nuevo:List[Int] = encontrarCamino(tablero, 4, 0, 4, 5, 20, true, getElem(4, tablero))

    mostrarTablero(nuevo,0, 4,5)
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
    if (size < 0 || !encontrado) {
      tablero

    }
    else
    {
      println(getElem(pos, tablero), " == ", color)
      if (getElem(pos, tablero) == color)//&& pos != pos_encontrar)
      {
        println("Entra", pos)
        val nuevo:List[Int]  = insertarElementoPosicion(-1, pos, tablero)

        val fila_siguiente: Int = ((pos + M) / M)
        val fila_anterior: Int = ((pos - M) / M)

        val col_siguiente: Int = ceil((pos + 1) / N)
        val col_anterior: Int = (pos - 1)  / N
        println("Pos= ", pos, "Col sig= ", col_siguiente, " Col anterior= ", col_anterior)
        if (pos + 1 < size && col_siguiente < M && col_siguiente > 0)
        {
          println("Derecha ", pos)
          encontrarCamino(nuevo, pos_encontrar, pos + 1, N, M, size - 1, true, color) //Derecha
        }

        if (pos - 1 > 0 && col_anterior >= 0 && col_anterior < M - 1)
        {
          println("Izquierda")
          encontrarCamino(nuevo, pos_encontrar, pos - 1, N, M, size - 1, true, color) //Izquierda
        }

        if (pos + M < size && fila_siguiente < N)
        {
          println("Abajo")
          encontrarCamino(nuevo, pos_encontrar, pos + M, N, M, size - 1, true, color) //Abajo
        }

        if (pos - M > 0 && fila_anterior > 0)
        {
          println("Arriba")
          encontrarCamino(nuevo, pos_encontrar, pos - M, N, M, size - 1, true, color) //Arriba
        }
        else
        {
          encontrarCamino(tablero, pos_encontrar, pos + 1, N, M, size - 1, true, color)
        }

      } else {
        if (pos == pos_encontrar)
        {
          encontrarCamino(tablero, pos_encontrar, pos, N, M, size - 1, true, color)
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
