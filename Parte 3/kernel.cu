#include "cuda_runtime.h"
#include "device_launch_parameters.h"
#include <stdio.h>
#include <cstdlib>
#include <ctime>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <curand.h>
#include <curand_kernel.h>
#include <device_functions.h>



//define numero de filas y columnas del tablero (CUIDADO CAMBIAR A COGER POR CONSOLA QUE FILAS SE QUIERE)
int vida = 5;
const int TESELAX = 5;
const int TESELAY = 2;

//Genera una semilla aleatoria para cada hilo
int generarSemilla()
{
    curandState_t state;
    int semilla = time(NULL) * 3663828372 + 12345 + rand();
    return semilla;
}

//Funcion que muestra el tablero por consola
void mostrarTablero(int* tablero, int numFilas, int numColumnas, int dificultad)
{
    int N = numFilas;
    int M = numColumnas;

    if (numFilas > numColumnas || numColumnas > numFilas) {     //Calculo realizado para mostrar correctamente los tableros ASIMETRICOS, solo entra en el caso de que el numero de columnas sea mayor
        N = numColumnas;
        M = numFilas;
    }
    printf("Tablero \n");
    for (int i = 0; i < numFilas; i++)                          //Recorremos las filas del tablero
    {
        printf(" \n");
        for (int j = 0; j < numColumnas; j++)                   //Recorremos las columnas del tableros
        {
            int num = tablero[i * N + j];                       //Calculamos su posicion correspondiente en el vector 1D, N será el numero de filas si numFilas > numColumnas, y N será el número de columnas en el caso contrario.
            if (num > dificultad)
            {
                if (7 <= num && num <= 13)                      //Como nuestro tablero es de tipo int los rompecabezas se encuentran entre un rango de 7 y 13 (7 asociado al rompecabezas y el resto al numero aleatorio generado en el kernel TNT)
                {
                    printf("  RC%d ||", num % 7);           //Modulo 7 nos devuelve el numero aleatorio generado en el kernel
                }
                else
                {
                    printf("  %c   ||", (char)num);             //Si otro numero se corresponde con su valor en ASCII
                }
            }
            else
            {
                printf("  %d   ||", num);                       //Numero del tablero
            }
        }
        printf("\n");
        printf(" \n");
    }
    printf("\n");
}



//Kernel que lleva a cabo la generacion del tablero de forma aleatoria
__global__ void kernelGenerarTablero(int* dev_tablero, int dev_semilla, int dificultad, int numCol, int numFila, int hilosBloqueX, int hilosBloqueY)
{
    int col = (blockIdx.x * blockDim.x) + threadIdx.x;
    int fila = (blockIdx.y * blockDim.y) + threadIdx.y;        //Posicion en la que nos encontramos
    int N = numFila;
    int dim = blockDim.x;

    int pos = ((col * N) + fila);
    if (numCol > numFila)
    {
        N = numCol;
        dim = blockDim.y;
        pos = ((fila * N) + col);
    }
    int id = threadIdx.y * dim + threadIdx.x;

    __shared__ int t_compartido[TESELAX * TESELAY];

    if (numFila > fila && numCol > col)
    {
        curandState_t state;
        curand_init(dev_semilla, pos, 0, &state); //curand_init(semilla, secuencia, offset, estado) secuencia dgenera diferentes secuencias de numeros aleatorio a partir de la misma semilla y offset genera numeros aleatorio s a partir de una secuencia y una semilla  CurandState curandState;
        t_compartido[id] = abs((int)(curand(&state) % dificultad) + 1);  //Rellena tablero con numeros aleatorios entre 1 y 6
        __syncthreads();
        dev_tablero[pos] = t_compartido[id];
    }

}
//Kernel que elimina la fila y columna de la posicion pasada (Bomba)
__global__ void kernelBomba(int* dev_tablero, int numFila, int numCol, int pos_encontrar)
{
    int col = (blockIdx.x * blockDim.x) + threadIdx.x;
    int fila = (blockIdx.y * blockDim.y) + threadIdx.y;        //Posicion en la que nos encontramos
    int N = numFila;
    int dim = blockDim.x;
    int pos = ((col * N) + fila);
    int M = numCol;

    if (numCol >= numFila)
    {
        N = numCol;
        M = numFila;
        dim = blockDim.y;
        pos = ((fila * N) + col);

    }

    int id = threadIdx.y * dim + threadIdx.x;                   //Id del hilo dentro de la tesela

    __shared__ int t_compartido[TESELAX * TESELAY];

    //Calcula fila y columna de la posición actual
    int filaActual = pos / numCol;
    int colActual = pos - filaActual * numCol;

    //Calcula fila y columna a borrar
    int filaBorrar = pos_encontrar / numCol;
    int colBorrar = pos_encontrar - filaBorrar * numCol;
    bool mem_compartida = false;

    if (numFila > fila && numCol > col)
    {
        //Precargamos los datos en el tablero
        t_compartido[id] = dev_tablero[pos];

        __syncthreads();                        //Esperamos a que todos los hilos de un bloque hayan  cargado sus datos en la memoria compartida


        //si posición actual esta en la fila o columna que queremos borrar
        if (filaBorrar == filaActual || colBorrar == colActual) //&& 0 <= filaActual <= numFila && 0 <= colActual <= numCol)
        {
            t_compartido[id] = -1;     //Indicamos que se borra
            mem_compartida = true;
        }
        if (pos == pos_encontrar)
        {
            t_compartido[id] = -1;  //Eliminamos bloque especial
            mem_compartida = true;
        }
        __syncthreads(); //Esperamos a que todos los hilos del mismo bloque hayan ejecutado el if antes de establecer la posicion a encontrar en -1
        if (mem_compartida == true) {
            dev_tablero[pos] = t_compartido[id];
        }

    }

}


//Kernel que elimina los elementos adyacentes a una posición (radio 4 elementos) (TNT)
__global__ void kernelTNT(int* dev_tablero, int numFila, int numCol, int pos_encontrar)
{
    int col = (blockIdx.x * blockDim.x) + threadIdx.x;
    int fila = (blockIdx.y * blockDim.y) + threadIdx.y;        //Posicion en la que nos encontramos
    int N = numFila;
    int dim = blockDim.x;
    int pos = ((col * N) + fila);
    int M = numCol;

    if (numCol >= numFila)
    {
        N = numCol;
        M = numFila;
        dim = blockDim.y;
        pos = ((fila * N) + col);

    }
    bool memoria_compartida = false;
    int id = threadIdx.y * dim + threadIdx.x;                   //Id del hilo dentro de la tesela

    __shared__ int t_compartido[TESELAX * TESELAY];

    //Calcula fila y columna de la posición actual
    int filaActual = pos / numCol;
    int colActual = pos - filaActual * numCol;

    //Calcula fila y columna a borrar
    int filaBorrar = pos_encontrar / numCol;
    int colBorrar = pos_encontrar - filaBorrar * numCol;
    bool mem_compartida = false;

    if (numFila > fila && numCol > col)
    {

        //Precargamos los datos en el tablero
        t_compartido[id] = dev_tablero[pos];
        __syncthreads();                        //Esperamos a que todos los hilos de un bloque hayan  cargado sus datos en la memoria compartida
       //Calcula fila y columna de la posición actual
        int filaActual = pos / numCol;
        int colActual = pos - filaActual * numCol;

        //Calcula fila y columna a borrar teniendo en cuenta el rango
        int filaBorrar = pos_encontrar / numCol;
        int colBorrar = pos_encontrar - filaBorrar * numCol;

        int filaBorrarDer = filaBorrar + 4;
        int colBorrarAbajo = colBorrar + 4;
        int filaBorrarIzq = filaBorrar - 4;
        int colBorrarArriba = colBorrar - 4;

        //si posición actual es adyacente y esta dentro del rango que queremos borrar (4)
        if (filaBorrarIzq <= filaActual && filaActual <= filaBorrarDer && colBorrarArriba <= colActual && colActual <= colBorrarAbajo && 0 <= filaActual && filaActual < numFila && 0 <= colActual && colActual < numCol && pos < (numCol * numFila))
        {
            t_compartido[id] = -1; //Indicamos que se borra
            mem_compartida = true;
        }

        if (pos == pos_encontrar) {
            t_compartido[id] = -1; //Eliminamos bloque especial
            mem_compartida = true;
        }
        __syncthreads(); //Esperamos a que todos los hilos del mismo bloque hayan ejecutado el if antes de establecer la posicion a encontrar en -1
        if (mem_compartida == true)
        {
            dev_tablero[pos] = t_compartido[id];
        }

    }


}


//Kernel que elimina todos las posiciones del color indicado (ROMPECABEZAS)
__global__ void kernelRompeCabezas(int* dev_tablero, int numFila, int numCol, int color, int pos_encontrar)
{
    int col = (blockIdx.x * blockDim.x) + threadIdx.x;
    int fila = (blockIdx.y * blockDim.y) + threadIdx.y;        //Posicion en la que nos encontramos
    int N = numFila;
    int dim = blockDim.x;

    int pos = ((col * N) + fila);
    if (numCol > numFila)
    {
        N = numCol;
        dim = blockDim.y;
        pos = ((fila * N) + col);
    }
    bool mem = false;
    int id = threadIdx.y * dim + threadIdx.x;

    __shared__ int t_compartido[TESELAX * TESELAY];

    if (numFila > fila && numCol > col)
    {
        t_compartido[id] = dev_tablero[pos];
        __syncthreads();

        int filaActual = pos / numCol;
        int colActual = pos - filaActual * numCol;

        //Si la posición actual tiene el color indicado se elimina
        if (t_compartido[id] == color && pos < (numCol * numFila))
        {
            t_compartido[id] = -1; //Indicamos que se borra
            mem = true;
        }
        if (pos == pos_encontrar) {
            t_compartido[id] = -1; //Indicamos que se borra
            mem = true;
        }
        __syncthreads(); //Esperamos a que todos los hilos del mismo bloque hayan ejecutado el if antes de establecer la posicion a encontrar en -1
        if (mem == true)
        {
            dev_tablero[pos] = t_compartido[id];              //Eliminamos bloque especial
        }

    }

}

__global__ void kernelReemplazarPosiciones(int* dev_tablero, int numFila, int numCol, int dev_semilla, int dificultad, int* dev_index)
{
    dev_index[0] = 0;
    int col = (blockIdx.x * blockDim.x) + threadIdx.x;
    int fila = (blockIdx.y * blockDim.y) + threadIdx.y;        //Posicion en la que nos encontramos

    int N = numFila;
    int dim = blockDim.x;
    int pos = ((col * N) + fila);
    int M = numCol;


    int bloque_fila_actual = (fila) / TESELAX;
    int bloque_col_actual = (col) / TESELAY;

    //Calculo del id del bloque tiene el hilo en la posicion de su izquierda, derecha, arriba y abajo
    int bloque_arriba_y = (fila - 1) / TESELAY;         //Arriba -1 Fila


    if (numCol >= numFila)
    {
        N = numCol;
        M = numFila;
        dim = blockDim.y;
        pos = ((fila * N) + col);

        //Id bloque actual del hilo en X e Y
        bloque_fila_actual = (fila) / TESELAY;
        bloque_col_actual = (col) / TESELAX;

        bloque_arriba_y = (fila - 1) / TESELAY;         //Arriba -1 Fila

    }

    int id = threadIdx.y * dim + threadIdx.x;                   //Id del hilo dentro de la tesela

    __shared__ int t_compartido[TESELAX * TESELAY];

    //Cargamos los valores de la tesela en memoria compartida
    for (int azulejo = 0; azulejo < (N + (TESELAX * TESELAY) - 1 / (TESELAX * TESELAY)); azulejo++)
    {
        if (fila < numFila && col < numCol && azulejo * (TESELAX)+threadIdx.x < numCol && azulejo * TESELAY + threadIdx.y < numFila)
        {

            t_compartido[id] = dev_tablero[pos];

        }
    }
    __syncthreads();

    if (numFila > fila && numCol > col)
    {
        __syncthreads();
        int filaActual = pos / numCol;
        int colActual = pos - filaActual * numCol;
        bool mem = false;
        bool mem_compartida = false;
        if (bloque_arriba_y == bloque_fila_actual)
        {

            if (t_compartido[id] == -1)
            {
                if ((id - TESELAX) > 0)// && ((id - TESELAX)* (TESELAX)+threadIdx.x) < numCol && ((id - TESELAX)* TESELAY + threadIdx.y) < numFila)
                {
                    if (t_compartido[id - TESELAX] != -1)
                    {
                        t_compartido[id] = t_compartido[id - TESELAX];
                        t_compartido[id - TESELAX] = -1;
                        atomicAdd(&dev_index[0], 1);
                        mem = true;
                        mem_compartida = true;
                    }
                    else if (t_compartido[id - TESELAX] != -1)
                    {
                        curandState_t state;
                        curand_init(dev_semilla, pos, 0, &state);                   //curand_init(semilla, secuencia, offset, estado) secuencia dgenera diferentes secuencias de numeros aleatorio a partir de la misma semilla y offset genera numeros aleatorio s a partir de una secuencia y una semilla  CurandState curandState;
                        int color = abs((int)(curand(&state) % dificultad) + 1);    //Rellena tablero con numeros aleatorios entre 1 y 6
                        t_compartido[id] = color;
                        atomicAdd(&dev_index[0], 1);
                        mem_compartida = true;
                    }

                }
                else
                {
                    curandState_t state;
                    curand_init(dev_semilla, pos, 0, &state); //curand_init(semilla, secuencia, offset, estado) secuencia dgenera diferentes secuencias de numeros aleatorio a partir de la misma semilla y offset genera numeros aleatorio s a partir de una secuencia y una semilla  CurandState curandState;
                    int color = abs((int)(curand(&state) % dificultad) + 1);  //Rellena tablero con numeros aleatorios entre 1 y 6
                    t_compartido[id] = color;
                    atomicAdd(&dev_index[0], 1);
                    mem_compartida = true;
                }
            }
            __syncthreads();
            if (mem)
            {
                dev_tablero[pos - numCol] = t_compartido[id - TESELAX];
            }

            if (mem_compartida == true) {
                dev_tablero[pos] = t_compartido[id];
            }

        }
        else if (dev_tablero[pos] == -1)
        {

            if (filaActual > 0 && filaActual <= numFila && dev_tablero[pos - numCol] != -1)
            {
                dev_tablero[pos] = dev_tablero[pos - numCol];
                dev_tablero[pos - numCol] = -1;
                atomicAdd(&dev_index[0], 1);
            }
            else if (dev_tablero[pos - numCol] != -1)
            {
                curandState_t state;
                curand_init(dev_semilla, pos, 0, &state); //curand_init(semilla, secuencia, offset, estado) secuencia dgenera diferentes secuencias de numeros aleatorio a partir de la misma semilla y offset genera numeros aleatorio s a partir de una secuencia y una semilla  CurandState curandState;
                int color = abs((int)(curand(&state) % dificultad) + 1);  //Rellena tablero con numeros aleatorios entre 1 y 6
                dev_tablero[pos] = color;
                atomicAdd(&dev_index[0], 1);
            }
        }
    }

}

__global__ void kernelEncontrarCaminos(int* dev_tablero, int numFila, int numCol, int* dev_index, int pos_encontrar, bool* dev_encontrado, int color)
{
    //Declaracion de varibles
    bool encontrado = false;
    bool camino_invalido = false;
    int posAux;
    int index = 0;

    int col = (blockIdx.x * blockDim.x) + threadIdx.x;
    int fila = (blockIdx.y * blockDim.y) + threadIdx.y;        //Posicion en la que nos encontramos

    int sig_bloqueX = (blockIdx.x + 1) * blockDim.x;            //Posicion inicial de la columna del siguiente hilo del siguiente bloque
    int sig_bloqueY = (blockIdx.y + 1) * blockDim.y;            //Posicion inicial de la fila del siguiente hilo en el siguiente bloque

    int bloqueX = blockIdx.x * blockDim.x;                      // //Posicion inicial de la columna del primer hilo del blqoue actual
    int bloqueY = blockIdx.y * blockDim.y;                      //Posicion inicial de la fila del primer hilo del blqoue actual

    int N = numFila;
    int dim = blockDim.x;
    int pos = ((col * N) + fila);
    int M = numCol;


    int bloque_fila_actual = (fila) / TESELAY;
    int bloque_col_actual = (col) / TESELAX;

    //Calculo del id del bloque tiene el hilo en la posicion de su izquierda, derecha, arriba y abajo
    int bloque_arriba_y = (fila - 1) / TESELAY;         //Arriba -1 Fila
    int bloque_abajo_y = (fila + 1) / TESELAY;         //Derecha +1
    int bloque_izquierda_x = (col - 1) / TESELAX;     //Abajo +1 Fila
    int bloque_derecha_x = (col + 1) / TESELAX;       //Izquierda -1

    //Recorrer 1º fila y 2ºColumna del tablero en la que se encuentra la celda de POS
    int filaActual = pos / numCol;
    int colActual = pos - filaActual * numCol;
    int ultima_posicion = pos;

    if (numCol >= numFila)
    {
        N = numCol;
        M = numFila;
        dim = blockDim.y;
        pos = ((fila * N) + col);

        //Id bloque actual del hilo en X e Y
        bloque_fila_actual = (fila) / TESELAY;
        bloque_col_actual = (col) / TESELAX;

        bloque_arriba_y = (fila - 1) / TESELAY;         //Arriba -1 Fila
        bloque_abajo_y = (fila + 1) / TESELAY;         //Derecha +1
        bloque_izquierda_x = (col - 1) / TESELAX;     //Abajo +1 Fila
        bloque_derecha_x = (col + 1) / TESELAX;       //Izquierda -1

    }

    int id = threadIdx.y * dim + threadIdx.x;                   //Id del hilo dentro de la tesela
    int posId = id;

    __shared__ int t_compartido[TESELAX * TESELAY];

    if (numFila > fila && numCol > col) {
        t_compartido[id] = dev_tablero[pos];
    }
    __syncthreads();

    if ((t_compartido[id] == color || t_compartido[id] == -1) && pos_encontrar == pos && numFila > fila && numCol > col)
    {
        encontrado = false;
        posAux = pos;
        posId = id;
        int tam_tesela = TESELAX * TESELAY;

        while ((posAux < (numCol * numFila)) && !camino_invalido && !encontrado)
        {
            bool uso_comp = false;
            int sigfila = (posAux + 1) / numCol;                 //Fila en la que se encuentra el siguiente elemento
            int sigcol = (posAux + 1) - sigfila * numCol;       //Columna en la que se encuentra el siguiente elemento

            int fila_anterior = (posAux - 1) / numCol;                 //Fila en la que se encuentra el elemento ANTERIOR
            int col_anterior = (posAux - 1) - fila_anterior * numCol; //Columna en la que se encuentra el elemento anterior

            int posSigFila = (posAux + numCol) / numCol;
            int fila_actual = posAux / numCol;
            int col_actual = posAux - fila_actual * numCol;

            if (bloque_derecha_x == bloque_col_actual && (posId + 1) < tam_tesela && t_compartido[posId + 1] == color && sigcol > 0 && (posAux + 1) != ultima_posicion && ((posId + 1) * (TESELAX)+threadIdx.x) < numCol && ((posId + 1) * TESELAY + threadIdx.y) < numFila)   //DERECHA memoria compartida
            {
                index = index + 1;
                ultima_posicion = posAux;
                posAux = posAux + 1;

                //Memoria compartida
                posId = posId + 1;
                t_compartido[posId] = -1;
                uso_comp = true;

            }
            else if (color == dev_tablero[posAux + 1] && sigcol > 0 && (posAux + 1) != ultima_posicion)         //DERECHA NORMAL
            {
                index += 1;
                ultima_posicion = posAux;
                posAux += 1;
                dev_tablero[posAux] = -1;
            }
            else if (bloque_abajo_y == bloque_fila_actual && (posId + TESELAX) < tam_tesela && t_compartido[posId + TESELAX] == color && (posAux + numCol) < (numCol * numFila) && ((posId + TESELAX) * (TESELAX)+threadIdx.x) < numCol && ((posId + TESELAX) * TESELAY + threadIdx.y) < numFila)  //ABAJO memoria compartida
            {
                index += 1;
                ultima_posicion = posAux;
                posAux = posAux + numCol;

                //Memoria compartida
                posId = posId + TESELAX;
                uso_comp = true;
                t_compartido[posId] = -1;

            }
            else if (color == dev_tablero[posAux + numCol] && (posAux + numCol) < (numCol * numFila))  //Hacia abajo 
            {
                ultima_posicion = posAux;
                posAux = posAux + numCol;
                index += 1;
                dev_tablero[posAux] = -1;
                uso_comp = true;
            }
            else if (bloque_izquierda_x == bloque_col_actual && (posId - 1) > 0 && t_compartido[posId - 1] == color && col_anterior > 0 && (posAux - 1) != ultima_posicion && ((posId - 1) * (TESELAX)+threadIdx.x) < numCol && ((posId - 1) * TESELAY + threadIdx.y) < numFila)   //IZQUIERDA memoria compartida
            {
                index += 1;
                ultima_posicion = posAux;
                posAux = posAux - 1;
                uso_comp = true;
                //Memoria compartida
                posId = posId - 1;
                t_compartido[posId] = -1;

            }
            else if (color == dev_tablero[posAux - 1] && col_anterior > 0 && (posAux - 1) != ultima_posicion)           //Izquierda && (col_anterior < numCol - 1) 
            {
                index += 1;
                ultima_posicion = posAux;
                dev_tablero[posAux] = -1;
                posAux = posAux - 1;
            }
            else if (bloque_arriba_y == bloque_fila_actual && (posId - TESELAX) > 0 && color == t_compartido[posId - TESELAX] && (posAux - numCol) >= 0 && filaActual >= 0 && filaActual <= numFila && (posAux - numCol) != ultima_posicion && ((posId - TESELAX) * (TESELAX)+threadIdx.x) < numCol && ((posId - TESELAX) * TESELAY + threadIdx.y) < numFila)    //Arriba memoria compartida
            {

                index += 1;
                ultima_posicion = posAux;
                posAux = posAux - numCol;

                //Memoria compartida
                posId = posId - TESELAX;
                t_compartido[posId] = -TESELAX;
                uso_comp = true;

            }
            else if (color == dev_tablero[posAux - numCol] && (posAux - numCol) >= 0 && filaActual >= 0 && filaActual <= numFila && (posAux - numCol) != ultima_posicion)  //ARRIBA
            {
                index += 1;
                ultima_posicion = posAux;
                posAux = posAux - numCol;
                dev_tablero[posAux] = -1;
            }
            else
            {

                if (index > 0) {
                    atomicAdd(&dev_index[0], 1);
                    encontrado = true;
                }
                else {
                    encontrado = false;
                }
                camino_invalido = true;
                t_compartido[id] = -1;
                ultima_posicion = posAux;
                uso_comp = true;
            }
            if (uso_comp == true) {
                dev_tablero[ultima_posicion] = t_compartido[id];
            }
            __syncthreads();
        }
        dev_encontrado[0] = encontrado;

        if (dev_index[0] >= 1 && pos == pos_encontrar)
        {
            dev_tablero[pos_encontrar] = -1;              //Establecemos la posicion a encontrar en -1
        }
    }
    __syncthreads();

}

__global__ void kernelEncontrarBomba(int* dev_tablero, int numFila, int numCol, int pos_encontrar, int* dev_index_fila, int* dev_index_col)
{
    int col = (blockIdx.x * blockDim.x) + threadIdx.x;
    int fila = (blockIdx.y * blockDim.y) + threadIdx.y;
    int N = numFila;
    int dim = blockDim.x;
    int pos = ((col * N) + fila);                               //Posicion en la que nos encontramos

    if (numCol >= numFila)
    {
        N = numCol;
        dim = blockDim.y;
        pos = ((fila * N) + col);
    }

    int id = threadIdx.y * dim + threadIdx.x;                   //Id del hilo dentro de la tesela

    __shared__ int t_compartido[TESELAX * TESELAY];

    //Calcula fila y columna de la posición actual
    int filaActual = pos / numCol;
    int colActual = pos - filaActual * numCol;

    //Calcula fila y columna de la posición a encontrar
    int filaEncontrar = pos_encontrar / numCol;
    int colEncontrar = pos_encontrar - filaEncontrar * numCol;

    bool mem_compartida = false;

    if (numFila > fila && numCol > col)
    {
        t_compartido[id] = dev_tablero[pos];
        __syncthreads();                        //Esperamos a que todos los hilos de cada bloque hayan cargado sus datos en el tablero

        if (filaActual == filaEncontrar && (int)dev_index_fila > 5 && t_compartido[id] == -1 && numFila > fila && numCol > col)
        {
            atomicAdd(&dev_index_fila[0], 1);
        }

        if (colActual == colEncontrar && (int)dev_index_col > 5 && t_compartido[id] == -1 && numFila > fila && numCol > col)
        {
            atomicAdd(&dev_index_col[0], 1);
        }

    }



    if (dev_index_fila[0] != dev_index_col[0] && (numFila > col) && (numCol > fila))
    {
        if ((dev_index_fila[0] == 5 && dev_index_col[0] == 1) || (dev_index_col[0] == 5 && dev_index_fila[0] == 1))
        {
            t_compartido[id] = 'B';
        }
        if (mem_compartida) {
            dev_tablero[pos] = t_compartido[id];
        }
    }
}

__global__ void kernelEncontrarRompecabezasTNT(int* dev_tablero, int numFila, int numCol, int pos_encontrar, int* dev_index_RC, int dev_semilla, int dificultad)
{
    int col = (blockIdx.x * blockDim.x) + threadIdx.x;
    int fila = (blockIdx.y * blockDim.y) + threadIdx.y;
    int N = numFila;
    int dim = blockDim.x;
    int pos = ((col * N) + fila);                   //Posicion en la que nos encontramos

    if (numCol >= numFila)
    {
        N = numCol;
        dim = blockDim.y;
        pos = ((fila * N) + col);
    }

    int id = threadIdx.y * dim + threadIdx.x;                   //Id del hilo dentro de la tesela

    __shared__ int t_compartido[TESELAX * TESELAY];

    bool mem_compartida = false;

    if (numFila > fila && numCol > col)
    {
        t_compartido[id] = dev_tablero[pos];
        __syncthreads();                        //Esperamos a que todos los hilos del bloque hayan incrementado el indice

        if (t_compartido[id] == -1)
        {

            atomicAdd(&dev_index_RC[0], 1);
        }

        if (dev_index_RC[0] == 6) // && pos == pos_encontrar)
        {
            t_compartido[id] = 'T';
            dev_index_RC[0] = 0;
            mem_compartida = true;
        }
        else if (dev_index_RC[0] >= 7 && pos == pos_encontrar)
        {
            curandState_t state;
            curand_init(dev_semilla, pos, 0, &state);                   //curand_init(semilla, secuencia, offset, estado) secuencia dgenera diferentes secuencias de numeros aleatorio a partir de la misma semilla y offset genera numeros aleatorio s a partir de una secuencia y una semilla  CurandState curandState;
            int color = abs((int)(curand(&state) % dificultad) + 1);    //Rellena tablero con numeros aleatorios entre 1 y 6
            int colorS = 7 + color;
            t_compartido[id] = colorS;
            dev_index_RC[0] = 0;
            mem_compartida = true;
        }
        __syncthreads();
        if (mem_compartida == true)
        {
            dev_tablero[pos] = t_compartido[id];
        }
        __syncthreads();
    }

}



//Inicializamos el tablero
int* inicializarTablero(int* h_tablero, int size, int numCol, int numFila, int dificultad, int hilosBloqueX, int hilosBloqueY, int gridX, int gridY)
{
    int* (dev_Tablero);

    //Reservar espacio en memoria para GPU (2 matrices y matriz resultado)
    cudaMalloc((void**)&dev_Tablero, size * sizeof(int));

    //Copiamos datos a la GPU 
    cudaMemcpy(dev_Tablero, h_tablero, size * sizeof(int), cudaMemcpyHostToDevice);

    int semilla = generarSemilla();
    dim3 dimGrid(gridX, gridY);
    dim3 dimBlock(hilosBloqueX, hilosBloqueY);
    kernelGenerarTablero << <dimGrid, dimBlock >> > (dev_Tablero, semilla, dificultad, numCol, numFila, hilosBloqueX, hilosBloqueY);

    // Copiamos de la GPU a la CPU
    cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);

    return h_tablero;

}

//Funcion que llama a kernel para encontrar todos los caminos hacia bloque indicado
int encontrarCamino(int* h_tablero_original, int numFilas, int numColumnas, int coordX, int coordY, int dificultad, int vida, int hilosBloqueX, int hilosBloqueY, int gridX, int gridY)
{
    int* h_tablero = h_tablero_original;
    int* (dev_Tablero), * (dev_index), * (dev_index_fila), * (dev_index_col), * (dev_index_RC), * (dev_hilos_x);
    bool* dev_encontrado;
    int size = numFilas * numColumnas;
    bool h_encontrado = true;
    int* h_index = { 0 };
    int* h_index_col = { 0 };
    int* h_index_fila = { 0 };
    int* h_index_RC = { 0 };

    //int pos_encontrar = coordX * numFilas + coordY;   //Posicion a ENCONTRAR en el vector 1D
    int  pos_encontrar = coordX * numColumnas + coordY;

    int color = h_tablero[pos_encontrar];

    int semilla = generarSemilla();

    //Reservar espacio en memoria para GPU (2 matrices y matriz resultado)
    cudaMalloc((void**)&dev_Tablero, size * sizeof(int));
    cudaMalloc((void**)&dev_index, sizeof(int));
    cudaMalloc((void**)&dev_index_col, sizeof(int));
    cudaMalloc((void**)&dev_index_fila, sizeof(int));
    cudaMalloc((void**)&dev_index_RC, sizeof(int));
    cudaMalloc((void**)&dev_hilos_x, sizeof(int));
    cudaMalloc(&dev_encontrado, sizeof(bool));

    //Copiamos datos a la GPU 
    cudaMemcpy(dev_Tablero, h_tablero, size * sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_index, h_index, sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_index, h_index, sizeof(int), cudaMemcpyHostToDevice);

    cudaMemcpy(dev_index_col, h_index_col, sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_index_fila, h_index_fila, sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_index_RC, h_index_RC, sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_encontrado, &h_encontrado, sizeof(bool), cudaMemcpyHostToDevice);


    dim3 dimGrid(gridX, gridY);
    dim3 dimBlock(hilosBloqueX, hilosBloqueY);

    //Segun si es alguno de los bloques especiales o es una jugada normal (66 --> B, 84 --> T,)
    int contenido = h_tablero[pos_encontrar];

    if (contenido == 'B')
    {
        kernelBomba << <dimGrid, dimBlock >> > (dev_Tablero, numFilas, numColumnas, pos_encontrar);
        cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);

    }

    else if (contenido == 'T')
    {
        kernelTNT << <dimGrid, dimBlock >> > (dev_Tablero, numFilas, numColumnas, pos_encontrar);
        cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
    }

    else if (7 <= contenido && contenido <= 13) //Si es RC
    {
        int colorBorrar = contenido % 7;
        kernelRompeCabezas << <dimGrid, dimBlock >> > (dev_Tablero, numFilas, numColumnas, colorBorrar, pos_encontrar);

        cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
    }
    else //Si es bloque simple
    {
        int cont = 0;
        //Desde posición idicada se encuentran todos los caminos con el mismo color
        while (cont < numColumnas * numFilas)
        {
            while (h_encontrado)
            {

                kernelEncontrarCaminos << <dimGrid, dimBlock >> > (dev_Tablero, numFilas, numColumnas, dev_index, pos_encontrar, dev_encontrado, color);
                cudaMemcpy(&h_encontrado, dev_encontrado, sizeof(bool), cudaMemcpyDeviceToHost);
                cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
                cudaMemcpy(&h_index, dev_index, sizeof(int), cudaMemcpyDeviceToHost);
            }

            if (h_tablero[cont] == -1)
            {
                pos_encontrar = cont;
                h_encontrado = 1;
            }
            cont += 1;
        }
        if ((int)h_index == 0 && vida >= 1)
        {
            vida = vida - 1;
        }
        h_index_fila = { 0 };
        h_index_col = { 0 };
        cudaMemcpy(dev_index_col, h_index_col, sizeof(int), cudaMemcpyHostToDevice);
        cudaMemcpy(dev_index_fila, h_index_fila, sizeof(int), cudaMemcpyHostToDevice);
        kernelEncontrarBomba << <dimGrid, dimBlock >> > (dev_Tablero, numFilas, numColumnas, pos_encontrar, dev_index_fila, dev_index_col);
        cudaMemcpy(&h_index_fila, dev_index_fila, sizeof(int), cudaMemcpyDeviceToHost);
        cudaMemcpy(&h_index_col, dev_index_col, sizeof(int), cudaMemcpyDeviceToHost);
        dev_index_fila = 0;
        dev_index_col = 0;
        h_index_RC = { 0 };
        cudaMemcpy(dev_index_RC, h_index_RC, sizeof(int), cudaMemcpyHostToDevice);
        kernelEncontrarRompecabezasTNT << <dimGrid, dimBlock >> > (dev_Tablero, numFilas, numColumnas, pos_encontrar, dev_index_RC, semilla, dificultad);
        cudaMemcpy(&h_index_RC, dev_index_RC, sizeof(int), cudaMemcpyDeviceToHost);
        cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
        dev_index_RC = 0;
    }

    h_index = { 0 };
    int iteraciones = 10;

    while (iteraciones > 0)
    {
        semilla = generarSemilla();
        kernelReemplazarPosiciones << <dimGrid, dimBlock >> > (dev_Tablero, numFilas, numColumnas, semilla, dificultad, dev_index);
        cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
        cudaMemcpy(&h_index, dev_index, sizeof(int), cudaMemcpyDeviceToHost);
        iteraciones = (int)h_index;
    }
    cudaFree(dev_encontrado);
    cudaFree(dev_Tablero);
    cudaFree(dev_index);
    cudaFree(dev_index_fila);
    cudaFree(dev_index_col);
    cudaFree(dev_index_RC);
    return vida;
}

void main(int argc, char* argv[])
{
    //Declaracion variables
    int* h_tablero;
    int numFilas = 0;
    int numColumnas = 0;
    int coordenadaX;
    int coordenadaY;
    int size = 0;
    int dificultad = 0;
    bool terminado = false;
    char modoJuego = 'A';


    //Calcula caracteristicas tarjeta grafica
    cudaDeviceProp deviceProp;
    cudaGetDeviceProperties(&deviceProp, 0);
    char* nombre = deviceProp.name;
    int maxThreadsPerBlock = deviceProp.maxThreadsPerBlock;
    int maxThreadsSM = deviceProp.maxThreadsPerMultiProcessor;
    int maxBlockx = deviceProp.maxGridSize[0];
    int maxBlocky = deviceProp.maxGridSize[1];
    int maxGridX = deviceProp.maxThreadsDim[0];
    int maxGridY = deviceProp.maxThreadsDim[1];

    //Codigo para ejecutar programa y recibir datos por comando
    //Controla que no de error la llamada
    if (argc == 1)  //No se ha ejecutado por comando
    {
        printf("\nElija el modo de juego: A (Automatico) - M (Manual):  \n");
        scanf("%c", &modoJuego);
        printf("Modo de juego seleccionado: %c \n", modoJuego);
        printf("\nIntroduzca el numero de filas que tendra el tablero:  \n");
        scanf("%d", &numFilas);
        printf("\nIntroduzca el numero de columnas que tendra el tablero:  \n");
        scanf("%d", &numColumnas);
        printf("\nIntroduzca la dificultad del juego:  \n");
        scanf("%d", &dificultad);
    }

    else if (argc == -1)
    {
        printf("ERROR en ejecucion\n");
    }
    //Controla que tengamos los argumentso encesarios (tipo de ejecucion, dificultad, filas, columnas)
    else if (argc < 5)
    {
        printf("ERROR: faltan argumentos de entrada\n");
        printf("%d \n", argc);
    }
    //Controlan que no se pasen mas argumentos de los deseados
    else if (argc > 5)
    {
        printf("ERROR: sobran argumentos de entrada\n");
    }
    else //Realiza llamada con los 4 argumemtos
    {
        //Guarda los argumentos pasadas en las respectivas variables
        modoJuego = argv[1][0];
        dificultad = std::stoi(argv[2]);    //Guarda valor argumentos usando funcion stoi para convertirlo a int
        numFilas = std::stoi(argv[3]);
        numColumnas = std::stoi(argv[4]);

    }
    size = numFilas * numColumnas;

    int hilosBloqueX = ceil(numColumnas / (float)2);
    int hilosBloqueY = ceil(numFilas / (float)2);

    if (numColumnas > TESELAX && numFilas > TESELAY && numColumnas > numFilas) {
        hilosBloqueX = ceil(numColumnas / TESELAY);
        hilosBloqueY = ceil(numFilas / TESELAX);
    }
    else if (numColumnas > TESELAX && numFilas > TESELAY && numFilas > numColumnas) {
        hilosBloqueX = ceil(numColumnas / TESELAX);
        hilosBloqueY = ceil(numFilas / TESELAY);
    }

    int gridX = ceil(numColumnas / (float)hilosBloqueX);
    int gridY = ceil(numFilas / (float) hilosBloqueY);

    printf("dimBlock(%d, %d), dimGrid(%d, %d): \n", hilosBloqueX, hilosBloqueY, gridX, gridY);
    //Pasamos a memoria constante el numero de filas y columnas introducidas por el usuario

    //Reservamos memoria para el tablero, ya que no esta inicializado
    h_tablero = (int*)malloc(numFilas * numColumnas * sizeof(int));

    //Llamamos a la funcion que inicializa con valores aleatorios el tablero
    h_tablero = inicializarTablero(h_tablero, size, numColumnas, numFilas, dificultad, hilosBloqueX, hilosBloqueY, gridX, gridY);
    mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);

    if (hilosBloqueX >= maxBlockx && hilosBloqueY >= maxBlocky && gridX > maxGridX && gridY > maxGridY)
    {
        printf("\nSe sobrepasan las dimensiones asociadas a la tarjeta grafica  \n");
    }
    else
    {
        while (vida > 0)
        {
            if (modoJuego == 'M' || modoJuego == 'm')
            {
                printf("\nIntroduzca las coordenadas del bloque que desea eliminar (x, y):  \n");
                scanf("%d %d", &coordenadaX, &coordenadaY);
            }
            else if (modoJuego == 'A' || modoJuego == 'a')
            {
                coordenadaX = (rand() % numFilas);
                coordenadaY = (rand() % numColumnas);
                printf("\nCoordenadas (%d, %d)  \n", coordenadaX, coordenadaY);
            }

            if ((coordenadaX < numFilas) && (coordenadaY < numColumnas) && (coordenadaX >= 0) && (coordenadaY >= 0))
            {
                vida = encontrarCamino(h_tablero, numFilas, numColumnas, coordenadaX, coordenadaY, dificultad, vida, hilosBloqueX, hilosBloqueY, gridX, gridY);
                printf("\nVida restante: %d \n", vida);
                mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);
            }
            else
            {
                printf("\nLas coordenadas introducidas se encuentran fuera del rango del tablero [%d][%d] \n", numFilas, numColumnas);
            }

        }

        printf("\nPERDEDOR \n");
    }

}



