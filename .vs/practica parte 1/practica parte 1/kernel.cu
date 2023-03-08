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

//Define constantes
#define AZUL 1
#define ROJO 2
#define NARANJA 3
#define VERDE 4
#define MARRON 5
#define AMARRILLO 6

//define numero de filas y columnas del tablero (CUIDADO CAMBIAR A COGER POR CONSOLA QUE FILAS SE QUIERE)
__constant__ int* FILAS;
__constant__ int* COLUMNAS;

__device__ int** caminos = {};
__device__ int indice = 0;

//Funcion que muestra el tablero por consola
void mostrarTablero(int* tablero, int numFilas, int numColumnas)
{
    for (int i = 0; i < numFilas; i++)
    {
        for (int j = 0; j < numColumnas; j++)
        {
            printf("%d  ", tablero[i * numFilas + j]);
        }
        printf("\n");
    }
    printf("\n");
}


//Kernel que lleva a cabo la generacion del tablero de forma aleatoria
__global__ void kernelGenerarTablero(int* dev_tablero, int dev_semilla, int dificultad)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;
    curandState_t state;
    curand_init(dev_semilla, pos, 0, &state); //curand_init(semilla, secuencia, offset, estado) secuencia dgenera diferentes secuencias de numeros aleatorio a partir de la misma semilla y offset genera numeros aleatorio s a partir de una secuencia y una semilla  CurandState curandState;
    dev_tablero[pos] = abs((int)(curand(&state) % dificultad) + 1);  //Rellena tablero con numeros aleatorios entre 1 y 6
}


__device__ bool backtrackingA(int* dev_tablero, int** dev_caminos, int* dev_camino, int numFila, int numCol, int* dev_index, int index, int pos_encontrar, bool encontrado, int posAux, int filaActual, int colActual, int ultima_posicion, int pos, bool camino_invalido, int num_camino)
{
    if (dev_tablero[pos] == dev_tablero[pos_encontrar])
    {
        posAux = pos;
        printf("\nHilo numero %d - posicion auxiliar inicial %d \n", pos, posAux);

        if ((posAux < numCol * numFila) && !encontrado && !camino_invalido)
        {
            int sigfila = (posAux + 1) / numCol;                 //Fila en la que se encuentra el siguiente elemento
            int sigcol = (posAux + 1) - sigfila * numCol;       //Columna en la que se encuentra el siguiente elemento

            int fila_anterior = (posAux - 1) / numCol;                 //Fila en la que se encuentra el elemento ANTERIOR
            int col_anterior = (posAux - 1) - fila_anterior * numCol; //Columna en la que se encuentra el elemento anterior


            colActual = sigcol - 1;                     //Columna de la posicion actual
            int posSigFila = (posAux + numCol) / numCol;

            /*for (int i = 0; i < numFila; i++)
            {
                for (int j = 0; j < numCol; j++)
                {
                    printf("%d  ", dev_tablero[i * numFila + j]);
                }
                printf("\n");
            }
            printf("\n");*/

            // printf("\n DERECHA- fila [%d] col [%d] hilo %d\n", sigfila, sigcol, pos);
            // printf("\n ABAJO - fila[%d] col[%d] (hilo %d)\n", posSigFila, colActual, pos);
            //printf("\n IZQUIEDA - fila[%d] col[%d] posicion %d (hilo %d) ultima pos %d\n", posSigFila, col_anterior, posAux - 1, pos, ultima_posicion);
            // printf("\n Condicion derecha: hilo %d va la columna %d (desde posAux %d )\n", pos, posAux+1, posAux);
            // printf("\n Condicion abajo: hilo %d desde: posAux %d va a la columna %d\n", pos, posAux, posAux + numCol);

            //Si son del mismo color
            if (posAux == pos_encontrar)
            {
                printf("\nCamino ENCONTRADO %d\n", pos);
                index += 1;
                dev_camino[index] = pos_encontrar;
                num_camino = atomicAdd(&dev_index[0], 1);
                printf("\nPosible camino num_camino[%d] (hilo %d) de tamano %d\n", num_camino, pos, index);

                __syncthreads(); // Sincronizamos los hilos del bloque para asegurarnos de que se complete la operación atomicAdd
                dev_caminos[num_camino] = dev_camino;
                encontrado = true;

                
                return encontrado;
            }
            else
            {

                if (dev_tablero[pos] == dev_tablero[posAux + 1] && sigcol >= 0 && (posAux + 1) != ultima_posicion)          //Nos desplazamos a la derecha
                {
                    printf("\nAvanza a la pos DERECHA [%d] hilo %d", posAux, pos);
                    dev_camino[index] = posAux;
                    dev_tablero[posAux] = -1;
                    ultima_posicion = posAux;
                    posAux += 1;
                    index += 1;
                    printf("\nContinua por posicion (fila) %d hilo %d\n", posAux, pos);
                    encontrado = backtrackingA(dev_tablero, dev_caminos, dev_camino, numFila, numCol, dev_index, index, pos_encontrar, encontrado, posAux, filaActual, colActual, ultima_posicion, pos, camino_invalido, num_camino);

                    if (encontrado)
                    {
                        printf("\nCamino ENCONTRADO %d\n", pos);
                        index += 1;
                        dev_camino[index] = pos_encontrar;
                        num_camino = atomicAdd(&dev_index[0], 1);
                        printf("\nPosible camino num_camino[%d] (hilo %d) de tamano %d\n", num_camino, pos, index);

                        __syncthreads(); // Sincronizamos los hilos del bloque para asegurarnos de que se complete la operación atomicAdd
                        dev_caminos[num_camino] = dev_camino;
                        encontrado = true;

                        return encontrado;
                    }

                    index--; //Elimina ultimo elemento si no encuentra camino para volver atras
                }
                else if (dev_tablero[pos] == dev_tablero[posAux + numCol] && (posAux + numCol) != ultima_posicion && (posAux + numCol) < numCol * numFila)  //Nos desplazamos hacia abajo
                {
                    ultima_posicion = posAux;
                    dev_tablero[posAux] = -1;
                    printf("\nAvanza a la pos de ABAJO [%d] ultima posicion %d hilo %d", posAux + numCol, ultima_posicion, pos);
                    dev_camino[index] = posAux;
                    posAux = posAux + numCol;
                    //printf("\nPos aux vale: %d hilo %d", posAux, pos);
                    index += 1;

                    encontrado = backtrackingA(dev_tablero, dev_caminos, dev_camino, numFila, numCol, dev_index, index, pos_encontrar, encontrado, posAux, filaActual, colActual, ultima_posicion, pos, camino_invalido, num_camino);

                    if (encontrado)
                    {
                        printf("\nCamino ENCONTRADO %d\n", pos);
                        index += 1;
                        dev_camino[index] = pos_encontrar;
                        num_camino = atomicAdd(&dev_index[0], 1);
                        printf("\nPosible camino num_camino[%d] (hilo %d) de tamano %d\n", num_camino, pos, index);

                        __syncthreads(); // Sincronizamos los hilos del bloque para asegurarnos de que se complete la operación atomicAdd
                        dev_caminos[num_camino] = dev_camino;
                        encontrado = true;

                        return encontrado;
                    }

                    index--;//Elimina ultimo elemento si no encuentra camino para volver atras

                }
                else if (dev_tablero[pos] == dev_tablero[posAux - 1] && col_anterior >= 0 && posAux - 1 >= numCol && (posAux - 1) != ultima_posicion)           //Izquierda
                {
                    dev_camino[index] = posAux;
                    ultima_posicion = posAux;
                    dev_tablero[posAux] = -1;
                    posAux = posAux - 1;
                    printf("\nAvanza a la pos IZQUIERDA [%d] hilo %d", posAux, pos);
                    index += 1;

                    encontrado = backtrackingA(dev_tablero, dev_caminos, dev_camino, numFila, numCol, dev_index, index, pos_encontrar, encontrado, posAux, filaActual, colActual, ultima_posicion, pos, camino_invalido, num_camino);

                    if (encontrado)
                    {
                        printf("\nCamino ENCONTRADO %d\n", pos);
                        index += 1;
                        dev_camino[index] = pos_encontrar;
                        num_camino = atomicAdd(&dev_index[0], 1);
                        printf("\nPosible camino num_camino[%d] (hilo %d) de tamano %d\n", num_camino, pos, index);

                        __syncthreads(); // Sincronizamos los hilos del bloque para asegurarnos de que se complete la operación atomicAdd
                        dev_caminos[num_camino] = dev_camino;
                        encontrado = true;

                        return encontrado;
                    }

                    index--; //Elimina ultimo elemento si no encuentra camino para volver atras

                }
                else if (dev_tablero[pos] == dev_tablero[posAux - numCol] && (posAux - numCol) != ultima_posicion && posAux >= numCol && (posAux - numCol) >= 0)  //ARRIBA
                {
                    //printf("\nAvanza a la pos de ARRIBA [%d] hilo %d", posAux, pos);
                    dev_camino[index] = posAux;
                    ultima_posicion = posAux;
                    dev_tablero[posAux] = -1;
                    printf("\nAvanza a la pos ARRIBA [%d] ultima posicion %d hilo %d", (posAux - numCol), ultima_posicion, pos);
                    posAux = posAux - numCol;
                    index += 1;

                    encontrado = backtrackingA(dev_tablero, dev_caminos, dev_camino, numFila, numCol, dev_index, index, pos_encontrar, encontrado, posAux, filaActual, colActual, ultima_posicion, pos, camino_invalido, num_camino);

                    if (encontrado)
                    {
                        printf("\nCamino ENCONTRADO %d\n", pos);
                        index += 1;
                        dev_camino[index] = pos_encontrar;
                        num_camino = atomicAdd(&dev_index[0], 1);
                        printf("\nPosible camino num_camino[%d] (hilo %d) de tamano %d\n", num_camino, pos, index);

                        __syncthreads(); // Sincronizamos los hilos del bloque para asegurarnos de que se complete la operación atomicAdd
                        dev_caminos[num_camino] = dev_camino;
                        encontrado = true;

                        return encontrado;
                    }

                    index--; //Elimina ultimo elemento si no encuentra camino para volver atras
                }
                else
                {
                    printf("\nCamino no encontrado desde la posicion %d\n", pos);
                    camino_invalido = true;
                    dev_tablero[posAux] = dev_tablero[ultima_posicion]; //Vuelve atras
                }

                
            }

        }
    }
}


__global__ void kernelEncontrarCaminosBack(int* dev_tablero, int** dev_caminos, int* dev_camino, int numFila, int numCol, int* dev_index, int cX, int cY)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;        //Posicion en la que nos encontramos
    int pos_encontrar = cX * numFila + cY;                  //Posicion a ENCONTRAR en el vector 1D

    //int* dev_camino;
    int num_camino = dev_index[0];

    bool encontrado = false;
    bool camino_invalido = false;
    int posAux;
    int index = 0;

    //Recorrer 1º fila y 2ºColumna del tablero en la que se encuentra la celda de POS
    int filaActual = pos / numCol;
    int colActual = pos - filaActual * numCol;
    int ultima_posicion = pos;

    if (dev_tablero[pos] == dev_tablero[pos_encontrar])
    {
        backtrackingA(dev_tablero, dev_caminos, dev_camino, numFila, numCol, dev_index, index, pos_encontrar, encontrado, posAux, filaActual, colActual, ultima_posicion, pos, camino_invalido, num_camino);
        __syncthreads();
    }
    else
    {
        printf("Color no corresponde hilo %d \n", pos);
    }
}
    

    __global__ void kernelEncontrarCaminos(int* dev_tablero, int** dev_caminos, int* dev_camino, int numFila, int numCol, int* dev_index, int cX, int cY)
    {
        int pos = blockIdx.x * blockDim.x + threadIdx.x;        //Posicion en la que nos encontramos
        int pos_encontrar = cX * numFila + cY;                  //Posicion a ENCONTRAR en el vector 1D

        //int* dev_camino;
        int num_camino = dev_index[0];

        bool encontrado = false;
        bool camino_invalido = false;
        int posAux;
        int index = 0;

        //Recorrer 1º fila y 2ºColumna del tablero en la que se encuentra la celda de POS
        int filaActual = pos / numCol;
        int colActual = pos - filaActual * numCol;
        int ultima_posicion = pos;

        if (dev_tablero[pos] == dev_tablero[pos_encontrar])
        {
            posAux = pos;
            printf("\nHilo numero %d - posicion auxiliar inicial %d \n", pos, posAux);

            while ((posAux < numCol * numFila) && !encontrado && !camino_invalido)
            {
                int sigfila = (posAux + 1) / numCol;                 //Fila en la que se encuentra el siguiente elemento
                int sigcol = (posAux + 1) - sigfila * numCol;       //Columna en la que se encuentra el siguiente elemento

                int fila_anterior = (posAux - 1) / numCol;                 //Fila en la que se encuentra el elemento ANTERIOR
                int col_anterior = (posAux - 1) - fila_anterior * numCol; //Columna en la que se encuentra el elemento anterior


                colActual = sigcol - 1;                     //Columna de la posicion actual
                int posSigFila = (posAux + numCol) / numCol;



                // printf("\n DERECHA- fila [%d] col [%d] hilo %d\n", sigfila, sigcol, pos);
                // printf("\n ABAJO - fila[%d] col[%d] (hilo %d)\n", posSigFila, colActual, pos);
                //printf("\n IZQUIEDA - fila[%d] col[%d] posicion %d (hilo %d) ultima pos %d\n", posSigFila, col_anterior, posAux - 1, pos, ultima_posicion);
                // printf("\n Condicion derecha: hilo %d va la columna %d (desde posAux %d )\n", pos, posAux+1, posAux);
                // printf("\n Condicion abajo: hilo %d desde: posAux %d va a la columna %d\n", pos, posAux, posAux + numCol);

                //Si son del mismo color
                if (posAux == pos_encontrar)
                {
                    printf("\nCamino ENCONTRADO %d\n", pos);
                    index += 1;
                    dev_camino[index] = pos_encontrar;
                    num_camino = atomicAdd(&dev_index[0], 1);
                    printf("\nPosible camino num_camino[%d] (hilo %d) de tamano %d\n", num_camino, pos, index);

                    __syncthreads(); // Sincronizamos los hilos del bloque para asegurarnos de que se complete la operación atomicAdd
                    dev_caminos[num_camino] = dev_camino;
                    encontrado = true;
                }

                else if (dev_tablero[pos] == dev_tablero[posAux + 1] && sigcol >= 0 && (posAux + 1) != ultima_posicion)          //Nos desplazamos a la derecha
                {
                    printf("\nAvanza a la pos DERECHA [%d] hilo %d", posAux, pos);
                    dev_camino[index] = posAux;
                    dev_tablero[posAux] = -1;
                    ultima_posicion = posAux;
                    posAux += 1;
                    index += 1;
                    printf("\nContinua por posicion (fila) %d hilo %d\n", posAux, pos);

                }
                else if (dev_tablero[pos] == dev_tablero[posAux + numCol] && (posAux + numCol) != ultima_posicion && (posAux + numCol) < numCol * numFila)  //Nos desplazamos hacia abajo
                {
                    ultima_posicion = posAux;
                    dev_tablero[posAux] = -1;
                    printf("\nAvanza a la pos de ABAJO [%d] ultima posicion %d hilo %d", posAux + numCol, ultima_posicion, pos);
                    dev_camino[index] = posAux;
                    posAux = posAux + numCol;
                    //printf("\nPos aux vale: %d hilo %d", posAux, pos);
                    index += 1;

                    // printf("\nContinua por posicion (columna)  %d hilo %d\n", posAux, pos);

                }
                else if (dev_tablero[pos] == dev_tablero[posAux - 1] && col_anterior >= 0 && posAux - 1 >= numCol && (posAux - 1) != ultima_posicion)           //Izquierda
                {
                    dev_camino[index] = posAux;
                    ultima_posicion = posAux;
                    dev_tablero[posAux] = -1;
                    posAux = posAux - 1;
                    printf("\nAvanza a la pos IZQUIERDA [%d] hilo %d", posAux, pos);
                    index += 1;

                }
                else if (dev_tablero[pos] == dev_tablero[posAux - numCol] && (posAux - numCol) != ultima_posicion && posAux >= numCol && (posAux - numCol) >= 0)  //ARRIBA
                {
                    //printf("\nAvanza a la pos de ARRIBA [%d] hilo %d", posAux, pos);
                    dev_camino[index] = posAux;
                    ultima_posicion = posAux;
                    dev_tablero[posAux] = -1;
                    printf("\nAvanza a la pos ARRIBA [%d] ultima posicion %d hilo %d", (posAux - numCol), ultima_posicion, pos);
                    posAux = posAux - numCol;
                    index += 1;
                }
                else
                {
                    printf("\nCamino no encontrado desde la posicion %d\n", pos);
                    camino_invalido = true;
                }


                __syncthreads();

            }
        

    }
}


//Inicializamos el tablero
int* inicializarTablero(int* h_tablero, int size, int dificultad)
{
    int* (dev_Tablero);

    //Reservar espacio en memoria para GPU (2 matrices y matriz resultado)
    cudaMalloc((void**)&dev_Tablero, size * sizeof(int));

    //Copiamos datos a la GPU 
    cudaMemcpy(dev_Tablero, h_tablero, size * sizeof(int), cudaMemcpyHostToDevice);

    dim3 threadsInBlock(size);

    unsigned int semilla = time(NULL);
    kernelGenerarTablero << <1, threadsInBlock >> > (dev_Tablero, semilla, dificultad);

    // Copiamos de la GPU a la CPU
    cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);

    return h_tablero;

}

//Funcion que llama a kernel para encontrar todos los caminos hacia bloque indicado
void encontrarCamino(int* h_tablero, int numFilas, int numColumnas, int coordX, int coordY)
{
    int** h_caminos;
    int* (dev_Tablero), ** (dev_caminos), * (dev_index), * (dev_camino);
    int size = numFilas * numColumnas;
    int* camino_posible;


    int* h_index = { 0 };

    h_caminos = (int**)malloc(numFilas * numColumnas * sizeof(int));
    cudaMalloc((void**)&camino_posible, size * sizeof(int));

    //Reservar espacio en memoria para GPU (2 matrices y matriz resultado)
    cudaMalloc((void**)&dev_Tablero, size * sizeof(int));
    cudaMalloc((void**)&dev_caminos, size * sizeof(int));
    cudaMalloc((void**)&dev_camino, size * sizeof(int));
    cudaMalloc((void**)&dev_index, sizeof(int));

    //Copiamos datos a la GPU 
    cudaMemcpy(dev_Tablero, h_tablero, size * sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_caminos, h_caminos, size * sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_camino, camino_posible, size * sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_index, h_index, sizeof(int), cudaMemcpyHostToDevice);

    dim3 threadsInBlock(size);
    //kernelEncontrarCaminos << <1, threadsInBlock >> > (dev_Tablero, dev_caminos, dev_camino, numFilas, numColumnas, dev_index, coordX, coordY);
    kernelEncontrarCaminosBack << <1, threadsInBlock >> > (dev_Tablero, dev_caminos, dev_camino, numFilas, numColumnas, dev_index, coordX, coordY);

    // Copiamos de la GPU a la CPU
    cudaMemcpy(h_caminos, dev_caminos, size * sizeof(int), cudaMemcpyDeviceToHost);
    //cudaMemcpy(h_index, dev_index, sizeof(int), cudaMemcpyDeviceToHost);
    //printf("H_INDEX: %d", h_index[0]);

    cudaFree(dev_Tablero);
    cudaFree(dev_caminos);
    cudaFree(dev_index);
    cudaFree(dev_camino);
    cudaFree(camino_posible);
    //Muestra caminoS
    
    int filas = sizeof(caminos) / sizeof(caminos[0]);
    for (int i = 0; i < indice; i++)
    {
        printf("Camino %d :\n", i + 1);
        int columnas = sizeof(caminos[i]) / sizeof(caminos[i][0]);
        for (int j = 0; j < columnas; j++)
        {
            printf("%d ", caminos[i][j]);
        }
        printf("/n");
    }


}




void main(int argc, char* argv[])
{
    //Declaracion variables
    //int* h_tablero;
    int numFilas = 5;
    int numColumnas = 5;
    int coordenadaX;
    int coordenadaY;
    int size = numFilas * numColumnas;
    int dificultad = 4;
    bool terminado = false;

    //Pasamos a memoria constante el numero de filas y columnas introducidas por el usuario
    cudaMemcpyToSymbol(FILAS, &numFilas, sizeof(int));
    cudaMemcpyToSymbol(COLUMNAS, &numColumnas, sizeof(int));

    //Reservamos memoria para el tablero, ya que no esta inicializado
   // h_tablero = (int*)malloc(numFilas * numColumnas * sizeof(int));

    //Llamamos a la funcion que inicializa con valores aleatorios el tablero
    //h_tablero = inicializarTablero(h_tablero, size, dificultad);
    int h_tablero[25] = { 3,3,3,3,4,3,3,4,3,1,4,3,4,3,1,3,1,3,3,3,4,1,1,4,3 };
    //Mostramos el tablero
    mostrarTablero(h_tablero, numFilas, numColumnas);

    while (!terminado) {
        printf("\nIntroduzca las coordenadas del bloque que desea eliminar (x, y):  \n");
        scanf("%d %d", &coordenadaX, &coordenadaY);
        encontrarCamino(h_tablero, numFilas, numColumnas, coordenadaX, coordenadaY);
    }

}



/*
//Crea un nuevo tablero aleatorio del tamaño indicado
int** generarTablero(int numFilas, int numColumnas)
{
    //Matriz que representa el tablero
    int** tablero = (int **)malloc(numFilas * sizeof(int));
    // Para que no se obtengan los mismos números aleatorios
    srand((unsigned int)time(NULL));

    for (int i = 0; i < numFilas; i++)
    {
        tablero[i] = (int *)malloc(numColumnas * sizeof(int));
        for (int j = 0; j < numColumnas; j++)
        {
            tablero[i][j] = (rand() % 6) + 1;   //Rellena tablero con numeros aleatorios entre 1 y 6
        }
    }
    mostrarTablero(tablero, numFilas, numColumnas); //Llama a funcion para mostrar tablero por pantalla
    return tablero; //devuelve el tablero creado
}
*/