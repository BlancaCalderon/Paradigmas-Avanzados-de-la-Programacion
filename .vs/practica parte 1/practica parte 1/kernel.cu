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

__global__ void kernelReemplazarPosiciones(int* dev_tablero, int numFila, int numCol, int dev_semilla, int dificultad, int* dev_index)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;        //Posicion en la que nos encontramos
    dev_index[0] = 0;
    curandState_t state;
    curand_init(dev_semilla, pos, 0, &state); //curand_init(semilla, secuencia, offset, estado) secuencia dgenera diferentes secuencias de numeros aleatorio a partir de la misma semilla y offset genera numeros aleatorio s a partir de una secuencia y una semilla  CurandState curandState;
    int color = abs((int)(curand(&state) % dificultad) + 1);  //Rellena tablero con numeros aleatorios entre 1 y 6
    if (dev_tablero[pos] == -1) 
    {
        
        int filaActual = pos / numCol;
        int colActual = pos - filaActual * numCol;
        printf("Hilo %d [%d][%d] \n", pos, filaActual, colActual);
        if (filaActual > 0 && filaActual <= numFila && dev_tablero[pos - numCol] != -1)
        {
            __syncthreads();
            printf("Hilo COGE COLOR %d \n", pos);
            dev_tablero[pos] = dev_tablero[pos - numCol];
            dev_tablero[pos - numCol] = -1;
            atomicAdd(&dev_index[0], 1);
            __syncthreads();
        }
        else if (dev_tablero[pos - numCol] != -1)
        {
            printf("Hilo GENERA COLOR NUEVO %d \n", pos);
            dev_tablero[pos] = color;
        }
    }
    __syncthreads();
}
 __global__ void kernelEncontrarCaminos(int* dev_tablero, int* dev_camino, int numFila, int numCol, int* dev_index, int pos_encontrar, bool* dev_encontrado, int color)
    {
        int pos = blockIdx.x * blockDim.x + threadIdx.x;        //Posicion en la que nos encontramos
        bool encontrado;
        bool camino_invalido = false;
        int posAux;
        int index = 0;

        //Recorrer 1º fila y 2ºColumna del tablero en la que se encuentra la celda de POS
        int filaActual = pos / numCol;
        int colActual = pos - filaActual * numCol;
        int ultima_posicion = pos;

        if (dev_tablero[pos] == color || dev_tablero[pos] == -1 && pos_encontrar == pos)
        {
          //  printf("POS A ENCONTRAR %d \n", pos_encontrar);
            encontrado = false;
            posAux = pos;
         //   printf("\nHilo numero %d - posicion auxiliar inicial %d \n", pos, posAux);

            while ((posAux < numCol * numFila) && !encontrado && !camino_invalido)
            {
                int sigfila = (posAux + 1) / numCol;                 //Fila en la que se encuentra el siguiente elemento
                int sigcol = (posAux + 1) - sigfila * numCol;       //Columna en la que se encuentra el siguiente elemento

                int fila_anterior = (posAux - 1) / numCol;                 //Fila en la que se encuentra el elemento ANTERIOR
                int col_anterior = (posAux - 1) - fila_anterior * numCol; //Columna en la que se encuentra el elemento anterior

                int posSigFila = (posAux + numCol) / numCol;
                int fila_actual = posAux / numCol;
                int col_actual = posAux - fila_actual * numCol;

                // printf("\n DERECHA- fila [%d] col [%d] hilo %d\n", sigfila, sigcol, pos);
                // printf("\n ABAJO - fila[%d] col[%d] (hilo %d)\n", posSigFila, col_actual, pos);
                // printf("\n IZQUIEDA - fila[%d] col[%d] posicion %d (hilo %d) posicion ACTUAL %d\n", fila_actual, col_anterior, posAux - 1, pos, posAux);
                // printf("\n Condicion derecha: hilo %d va la columna %d (desde posAux %d )\n", pos, posAux+1, posAux);
                // printf("\n Condicion abajo: hilo %d desde: posAux %d va a la columna %d\n", pos, posAux, posAux + numCol);

                if (color == dev_tablero[posAux + 1]  && sigcol >= 0 && (posAux + 1) != ultima_posicion)          //Nos desplazamos a la derecha
                {
                    
                    printf("\nAvanza a la pos DERECHA [%d] hilo %d", posAux, pos);
                    index += 1;
                    ultima_posicion = posAux;
                    posAux += 1;
                    dev_tablero[posAux] = -1;
                    printf("\nContinua por posicion (fila) %d hilo %d\n", posAux, pos);

                }
                else if (color == dev_tablero[posAux + numCol] && (posAux + numCol) != ultima_posicion && (posAux + numCol) < numCol * numFila)  //Hacia abajo
                {
                    ultima_posicion = posAux;
                    printf("\nAvanza a la pos de ABAJO [%d] ultima posicion %d hilo %d", posAux + numCol, ultima_posicion, pos);
                    posAux = posAux + numCol;
                    index += 1;
                   
                    dev_tablero[posAux] = -1;
                }
                else if (color == dev_tablero[posAux - 1] && col_anterior >= 0 && (posAux - 1) != ultima_posicion)           //Izquierda
                {
                    index += 1;
                    ultima_posicion = posAux;
                    posAux = posAux - 1;
                    printf("\nAvanza a la pos IZQUIERDA [%d] hilo %d", posAux, pos);
                    
                    dev_tablero[posAux] = -1;
                }
                else if (color == dev_tablero[posAux - numCol] && (posAux - numCol) != ultima_posicion && (posAux - numCol) >= 0 && filaActual >= 0 && filaActual <= numFila)  //ARRIBA
                {
                    //printf("\nAvanza a la pos de ARRIBA [%d] hilo %d", posAux, pos);
                    /*
                    if (posAux != pos) {
                        dev_camino[dev_index[0]] = posAux;
                        atomicAdd(&dev_index[0], 1);
                        
                    }
                    */
                    index += 1;
                    ultima_posicion = posAux;
                    printf("\nAvanza a la pos ARRIBA [%d] ultima posicion %d hilo %d", (posAux - numCol), ultima_posicion, pos);
                    posAux = posAux - numCol;
                    
                    dev_tablero[posAux] = -1;
                }
                else
                {
                    printf("\nNumero elementos %d\n", dev_index[0]);
                    
                        printf("\nCamino ENCONTRADO %d\n", pos);
                        //dev_camino[dev_index[0]]
                        if (index > 0) {
                            dev_camino[dev_index[0]] = pos;
                            atomicAdd(&dev_index[0], 1);
                            encontrado = true;
                        }
                        else {
                            encontrado = false;
                        }
                        __syncthreads();
                        for (int i = 0; i < dev_index[0]; i++)
                        {
                            printf(" Camino %d   ", dev_camino[i]);
                        }

                    printf("\nCamino no encontrado desde la posicion %d\n", posAux);
                    camino_invalido = true;
                }

            }
            //atomicOr(dev_encontrado, encontrado);
            dev_encontrado[0] = encontrado;
            printf("DEV_ENCONTRADO %d \n", dev_encontrado[0]);
            printf("DEV_INDEX %d \n", dev_index[0]);
            
        }
        __syncthreads();
        
    
}

//kernel usado para eliminar las posiciones encontradas
__global__ void kernelEliminar(int* dev_tablero, int* posiciones, int numFila, int numCol, int numPosiciones)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;

    for (int i = 0; i < numPosiciones; i++)    
    {
        if (pos == posiciones[i])
        {
            dev_tablero[pos] = 0; //Sustitute valor de la casilla a borrar por 0 
        }
    }
}

//kernel que cambia las posiciones eliminadas por la casilla de encima o un numero aleatorio en el caso de que no lo haya
__global__ void kernelRellenar(int* dev_tablero, int numFila, int numCol)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;
    for (int i = numFila * numCol; i > 0; i--)  //Recorre de forma inversa el tablero
    {
        printf("RELLLLENAR");
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
void encontrarCamino(int* h_tablero_original, int numFilas, int numColumnas, int coordX, int coordY, int dificultad)
{   
    int* h_tablero = h_tablero_original;
    int* h_caminos;
    int* (dev_Tablero), * (dev_index), * (dev_camino);
    bool* dev_encontrado;
    int size = numFilas * numColumnas;
    bool h_encontrado = true;
    int* h_index = { 0 };
    int pos_encontrar = coordX * numFilas + coordY;   //Posicion a ENCONTRAR en el vector 1D
    int color = h_tablero[pos_encontrar];
    h_caminos = (int*)malloc(size * sizeof(int));
    //h_index = (int*)malloc(sizeof(int));
    //cudaMalloc((void**)&h_caminos, size * sizeof(int));

    //Reservar espacio en memoria para GPU (2 matrices y matriz resultado)
    cudaMalloc((void**)&dev_Tablero, size * sizeof(int));
    cudaMalloc((void**)&dev_camino, size * sizeof(int));
    cudaMalloc((void**)&dev_index, sizeof(int));
    cudaMalloc(&dev_encontrado, sizeof(bool));

    //Copiamos datos a la GPU 
    cudaMemcpy(dev_Tablero, h_tablero, size * sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_camino, h_caminos, size * sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_index, h_index, sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_encontrado, &h_encontrado, sizeof(bool), cudaMemcpyHostToDevice);

    dim3 threadsInBlock(size);
    while (h_encontrado) {
        kernelEncontrarCaminos << <1, threadsInBlock >> > (dev_Tablero, dev_camino, numFilas, numColumnas, dev_index, pos_encontrar, dev_encontrado, color);
        cudaMemcpy(h_caminos, dev_camino, size * sizeof(int), cudaMemcpyDeviceToHost);
        cudaMemcpy(&h_encontrado, dev_encontrado, sizeof(bool), cudaMemcpyDeviceToHost);
        cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
        cudaMemcpy(&h_index, dev_index, sizeof(int), cudaMemcpyDeviceToHost);
        printf("Valor del puntero %d \n", h_encontrado);
        printf("H_inxex %d\n", h_index);
        for (int i = 0; i < (int) h_index; i++) {
            printf("Camino \n", &h_caminos[i]);
        }
        mostrarTablero(h_tablero, numFilas, numColumnas);
    }

    unsigned int semilla = time(NULL);
    h_index = { 0 };
    int iteraciones = 10;
    while (iteraciones > 0) 
    {
        //h_index = { 0 };
        kernelReemplazarPosiciones << <1, threadsInBlock >> > (dev_Tablero, numFilas, numColumnas, semilla, dificultad, dev_index);
        cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
        cudaMemcpy(&h_index, dev_index, sizeof(int), cudaMemcpyDeviceToHost);
        mostrarTablero(h_tablero, numFilas, numColumnas);
        iteraciones = (int) h_index;
        printf("Iteraciones %d\n", iteraciones);

    }
    

    cudaFree(dev_encontrado);
    cudaFree(dev_Tablero);
    cudaFree(dev_index);
    cudaFree(dev_camino);

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
        encontrarCamino(h_tablero, numFilas, numColumnas, coordenadaX, coordenadaY, dificultad);
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