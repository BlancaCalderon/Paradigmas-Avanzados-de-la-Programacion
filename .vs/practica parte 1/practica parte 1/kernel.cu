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

//Kernel que lleva a cabo la generacion del tablero de forma aleatoria
__global__ void kernelGenerarTablero(int* dev_tablero, int dev_semilla, int dificultad)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;
    curandState_t state;
    curand_init(dev_semilla, pos, 0, &state); //curand_init(semilla, secuencia, offset, estado) secuencia dgenera diferentes secuencias de numeros aleatorio a partir de la misma semilla y offset genera numeros aleatorio s a partir de una secuencia y una semilla  CurandState curandState;
    dev_tablero[pos] = abs((int)(curand(&state) % dificultad) + 1);  //Rellena tablero con numeros aleatorios entre 1 y 6
}

__global__ void kernelEncontrarCamino(int* dev_tablero, int** camino_final, int tamFila, int tamCol, int cX, int cY)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;        //Posicion en la que nos encontramos
    int pos_encontrar = cX * tamFila + cY;                  //Posicion a ENCONTRAR en el vector 1D
    bool encontrado = false;
    bool camino_invalido = false;
    int* posibleCamino;                                    //Guarda el posible camino para cada posicion
    __shared__ int** s_camino_final;                       //Lista de caminos finales que encuentran la posicion buscada
    __shared__ int num_camino;
    int posAux = pos + 1;
    int index = 0;
    //Recorrer 1º fila y 2ºColumna del tablero en la que se encuentra la celda de POS
    int fila = posAux / tamCol;
    int col = posAux - fila * tamCol;
    if (dev_tablero[pos] == dev_tablero[pos_encontrar])
    {

        while ((posAux % tamCol > 0) || (col < tamCol) && (pos < (int)FILAS * (int)COLUMNAS) && !encontrado && !camino_invalido)
        {
            fila = posAux / tamCol;
            col = posAux - fila * tamCol;
            printf("\n FILA %d\n", fila);
            printf("\n COLUMNA %d\n", col);

            //Si son del mismo color
            if (dev_tablero[pos] == dev_tablero[posAux])
            {
                posibleCamino[index] = posAux;
                posAux += 1;
                index += 1;
                printf("\nContinua por posicion (fila) %d\n", posAux);

            }
            else if (dev_tablero[pos] == dev_tablero[fila * tamFila + col])
            {
                posibleCamino[index] = fila * tamFila + col;
                posAux = fila * tamFila + col + 1;
                index += 1;
                printf("\nContinua por posicion (columna)  %d\n", fila * tamFila + col);
            }
            else
            {
                printf("\nCamino no encontrado desde la posicion %d\n", pos);
                camino_invalido = true;
            }

            if (pos == pos_encontrar)
            {
                s_camino_final[num_camino] = posibleCamino;
                encontrado = true;
                atomicAdd(&num_camino, 1);
                printf("\nCamino encontrado \n");
                for(int i = 0; i < sizeof(posibleCamino)/sizeof(posibleCamino[0]); i++)
                {
                    printf("%d ", posibleCamino[i]);
                }
                printf("\n");
            }
            __syncthreads();
        }

        camino_final = s_camino_final;
    }
    else
    {
        printf("\nEl color no coincide %d\n", pos);
    }

}

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
    int* (dev_Tablero);
    int** (dev_caminos);
    int size = numFilas * numColumnas;

    h_caminos = (int**)malloc(numFilas * numColumnas * sizeof(int));

    //Reservar espacio en memoria para GPU (2 matrices y matriz resultado)
    cudaMalloc((void**)&dev_Tablero, size * sizeof(int));
    cudaMalloc((void**)&dev_caminos, size * sizeof(int));

    //Copiamos datos a la GPU 
    cudaMemcpy(dev_Tablero, h_tablero, size * sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_caminos, h_caminos, size * sizeof(int), cudaMemcpyHostToDevice);

    dim3 threadsInBlock(size);

    kernelEncontrarCamino << <1, threadsInBlock >> > (dev_Tablero, dev_caminos, numFilas, numColumnas, coordX, coordY);

    // Copiamos de la GPU a la CPU
    cudaMemcpy(h_caminos, dev_caminos, size * sizeof(int), cudaMemcpyDeviceToHost);

    //Muestra caminoS
    /*
    int filas = sizeof(h_caminos) / sizeof(h_caminos[0]);
    for (int i = 0; i < filas; i++)
    {
        printf("Camino %d :\n", i + 1);
        int columnas = sizeof(h_caminos[i]) / sizeof(h_caminos[i][0]);
        for (int j = 0; j < columnas; j++)
        {
            printf("%d ", h_caminos[i][j]);
        }
        printf("/n");
    }*/


}




void main(int argc, char* argv[])
{
    //Declaracion variables
    int* h_tablero;
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
    h_tablero = (int*)malloc(numFilas * numColumnas * sizeof(int));

    //Llamamos a la funcion que inicializa con valores aleatorios el tablero
    h_tablero = inicializarTablero(h_tablero, size, dificultad);

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