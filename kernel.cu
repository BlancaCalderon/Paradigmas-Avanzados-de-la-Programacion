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
__constant__ int* c_mask_camino;

//Kernel que lleva a cabo la generacion del tablero de forma aleatoria
__global__ void kernelGenerarTablero(int* dev_tablero, int dev_semilla, int dificultad)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;
    curandState_t state;
    curand_init(dev_semilla, pos, 0, &state); //curand_init(semilla, secuencia, offset, estado) secuencia dgenera diferentes secuencias de numeros aleatorio a partir de la misma semilla y offset genera numeros aleatorio s a partir de una secuencia y una semilla  CurandState curandState;
    dev_tablero[pos] = abs((int)(curand(&state) % dificultad) + 1);  //Rellena tablero con numeros aleatorios entre 1 y 6
}

__global__ void kernelEliminarBloque(int* dev_tablero, int ** camino_final, int tamFila, int tamCol, int cX, int cY)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;        //Posicion en la que nos encontramos
    int pos_encontrar = cX * tamFila + cY;                  //Posicion a ENCONTRAR en el vector 1D
    bool encontrado = false;
    bool camino_invalido = false;
    int * posibleCamino;                                    //Guarda el posible camino para cada posicion
    __shared__ int ** s_camino_final;                       //Lista de caminos finales que encuentran la posicion buscada
    __shared__ int num_camino;
    int posAux = pos + 1;
    int index = 0;
            //Recorrer 1� fila y 2�Columna del tablero en la que se encuentra la celda de POS
    int fila = posAux / tamCol;
    int col = posAux - fila * tamCol;
    if (dev_tablero[pos] == dev_tablero[pos_encontrar]) 
    {

            while ( (posAux % tamCol > 0) || (col < tamCol) && (pos <  (int)FILAS * (int)COLUMNAS ) && !encontrado && !camino_invalido) 
            {
                fila = posAux / tamCol;
                col = posAux - fila * tamCol;

                //Si son del mismo color
                if (dev_tablero[pos] == dev_tablero[posAux]) 
                {
                    posibleCamino[index] = posAux;
                    posAux += 1;
                    index += 1;
                    
                }
                else if (dev_tablero[pos] == dev_tablero[fila*tamFila + col])
                {
                    posibleCamino[index] = fila * tamFila + col;
                    posAux += 1;
                    index += 1;
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

__global__ void kernelEncontrarCamino(int* dev_tablero, int* dev_camino, int numFila, int numCol, int* dev_index, int cX, int cY)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;        //Posicion en la que nos encontramos
    int pos_encontrar = cX * numFila + cY;                  //Posicion a ENCONTRAR en el vector 1D


    int new_index = dev_index[0];
    printf("\nPosicion a encontrar[%d][%d]: %d\n", cX, cY, pos_encontrar);
    printf("\nPosicion actual:%d\n ", pos);


    printf("\nComparamos colores [%d] [%d]: %d\n", dev_tablero[pos], dev_tablero[pos_encontrar]);

    if (dev_tablero[pos] == dev_tablero[pos_encontrar])
    {
        new_index = atomicAdd(&dev_index[0], 1);
        printf("\nPosible camino [%d] = %d\n", new_index, pos);

        __syncthreads(); // Sincronizamos los hilos del bloque para asegurarnos de que se complete la operaci�n atomicAdd
        dev_camino[new_index] = pos;
    }
    __syncthreads();

}

__global__ void kernelEncontrarCaminos(int* dev_tablero, int** dev_caminos, int numFila, int numCol, int *dev_index, int cX, int cY)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;        //Posicion en la que nos encontramos
    int pos_encontrar = cX * numFila + cY;                  //Posicion a ENCONTRAR en el vector 1D

    int* dev_camino;
    int num_camino = dev_index[0];

    bool encontrado = false;
    bool camino_invalido = false;
    int posAux = pos + 1;
    int index = 0;

    //Recorrer 1� fila y 2�Columna del tablero en la que se encuentra la celda de POS
    int fila = posAux / numCol;
    int col = posAux - fila * numCol;


    if (dev_tablero[pos] == dev_tablero[pos_encontrar] && pos != pos_encontrar)
    {
        printf("\nHilo numero %d coincide\n", pos);
        printf("\nComparamos colores [%d] [%d] en la posicion actual %d\n", dev_tablero[pos], dev_tablero[pos_encontrar], pos);

        while ((posAux % numCol > 0) || (col < numCol) && (pos < (int)FILAS * (int)COLUMNAS) && !encontrado && !camino_invalido) 
        {
            fila = posAux / numCol;         //Fila en la que se encuentra el siguiente elemento
            col = posAux - fila * numCol;   //Columna en la que se encuentra el siguiente elemento

            printf("\n FILA  siguiente %d hilo %d\n", fila, pos);
            printf("\n COLUMNA siguiente %d hilo %d\n", col, pos);

            
            //Si son del mismo color
            if (posAux == pos_encontrar)
            {
                printf("\nCamino ENCONTRADO %d\n", pos);
                encontrado = true;
                num_camino = atomicAdd(&dev_index[0], 1);
                printf("\nPosible camino [%d] = %d\n", num_camino, pos);

                __syncthreads(); // Sincronizamos los hilos del bloque para asegurarnos de que se complete la operaci�n atomicAdd
                dev_caminos[num_camino] = dev_camino;
            }
            __syncthreads();

            printf("\n Condicion fila: hilo %d == %d\n", pos, posAux);
            printf("\n Condicion columna hilo %d hilo == %d\n", pos, posAux - 1 + numCol);

            if (dev_tablero[pos] == dev_tablero[posAux])
            {
                dev_camino[index] = posAux;
                posAux += 1;
                index += 1;
                printf("\nContinua por posicion (fila) %d hilo %d\n", posAux, pos);

            }
            else if (dev_tablero[pos] == dev_tablero[posAux - 1 + numCol])
            {
                dev_camino[index] = posAux - 1 + numCol;
                posAux = posAux - 1 + numCol + 1;
                index += 1;
                printf("\nContinua por posicion (columna)  %d hilo %d\n", fila * numFila + col, pos);
            }
            else 
            {
                printf("\nCamino no encontrado desde la posicion %d\n", pos);
                camino_invalido = true;
            }

            

        }
    }


}

//Eliminar bloque
void encontrarCamino(int* h_tablero, int numFilas, int numColumnas, int cX, int cY)
{
    int** h_caminos;
    int* (dev_Tablero), **(dev_caminos), *(dev_index);
    int size = numFilas * numColumnas;

    int* h_index = { 0 };
    h_caminos = (int**)malloc(numFilas * numColumnas * sizeof(int));

    
    //Reservar espacio en memoria para GPU (2 matrices y matriz resultado)
    cudaMalloc((void**)&dev_Tablero, size * sizeof(int));
    cudaMalloc((void**)&dev_caminos, size * sizeof(int));
    cudaMalloc((void**)&dev_index, sizeof(int));

    //Copiamos datos a la GPU 
    cudaMemcpy(dev_Tablero, h_tablero, size * sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_caminos, h_caminos, size * sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_index, h_index, sizeof(int), cudaMemcpyHostToDevice);

    dim3 threadsInBlock(size);
    kernelEncontrarCaminos << <1, threadsInBlock >> > (dev_Tablero, dev_caminos, numFilas, numColumnas, dev_index, cX, cY);

    // Copiamos de la GPU a la CPU
    cudaMemcpy(h_caminos, dev_caminos, size * sizeof(int), cudaMemcpyDeviceToHost);
    //cudaMemcpy(h_index, dev_index, sizeof(int), cudaMemcpyDeviceToHost);
    //printf("H_INDEX: %d", h_index[0]);

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
    encontrarCamino(h_tablero, numFilas, numColumnas, 3, 3);
   // while (!terminado) {
        printf("\nIntroduzca las coordenadas del bloque que desea eliminar (x, y):  \n");
        //scanf("%d %d", &coordenadaX, &coordenadaY);
        //encontrarCamino(int* h_tablero, int numFilas, int numColumnas, int cX, int cY)
        //encontrarCamino(h_tablero, numFilas, numColumnas, 3, 3);
   // }
    
}



/*
//Crea un nuevo tablero aleatorio del tama�o indicado
int** generarTablero(int numFilas, int numColumnas)
{
    //Matriz que representa el tablero
    int** tablero = (int **)malloc(numFilas * sizeof(int));
    // Para que no se obtengan los mismos n�meros aleatorios
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