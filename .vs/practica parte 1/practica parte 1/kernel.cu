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
void mostrarTablero(int* tablero, int numFilas, int numColumnas, int dificultad)
{
    
    for (int i = 0; i < numFilas; i++)
    {
        for (int j = 0; j < numColumnas; j++)
        {
            int num = tablero[i * numFilas + j];
            if (num > dificultad)
            {
                if (7 <= num <= 13)
                {
                    printf("RC%d  ", num%7);
                }
                else
                {
                    printf("%c  ", (char)num);
                } 
            }
            else
            {
                printf("%d  ", num);
            }
            
        }
        printf("\n");
    }
    printf("\n");
}

//Kernel que elimina la fila y columna de la posicion pasada (Bomba)
__global__ void kernelBomba(int* dev_tablero, int numFila, int numCol, int pos_encontrar)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;        //Posicion en la que nos encontramos

    //Calcula fila y columna de la posición actual
    int filaActual = pos / numCol;
    int colActual = pos - filaActual * numCol;

    //Calcula fila y columna a borrar
    int filaBorrar = pos_encontrar / numCol;
    int colBorrar = pos_encontrar - filaBorrar * numCol;

    //si posición actual esta en la fila o columna que queremos borrar
    if (filaBorrar == filaActual || colBorrar == colActual && 0 <= filaActual <= numFila && 0 <= colActual <= numCol)
    {
        dev_tablero[pos] = -1; //Indicamos que se borra
    }
    __syncthreads(); //Esperamos a que todos los hilos del mismo bloque hayan ejecutado el if antes de establecer la posicion a encontrar en -1
    dev_tablero[pos_encontrar] = -1;              //Eliminamos bloque especial
}


//Kernel que elimina los elementos adyacentes a una posición (radio 4 elementos) (TNT)
__global__ void kernelTNT(int* dev_tablero, int numFila, int numCol, int pos_encontrar)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;        //Posicion en la que nos encontramos

    //Calcula fila y columna de la posición actual
    int filaActual = pos / numCol;
    int colActual = pos - filaActual * numCol;

    //Calcula fila y columna a borrar teniendo en cuenta el rango
    int filaBorrar = pos_encontrar / numCol;
    int colBorrar = pos_encontrar - filaBorrar * numCol;

    int filaBorrarDer = filaBorrar + 4;
    int colBorrarAbajo = colBorrar + 4;
    int filaBorrarIzq = filaBorrar - 4;
    int colBorrarArriba = colBorrar  - 4;

    //si posición actual es adyacente y esta dentro del rango que queremos borrar (4)
    if (filaBorrarIzq <= filaActual <= filaBorrarDer && colBorrarArriba <= colActual <= colBorrarAbajo &&  0 <= filaActual <= numFila && 0 <= colActual <= numCol)     
    {
        dev_tablero[pos] = -1; //Indicamos que se borra
    }

    __syncthreads(); //Esperamos a que todos los hilos del mismo bloque hayan ejecutado el if antes de establecer la posicion a encontrar en -1
    dev_tablero[pos_encontrar] = -1;              //Eliminamos bloque especial
}


//Kernel que elimina todos las posiciones del color indicado (ROMPECABEZAS)
__global__ void kernelRompeCabezas(int* dev_tablero, int numFila, int numCol, int color, int pos_encontrar)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;        //Posicion en la que nos encontramos

    //Calcula fila y columna de la posición actual
    int filaActual = pos / numCol;
    int colActual = pos - filaActual * numCol;

    //si posición actual tiene el color indicado se elimina
    if (dev_tablero[pos] == color)
    {
        dev_tablero[pos] = -1; //Indicamos que se borra
    }

    __syncthreads(); //Esperamos a que todos los hilos del mismo bloque hayan ejecutado el if antes de establecer la posicion a encontrar en -1
    dev_tablero[pos_encontrar] = -1;              //Eliminamos bloque especial
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
    dev_index[0] = 0;                                       //Lo utilizamos para contabilizar el numero de llamadas que hay que realizar al kernel 
   
    if (dev_tablero[pos] == -1) 
    {
        
        int filaActual = pos / numCol;
        int colActual = pos - filaActual * numCol;
        if (filaActual > 0 && filaActual <= numFila && dev_tablero[pos - numCol] != -1)
        {
            __syncthreads();

            dev_tablero[pos] = dev_tablero[pos - numCol];
            dev_tablero[pos - numCol] = -1;
            atomicAdd(&dev_index[0], 1);        
            __syncthreads();
        }
        else if (dev_tablero[pos - numCol] != -1)
        {
            curandState_t state;
            curand_init(dev_semilla, pos, 0, &state); //curand_init(semilla, secuencia, offset, estado) secuencia dgenera diferentes secuencias de numeros aleatorio a partir de la misma semilla y offset genera numeros aleatorio s a partir de una secuencia y una semilla  CurandState curandState;
            int color = abs((int)(curand(&state) % dificultad) + 1);  //Rellena tablero con numeros aleatorios entre 1 y 6
            printf("COLOR %d\n", color);
            dev_tablero[pos] = color;
            atomicAdd(&dev_index[0], 1);
            __syncthreads();
        }
    }

}

 __global__ void kernelEncontrarCaminos(int* dev_tablero, int numFila, int numCol, int* dev_index, int pos_encontrar, bool* dev_encontrado, int color)
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

        if ((dev_tablero[pos] == color || dev_tablero[pos] == -1) && pos_encontrar == pos)
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

                        if (index > 0) {
                            atomicAdd(&dev_index[0], 1);
                            encontrado = true;
                        }
                        else {
                            encontrado = false;
                        }
                        __syncthreads();

                    printf("\nCamino no encontrado desde la posicion %d\n", posAux);
                    camino_invalido = true;
                }

            }
            dev_encontrado[0] = encontrado;
            printf("DEV_ENCONTRADO %d \n", dev_encontrado[0]);
            printf("DEV_INDEX %d \n", dev_index[0]);
            
        }

        __syncthreads(); //Esperamos a que todos los hilos del mismo bloque hayan ejecutado el if antes de establecer la posicion a encontrar en -1
        if (dev_index[0] >= 1)
        {
            dev_tablero[pos_encontrar] = -1;              //Establecemos la posicion a encontrar en -1
        }
        
        
    
}

 __global__ void kernelEncontrarBomba(int* dev_tablero, int numFila, int numCol, int pos_encontrar, int* dev_index_fila, int* dev_index_col)
 {
     int pos = blockIdx.x * blockDim.x + threadIdx.x;

     //Calcula fila y columna de la posición actual
     int filaActual = pos / numCol;
     int colActual = pos - filaActual * numCol;

     //Calcula fila y columna de la posición a encontrar
     int filaEncontrar = pos_encontrar / numCol;
     int colEncontrar = pos_encontrar - filaEncontrar * numCol;

     if (filaActual == filaEncontrar && (int)dev_index_fila > 5 && dev_tablero[pos] == -1)
     {
         atomicAdd(&dev_index_fila[0], 1);
     } 

     if (colActual == colEncontrar && (int)dev_index_col > 5 && dev_tablero[pos] == -1)
     {
         atomicAdd(&dev_index_col[0], 1);
     }

     __syncthreads();
     if (dev_index_fila[0] != dev_index_col[0])
     {
         printf("Valor del contador de fila %d y contador columna %d \n", dev_index_col[0], dev_index_fila[0]);
         if ((dev_index_fila[0] == 5 && dev_index_col[0] == 1) || (dev_index_col[0] == 5 && dev_index_fila[0] == 1))
         {
             dev_tablero[pos_encontrar] = 'B';
         }
     }
 }

 __global__ void kernelEncontrarRompecabezas(int* dev_tablero, int numFila, int numCol, int pos_encontrar, int* dev_index, int dev_semilla, int dificultad)
 {
     int pos = blockIdx.x * blockDim.x + threadIdx.x;

     //Calcula fila y columna de la posición actual
     int filaActual = pos / numCol;
     int colActual = pos - filaActual * numCol;

     //Calcula fila y columna de la posición a encontrar
     int filaEncontrar = pos_encontrar / numCol;
     int colEncontrar = pos_encontrar - filaEncontrar * numCol;

     if (dev_tablero[pos] == -1 && (int)dev_index > 7)
     {
         atomicAdd(&dev_index[0], 1);
     }
     __syncthreads();

     if (dev_index[0] >= 7 && pos == pos_encontrar)
     {
         printf("Valor del contador de rompecabezas %d \n", dev_index[0]);
         curandState_t state;
         curand_init(dev_semilla, pos, 0, &state); //curand_init(semilla, secuencia, offset, estado) secuencia dgenera diferentes secuencias de numeros aleatorio a partir de la misma semilla y offset genera numeros aleatorio s a partir de una secuencia y una semilla  CurandState curandState;
         int color = abs((int)(curand(&state) % dificultad) + 1);  //Rellena tablero con numeros aleatorios entre 1 y 6
         printf("Soy el hilo %d voy a actualizar el tablero \n ", pos);
         int colorS = 10 + color;
         dev_tablero[pos_encontrar] = colorS;
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
int encontrarCamino(int* h_tablero_original, int numFilas, int numColumnas, int coordX, int coordY, int dificultad, int vida)
{   
    int* h_tablero = h_tablero_original;
    int* (dev_Tablero), * (dev_index), *(dev_index_fila), *(dev_index_col), * (dev_index_RC);
    bool* dev_encontrado;
    int size = numFilas * numColumnas;
    bool h_encontrado = true;
    int* h_index = { 0 };
    int* h_index_col = { 0 };
    int* h_index_fila = { 0 };
    int* h_index_RC = { 0 };
    int pos_encontrar = coordX * numFilas + coordY;   //Posicion a ENCONTRAR en el vector 1D
    int color = h_tablero[pos_encontrar];
    unsigned int semilla = time(NULL);
    
    //Reservar espacio en memoria para GPU (2 matrices y matriz resultado)
    cudaMalloc((void**)&dev_Tablero, size * sizeof(int));
    cudaMalloc((void**)&dev_index, sizeof(int));
    cudaMalloc((void**)&dev_index_col, sizeof(int));
    cudaMalloc((void**)&dev_index_fila, sizeof(int));
    cudaMalloc((void**)&dev_index_RC, sizeof(int));
    cudaMalloc(&dev_encontrado, sizeof(bool));

    //Copiamos datos a la GPU 
    cudaMemcpy(dev_Tablero, h_tablero, size * sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_index, h_index, sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_index_col, h_index_col, sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_index_fila, h_index_fila, sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_index_RC, h_index_RC, sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_encontrado, &h_encontrado, sizeof(bool), cudaMemcpyHostToDevice);

    dim3 threadsInBlock(size);

    //Segun si es alguno de los bloques especiales o es una jugada normal (66 --> B, 84 --> T,
    switch (h_tablero[pos_encontrar])
    {
        case 'B':
            kernelBomba << <1, threadsInBlock >> > (dev_Tablero, numFilas, numColumnas, pos_encontrar);
            cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
            mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);
            break;

        case 'T':
            kernelTNT << <1, threadsInBlock >> > (dev_Tablero, numFilas, numColumnas, pos_encontrar);
            cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
            mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);
            break;

        case 'R1':
            kernelRompeCabezas<<<1, threadsInBlock >> > (dev_Tablero, numFilas, numColumnas, 1, pos_encontrar);
            cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
            mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);
            break;

        case 'R2':
            kernelRompeCabezas << <1, threadsInBlock >> > (dev_Tablero, numFilas, numColumnas, 2, pos_encontrar);
            cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
            mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);
            break;

        case 'R3':
            kernelRompeCabezas << <1, threadsInBlock >> > (dev_Tablero, numFilas, numColumnas, 3, pos_encontrar);
            cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
            mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);
            break;

        
        case 'R4':
            kernelRompeCabezas << <1, threadsInBlock >> > (dev_Tablero, numFilas, numColumnas, 4, pos_encontrar);
            cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
            mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);
            break;

        case 'R5':
            kernelRompeCabezas << <1, threadsInBlock >> > (dev_Tablero, numFilas, numColumnas, 5, pos_encontrar);
            cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
            mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);
            break;

        case 'R6':
            kernelRompeCabezas << <1, threadsInBlock >> > (dev_Tablero, numFilas, numColumnas, 6, pos_encontrar);
            cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
            mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);
            break;

        default:
            //Desde posición idicada se encuentran todos los caminos con el mismo color
            while (h_encontrado)
            {
                kernelEncontrarCaminos << <1, threadsInBlock >> > (dev_Tablero, numFilas, numColumnas, dev_index, pos_encontrar, dev_encontrado, color);
                cudaMemcpy(&h_encontrado, dev_encontrado, sizeof(bool), cudaMemcpyDeviceToHost);
                cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
                cudaMemcpy(&h_index, dev_index, sizeof(int), cudaMemcpyDeviceToHost);
                printf("Valor del puntero %d \n", h_encontrado);
                printf("H_inxex %d\n", h_index);
                
                mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);
            }
            if ((int)h_index == 0 && vida >= 1)
            {
                vida = vida - 1;
            }
            h_index_fila = { 0 };
            h_index_col = { 0 };
            mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);
            kernelEncontrarBomba << <1, threadsInBlock >> > (dev_Tablero, numFilas, numColumnas, pos_encontrar, dev_index_fila, dev_index_col);
            cudaMemcpy(&h_index_fila, dev_index_fila, sizeof(int), cudaMemcpyDeviceToHost);
            cudaMemcpy(&h_index_col, dev_index_col, sizeof(int), cudaMemcpyDeviceToHost);

            printf("N Filas %d - N Columnas %d \n", h_index_fila, h_index_col);
            mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);

            
            kernelEncontrarRompecabezas << <1, threadsInBlock >> > (dev_Tablero, numFilas, numColumnas, pos_encontrar, dev_index_RC, semilla, dificultad);
            cudaMemcpy(&h_index_RC, dev_index_RC, sizeof(int), cudaMemcpyDeviceToHost);
            cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
            mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);
            
    }

    
    h_index = { 0 };
    int iteraciones = 10;
    //Bucle para reemplazar las posiciones eliminadas mientras que se pueda hacer algun cambio y si no termine
    while (iteraciones > 0) 
    {
        kernelReemplazarPosiciones << <1, threadsInBlock >> > (dev_Tablero, numFilas, numColumnas, semilla, dificultad, dev_index);
        cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
        cudaMemcpy(&h_index, dev_index, sizeof(int), cudaMemcpyDeviceToHost);
        iteraciones = (int) h_index;
    }
    mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);
    
    cudaFree(dev_encontrado);
    cudaFree(dev_Tablero);
    cudaFree(dev_index);
    cudaFree(dev_index_fila);
    cudaFree(dev_index_col);

    return vida;
}

void main(int argc, char* argv[])
{
    //Declaracion variables
    //int* h_tablero;
    int numFilas = 3;
    int numColumnas = 9;
    int coordenadaX;
    int coordenadaY;
    int size = numFilas * numColumnas;
    int dificultad = 4;
    bool terminado = false;
    int vida = 5;
    char modoJuego = 'A';

    //Pasamos a memoria constante el numero de filas y columnas introducidas por el usuario
    cudaMemcpyToSymbol(FILAS, &numFilas, sizeof(int));
    cudaMemcpyToSymbol(COLUMNAS, &numColumnas, sizeof(int));

    //Reservamos memoria para el tablero, ya que no esta inicializado
   // h_tablero = (int*)malloc(numFilas * numColumnas * sizeof(int));

    //Llamamos a la funcion que inicializa con valores aleatorios el tablero
    //h_tablero = inicializarTablero(h_tablero, size, dificultad);
    //int h_tablero[25] = { 3,3,3,3,4,3,3,4,3,1,4,3,'B',3,1,3,1,3,3,3,4,1,1,4,3};
     int h_tablero[27] = { 3,3,3,3,3,3,3,4,4,4,4,3,1,3,1,3,1,3,3,3,4,1,1,4,3,4,4};
   // int h_tablero[25] = { 3,2,1,5,5,3,3,6,7,3,9,3,'B',3,1,3,1,3,3,3,4,1,1,4,3 };
    //Mostramos el tablero

    //Codigo para ejecutar programa y recibir datos por comando
    //Controla que no de error la llamada
     if (argc == 1)  //No se ha ejecutado por comando
     {
         printf("\nElija el modo de juego: A (Automatico) - M (Manual):  \n");
         scanf("%c", &modoJuego);
         printf("Modo de juego seleccionado: %c \n", modoJuego);
         mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);
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
    else if(argc > 5)
    {
        printf("ERROR: sobran argumentos de entrada\n");
    }
    else //Realiza llamada con los 4 argumemtos
    {
        //Guarda los argumentos pasadas en las respectivas variables
        modoJuego = (char)argv[1];
        dificultad = std::stoi(argv[2]);    //Guarda valor argumentos usando funcion stoi para convertirlo a int
        numFilas = std::stoi(argv[3]);
        numColumnas = std::stoi(argv[4]);
    }

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
            vida = encontrarCamino(h_tablero, numFilas, numColumnas, coordenadaX, coordenadaY, dificultad, vida);
            printf("\nVida restante: %d \n", vida);
        }
        else
        {
            printf("\nLas coordenadas introducidas se encuentran fuera del rango del tablero [%d][%d] \n", numFilas, numColumnas);
        }
        
        
    }
    printf("\nPERDEDOR \n");
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