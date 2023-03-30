
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


int vida = 5;

//Funcion que muestra el tablero por consola
void mostrarTablero(int* tablero, int numFilas, int numColumnas, int dificultad)
{
    int N = numFilas;
    int M = numColumnas;

    if (numFilas > numColumnas || numColumnas > numFilas) {
        N = numColumnas;
        M = numFilas;
    }

    for (int i = 0; i < numFilas; i++)
    {
        for (int j = 0; j < numColumnas; j++)
        {
            // printf("%d  ", tablero[i * N + j]);

            int num = tablero[i * N + j];
            if (num > dificultad)
            {
                if (7 <= num && num <= 13)
                {
                    printf("RC%d  ", num % 7);
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
    int colBorrarArriba = colBorrar - 4;

    //si posición actual es adyacente y esta dentro del rango que queremos borrar (4)
    if (filaBorrarIzq <= filaActual <= filaBorrarDer && colBorrarArriba <= colActual <= colBorrarAbajo && 0 <= filaActual <= numFila && 0 <= colActual <= numCol)
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
    int color = abs((int)(curand(&state) % dificultad) + 1);
    dev_tablero[pos] = color; //Rellena tablero con numeros aleatorios entre 1 y 6
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
            dev_tablero[pos] = color;
            atomicAdd(&dev_index[0], 1);
            __syncthreads();
        }
    }

}

__global__ void kernelEncontrarCaminos(int* dev_tablero, int numFila, int numCol, int* dev_index, int pos_encontrar, bool* dev_encontrado, int color)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;        //Posicion en la que nos encontramos
    bool encontrado = false;
    bool camino_invalido = false;
    int posAux;
    int index = 0;

    //Recorrer 1º fila y 2ºColumna del tablero en la que se encuentra la celda de POS
    int filaActual = pos / numCol;
    int colActual = pos - filaActual * numCol;
    int ultima_posicion = pos;

    if ((dev_tablero[pos] == color || dev_tablero[pos] == -1) && pos_encontrar == pos)
    {
        encontrado = false;
        posAux = pos;

        while ((posAux < numCol * numFila) && !encontrado && !camino_invalido)
        {
            int sigfila = (posAux + 1) / numCol;                 //Fila en la que se encuentra el siguiente elemento
            int sigcol = (posAux + 1) - sigfila * numCol;       //Columna en la que se encuentra el siguiente elemento

            int fila_anterior = (posAux - 1) / numCol;                 //Fila en la que se encuentra el elemento ANTERIOR
            int col_anterior = (posAux - 1) - fila_anterior * numCol; //Columna en la que se encuentra el elemento anterior

            int posSigFila = (posAux + numCol) / numCol;
            int fila_actual = posAux / numCol;
            int col_actual = posAux - fila_actual * numCol;

            if (color == dev_tablero[posAux + 1] && sigcol > 0 && (posAux + 1) != ultima_posicion)          //Nos desplazamos a la derecha
            {
                index += 1;
                ultima_posicion = posAux;
                posAux += 1;
                dev_tablero[posAux] = -1;
            }
            else if (color == dev_tablero[posAux + numCol] && (posAux + numCol) != ultima_posicion && (posAux + numCol) < numCol * numFila)  //Hacia abajo
            {
                ultima_posicion = posAux;
                posAux = posAux + numCol;
                index += 1;

                dev_tablero[posAux] = -1;
            }
            else if (color == dev_tablero[posAux - 1] && col_anterior > 0 && (posAux - 1) != ultima_posicion)           //Izquierda
            {
                index += 1;
                ultima_posicion = posAux;
                posAux = posAux - 1;
                dev_tablero[posAux] = -1;
            }
            else if (color == dev_tablero[posAux - numCol] && (posAux - numCol) != ultima_posicion && (posAux - numCol) >= 0 && filaActual > 0 && filaActual < numFila)  //ARRIBA
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
                else
                {
                    encontrado = false;
                }
                __syncthreads();
                camino_invalido = true;
            }

        }
        dev_encontrado[0] = encontrado;

    }
    dev_encontrado[0] = encontrado;

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
        if ((dev_index_fila[0] == 5 && dev_index_col[0] == 1) || (dev_index_col[0] == 5 && dev_index_fila[0] == 1))
        {
            dev_tablero[pos_encontrar] = 'B';
        }
    }
    dev_index_fila[0] = 0;
    dev_index_col[0] = 0;
}

__global__ void kernelEncontrarRompecabezasTNT(int* dev_tablero, int numFila, int numCol, int pos_encontrar, int* dev_index, int dev_semilla, int dificultad)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;

    //Calcula fila y columna de la posición actual
    int filaActual = pos / numCol;
    int colActual = pos - filaActual * numCol;

    //Calcula fila y columna de la posición a encontrar
    int filaEncontrar = pos_encontrar / numCol;
    int colEncontrar = pos_encontrar - filaEncontrar * numCol;

    if (dev_tablero[pos] == -1)
    {
        atomicAdd(&dev_index[0], 1);
    }
    __syncthreads();

    if (dev_index[0] == 6 && pos == pos_encontrar)
    {
        dev_tablero[pos_encontrar] = 'T';
    }
    else if (dev_index[0] >= 7 && pos == pos_encontrar)
    {
        curandState_t state;
        curand_init(dev_semilla, pos, 0, &state); //curand_init(semilla, secuencia, offset, estado) secuencia dgenera diferentes secuencias de numeros aleatorio a partir de la misma semilla y offset genera numeros aleatorio s a partir de una secuencia y una semilla  CurandState curandState;
        int color = abs((int)(curand(&state) % dificultad) + 1);  //Rellena tablero con numeros aleatorios entre 1 y 6
        int colorS = 7 + color;
        dev_tablero[pos_encontrar] = colorS;
    }
    dev_index[0];

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
    int* (dev_Tablero), * (dev_index), * (dev_index_fila), * (dev_index_col), * (dev_index_RC), * (dev_index_T);
    bool* dev_encontrado;
    int size = numFilas * numColumnas;
    bool h_encontrado = true;
    int* h_index = { 0 };
    int* h_index_col = { 0 };
    int* h_index_fila = { 0 };
    int* h_index_RC = { 0 };
    int pos_encontrar = coordX * numFilas + coordY;   //Posicion a ENCONTRAR en el vector 1D
    unsigned int semilla = time(NULL);

    if (numColumnas > numFilas) //Si matriz asimetrica con mas columnas que filas
    {
        pos_encontrar = coordX * numColumnas + coordY;   //Posicion a ENCONTRAR en el vector 1D   
    }

    int color = h_tablero[pos_encontrar];

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

    //Segun si es alguno de los bloques especiales o es una jugada normal (66 --> B, 84 --> T,)
    int contenido = h_tablero[pos_encontrar];

    if (contenido == 'B')
    {
        kernelBomba << <1, threadsInBlock >> > (dev_Tablero, numFilas, numColumnas, pos_encontrar);
        cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
    }

    else if (contenido == 'T')
    {
        kernelTNT << <1, threadsInBlock >> > (dev_Tablero, numFilas, numColumnas, pos_encontrar);
        cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
    }

    else if (7 <= contenido && contenido <= 13) //Si es RC
    {
        int colorBorrar = contenido % 7;
        kernelRompeCabezas << <1, threadsInBlock >> > (dev_Tablero, numFilas, numColumnas, colorBorrar, pos_encontrar);
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

                kernelEncontrarCaminos << <1, threadsInBlock >> > (dev_Tablero, numFilas, numColumnas, dev_index, pos_encontrar, dev_encontrado, color);
                cudaMemcpy(&h_encontrado, dev_encontrado, sizeof(bool), cudaMemcpyDeviceToHost);
                cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);
                cudaMemcpy(&h_index, dev_index, sizeof(int), cudaMemcpyDeviceToHost);;
            }

            if (h_tablero[cont] == -1)
            {
                pos_encontrar = cont;
                h_encontrado = 1;
            }
            cont += 1;
        }
        mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);

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


        kernelEncontrarRompecabezasTNT << <1, threadsInBlock >> > (dev_Tablero, numFilas, numColumnas, pos_encontrar, dev_index_RC, semilla, dificultad);
        cudaMemcpy(&h_index_RC, dev_index_RC, sizeof(int), cudaMemcpyDeviceToHost);
        cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);

    }


    h_index = { 0 };
    int iteraciones = 10;
    //Bucle para reemplazar las posiciones eliminadas mientras que se pueda hacer algun cambio y si no termine
    while (iteraciones > 0)
    {
        kernelReemplazarPosiciones << <1, threadsInBlock >> > (dev_Tablero, numFilas, numColumnas, semilla, dificultad, dev_index);
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
    int dificultad = 4;
    bool terminado = false;
    char modoJuego = 'A';

    //int h_tablero[27] = { 3,3,3,3,3,3,3,4,4,4,4,3,1,3,3,3,4,3,3,3,4,3,3,4,3,4,4 };
    //int h_tablero[27] = { 3,3,3,3,4,3,3,4,4,1,4,4,1,3,1,3,1,3,3,3,4,1,1,4,3 };
    //  int h_tablero[27] = { 3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,1,1,4,4,4,4 };
    //  mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);
      //int h_tablero[25] = { 3,2,1,5,5,3,3,6,7,3,9,3,'B',3,1,3,1,3,3,3,4,1,1,4,3 };

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
        modoJuego = (char)argv[1];
        dificultad = std::stoi(argv[2]);    //Guarda valor argumentos usando funcion stoi para convertirlo a int
        numFilas = std::stoi(argv[3]);
        numColumnas = std::stoi(argv[4]);
    }

    size = numColumnas * numFilas;

    //Reservamos memoria para el tablero, ya que no esta inicializado
    h_tablero = (int*)malloc(size * sizeof(int));

    //Llamamos a la funcion que inicializa con valores aleatorios el tablero
    h_tablero = inicializarTablero(h_tablero, size, dificultad);

    mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);

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
            mostrarTablero(h_tablero, numFilas, numColumnas, dificultad);
        }
        else
        {
            printf("\nLas coordenadas introducidas se encuentran fuera del rango del tablero [%d][%d] \n", numFilas, numColumnas);
        }


    }
    printf("\nPERDEDOR \n");
}



