
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

//Genera una semilla aleatoria para cada hilo
int generarSemilla()
{
    curandState_t state;
    int semilla = time(NULL) * 3663828372 + 12345 + rand();     //Semilla para generar numeros aleatorios en el kernel
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

//Kernel que elimina la fila y columna de la posicion pasada (Bomba)
__global__ void kernelBomba(int* dev_tablero, int numFila, int numCol, int pos_encontrar)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;        //Posicion en el vector 1D del hilo que llega

    //Calcula fila y columna de la posición actual
    int filaActual = pos / numCol;                          //Calculamos la fila del tablero en la que se encuentra el hilo
    int colActual = pos - filaActual * numCol;              //Calculamos la columna del tablero en la que se encuentra el hilo

    //Calcula fila y columna a borrar
    int filaBorrar = pos_encontrar / numCol;                //Calculamos la fila en la que se encuentra la posicion a encontrar
    int colBorrar = pos_encontrar - filaBorrar * numCol;    //Calculamos la columna en la que se encuentra la posicion a encontrar

    //Comprobamos i el hilo esta en la fila o columna que queremos borrar
    if (filaBorrar == filaActual || colBorrar == colActual && 0 < filaActual && filaActual < numFila && 0 < colActual && colActual < numCol)
    {
        dev_tablero[pos] = -1;                              //Indicamos que se borra
    }
    dev_tablero[pos_encontrar] = -1;                        //Eliminamos bloque especial
}


//Kernel que elimina los elementos adyacentes a una posición (radio 4 elementos) (TNT)
__global__ void kernelTNT(int* dev_tablero, int numFila, int numCol, int pos_encontrar)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;        //Posicion en la que nos encontramos

    //Calcula fila y columna de la posición actual
    int filaActual = pos / numCol;
    int colActual = pos - filaActual * numCol;

    //Calcula fila y columna a borrar teniendo en cuenta el rango y la posicion de las coordenadas
    int filaBorrar = pos_encontrar / numCol;
    int colBorrar = pos_encontrar - filaBorrar * numCol;
    int filaBorrarDer = filaBorrar + 4;
    int colBorrarAbajo = colBorrar + 4;
    int filaBorrarIzq = filaBorrar - 4;
    int colBorrarArriba = colBorrar - 4;

    //Si posición actual es adyacente y esta dentro del rango que queremos borrar (4)
    if (filaBorrarIzq <= filaActual <= filaBorrarDer && colBorrarArriba <= colActual <= colBorrarAbajo && 0 <= filaActual <= numFila && 0 <= colActual <= numCol)
    {
        dev_tablero[pos] = -1; //Indicamos que se borra
    }
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
    dev_tablero[pos_encontrar] = -1;              //Eliminamos bloque especial
}


//Kernel que lleva a cabo la generacion del tablero de forma aleatoria
__global__ void kernelGenerarTablero(int* dev_tablero, int dev_semilla, int dificultad)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;            //Posicion del hilo en el vector 1D
    curandState_t state;
    curand_init(dev_semilla, pos, 0, &state);                   //Genera diferentes secuencias de numeros aleatorio a partir de la semilla 
    int color = abs((int)(curand(&state) % dificultad) + 1);
    dev_tablero[pos] = color;                                   //Rellena tablero con numeros aleatorios entre 1 y la dificultad
}

__global__ void kernelReemplazarPosiciones(int* dev_tablero, int numFila, int numCol, int dev_semilla, int dificultad, int* dev_index)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;        //Posicion en la que nos encontramos
    dev_index[0] = 0;                                       //Lo utilizamos para contabilizar el numero de llamadas que hay que realizar al kernel 

    if (dev_tablero[pos] == -1)                             //Si la posicion tiene un -1 la reemplazamos en caso de que su posicion de arriba tenga un valor distinto de -1
    {
        //Calculamos su fila y columna para comprobar que no nos salimos del rango
        int filaActual = pos / numCol;
        int colActual = pos - filaActual * numCol;

        if (filaActual > 0 && filaActual <= numFila && dev_tablero[pos - numCol] != -1)     //Si la posicion de arriba es distinta de -1 se la asignamos a la posicion que nos llega y se la quitamos a la de arriba
        {
            dev_tablero[pos] = dev_tablero[pos - numCol];               //Le asignamos el valor a la posicion del hilo
            dev_tablero[pos - numCol] = -1;                             //Establecemos el valor de la posicion de arriba en -1
            atomicAdd(&dev_index[0], 1);                                //Incrementamos el valor del contador, el cual sera utilizado para parar el bucle while que llama a este kernel, ya que si vale 1 significa que no quedan posiciones con valor -1
        }
        else if (dev_tablero[pos - numCol] != -1)                       
        {
            curandState_t state;
            curand_init(dev_semilla, pos, 0, &state);                   //Genera diferentes secuencias de numeros aleatorio a partir de la semilla
            int color = abs((int)(curand(&state) % dificultad) + 1);    //Rellena tablero con numeros aleatorios entre 1 y dificultad
            dev_tablero[pos] = color;                                   //Asignamos un nuevo color a la posicion del tablero
            atomicAdd(&dev_index[0], 1);
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

    if ((dev_tablero[pos] == color || dev_tablero[pos] == -1) && pos_encontrar == pos)      //Comprobamos que la posicion que nos llega se corresponda con la posicion a encontrar y que su valor sea el color o -1 (lo que significa que ya ha formado parte de un camino pero puede haber mas)
    {
        encontrado = false;
        posAux = pos;

        while ((posAux < numCol * numFila) && !encontrado && !camino_invalido)              //Finaliza cuando no encuentra un camino posible
        {
            //Variables que utilizamos para ver que no nos salimos del rango de la matriz segun vamos incrementando el valor de posAux
            int sigfila = (posAux + 1) / numCol;                                    //Fila en la que se encuentra el siguiente elemento
            int sigcol = (posAux + 1) - sigfila * numCol;                           //Columna en la que se encuentra el siguiente elemento

            int fila_anterior = (posAux - 1) / numCol;                              //Fila en la que se encuentra el elemento ANTERIOR
            int col_anterior = (posAux - 1) - fila_anterior * numCol;               //Columna en la que se encuentra el elemento anterior

            int posSigFila = (posAux + numCol) / numCol;
            int fila_actual = posAux / numCol;
            int col_actual = posAux - fila_actual * numCol;

            if (color == dev_tablero[posAux + 1] && sigcol > 0 && (posAux + 1) != ultima_posicion)                                           //Comprueba el valor de su posicion DERECHA
            {
                index += 1;                 //Incrementa el indice pasa saber que hay un camino
                ultima_posicion = posAux;   //Almacenamos la ultima posicion
                posAux += 1;                //Avanzamos a la derecha
                dev_tablero[posAux] = -1;   //Marcamos la posicion del tablero a -1
            }
            else if (color == dev_tablero[posAux + numCol] && (posAux + numCol) != ultima_posicion && (posAux + numCol) < numCol * numFila)   //Comprueba el color de ABAJO
            {
                index += 1;
                ultima_posicion = posAux;
                posAux = posAux + numCol;       //Avanzamos abajo
                dev_tablero[posAux] = -1;
            }
            else if (color == dev_tablero[posAux - 1] && col_anterior > 0 && (posAux - 1) != ultima_posicion)                                  //Comprueba el color de posicion a la IZQUIERDA
            {
                index += 1;
                ultima_posicion = posAux;
                posAux = posAux - 1;            //Avanzamos a la izquierda
                dev_tablero[posAux] = -1;
            }
            else if (color == dev_tablero[posAux - numCol] && (posAux - numCol) != ultima_posicion && (posAux - numCol) >= 0 && filaActual > 0 && filaActual < numFila)  //Comprueba el color de su posicion ARRIBA
            {
                index += 1;
                ultima_posicion = posAux;
                posAux = posAux - numCol;       //Avanzamos arriba
                dev_tablero[posAux] = -1;
            }
            else
            {
                if (index > 0) {                     //Si el indice es mayor de 0 significa que en una iteracion anterior del bucle while ha encontrado un camino
                    atomicAdd(&dev_index[0], 1);     //Incrementamos una variable que sera devuelta al host para saber si se ha encontrado un camino
                    encontrado = true;               //Volvera al Host, y si es true seguira en el bucle while para ver si hay mas caminos que no ha recorrido
                }
                else
                {
                    encontrado = false;            
                }

                camino_invalido = true;              //Para el bucle del device, es necesario ya que que encontrado = true cuando el hilo a encontrado algun camino
            }

        }

    }
    dev_encontrado[0] = encontrado;                 //Almacena el valor de booleano encontrado y lo devuelve al host

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

    if (filaActual == filaEncontrar && (int)dev_index_fila < 5 && dev_tablero[pos] == -1)       //Si el hilo que llega se encuentra en la misma FILA que la de las coordendas introducidas por el usuario 
    {
        atomicAdd(&dev_index_fila[0], 1);                                                       //Incrementamos el contador de las filas
    }

    if (colActual == colEncontrar && (int)dev_index_col < 5 && dev_tablero[pos] == -1)          //Si el hilo que llega se encuentra en la misma COLUMNA que la de las coordendas introducidas por el usuario 
    {
        atomicAdd(&dev_index_col[0], 1);                                                        //Incrementamos el contador de las columnas
    }

    __syncthreads();                                                                            //Esperamos a que todos los hilos del bloque hayan incrementado los indices para saber el numero de apariciones y borrarlos

    if (dev_index_fila[0] != dev_index_col[0])      //Comprobamos que sean distintos, ya que si son iguales no cumpliria con nuestro requisito, solo puede haber 5 seguidos en una fila o en una columna, pero no simultaneamente
    {
        if ((dev_index_fila[0] == 5 && dev_index_col[0] == 1) || (dev_index_col[0] == 5 && dev_index_fila[0] == 1))     //Comprobamos que uno de los indices a encontrar sea 5
        {
            dev_tablero[pos_encontrar] = 'B';                                                                           //Generamos la bomba en el tablero
        }
    }
    __syncthreads();        //Esperamos a que todos los hilos del bloque hayan comprobado los indices 
    dev_index_fila[0] = 0;
    dev_index_col[0] = 0;
}

/* Kernel que genera un Rompecabezas RC y un TNT */
__global__ void kernelEncontrarRompecabezasTNT(int* dev_tablero, int numFila, int numCol, int pos_encontrar, int* dev_index, int dev_semilla, int dificultad)
{
    int pos = blockIdx.x * blockDim.x + threadIdx.x;

    //Calcula fila y columna de la posición actual
    int filaActual = pos / numCol;
    int colActual = pos - filaActual * numCol;

    //Calcula fila y columna de la posición a encontrar
    int filaEncontrar = pos_encontrar / numCol;
    int colEncontrar = pos_encontrar - filaEncontrar * numCol;

    if (dev_tablero[pos] == -1)                                     //Si la posicion que nos llega es un -1
    {
        atomicAdd(&dev_index[0], 1);                                //Incrementamos el contador mediante una variable atomica
    }
    __syncthreads();                                                //Esperamos a que todos los hilos del bloque hayan incrementado los indices para saber el numero de apariciones 

    if (dev_index[0] == 6 && pos == pos_encontrar)                  //Si el indice vale 6 es el TNT
    {
        dev_tablero[pos_encontrar] = 'T';
    }
    else if (dev_index[0] >= 7 && pos == pos_encontrar)             //Si el indice es mayor de o igual de 7 introducimos un RC
    {
        curandState_t state;
        curand_init(dev_semilla, pos, 0, &state);                   //Genera diferentes secuencias de numeros aleatorio a partir de la semilla
        int color = abs((int)(curand(&state) % dificultad) + 1);    //Rellena tablero con numeros aleatorios entre 1 y la dificultad                                 
        dev_tablero[pos_encontrar] = 7 + color;                      //Introducimos el RC en el tablero mas el color
    }
    __syncthreads();
    dev_index[0] = 0;

}


//Funcion que se encarga de inicializar el tablero
int* inicializarTablero(int* h_tablero, int size, int dificultad)
{
    int* (dev_Tablero);
    unsigned int semilla;

    //Reservar espacio en memoria para GPU (2 matrices y matriz resultado)
    cudaMalloc((void**)&dev_Tablero, size * sizeof(int));

    //Copiamos datos a la GPU 
    cudaMemcpy(dev_Tablero, h_tablero, size * sizeof(int), cudaMemcpyHostToDevice);

    dim3 threadsInBlock(size);

    int dev_semilla = generarSemilla();

    kernelGenerarTablero << <1, threadsInBlock >> > (dev_Tablero, dev_semilla, dificultad);

    // Copiamos de la GPU a la CPU
    cudaMemcpy(h_tablero, dev_Tablero, size * sizeof(int), cudaMemcpyDeviceToHost);

    return h_tablero;

}

//Funcion que llama a kernel para encontrar todos los caminos hacia bloque indicado e ir realizando todas las acciones correspondientes hasta perder una partida
int jugar(int* h_tablero_original, int numFilas, int numColumnas, int coordX, int coordY, int dificultad, int vida)
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
    int semilla = generarSemilla();

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

        if ((int)h_index == 0 && vida >= 1)
        {
            vida = vida - 1;
        }
        h_index_fila = { 0 };
        h_index_col = { 0 };
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
        semilla = generarSemilla();
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
    int coordenadaX = 0;
    int coordenadaY = 0;
    int size = 0;
    int dificultad = 4;
    bool terminado = false;
    char modoJuego = 'A';

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
        else if (modoJuego == 'A' || modoJuego == 'a' && numFilas > 0 && numColumnas > 0)
        {
            coordenadaX = (rand() % numFilas);
            coordenadaY = (rand() % numColumnas);
        }
        if ((coordenadaX < numFilas) && (coordenadaY < numColumnas) && (coordenadaX >= 0) && (coordenadaY >= 0))
        {
            vida = jugar(h_tablero, numFilas, numColumnas, coordenadaX, coordenadaY, dificultad, vida);
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



