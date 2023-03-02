#include "cuda_runtime.h"
#include "device_launch_parameters.h"
#include <stdio.h>
#include <cstdlib>
#include <ctime>
#include <string>
#include <curand.h>
#include <cuand_kernel.h>


cudaError_t addWithCuda(int* c, const int* a, const int* b, unsigned int size);

//Define constantes
const int AZUL = 1;
const int ROJO = 2;
const int NARANJA = 3;
const int VERDE = 4;
const int MARRON = 5;
const int AMARRILLO = 6;

//define numero de filas y columnas del tablero (CUIDADO CAMBIAR A COGER POR CONSOLA QUE FILAS SE QUIERE)
const int FILAS = 5;
const int COLUMNAS = 5;

//Funcion que muestra el tablero por consola
void mostrarTablero(int** tablero, int numFilas, int numColumnas)
{
    for (int i = 0; i < numFilas; i++)
    {
        for (int j = 0; j < numColumnas; j++)
        {
            printf("%d  ", tablero[i][j]);
        }
        printf("\n");
    }
    printf("\n");
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

//Kernel que lleva a cabo la generacion del tablero de forma aleatoria
_global_ void kernelGenerarTablero(int **tablero, int numFilas, int numColumnas, unsigned int semilla)
{
    //Definimos índices de X e Y
    int fila = blockIdx.x * blockDim.x + threadIdx.x;
    int columna = blockIdx.y * blockDim.y + threadIdx.y;

    //Usamos biblioteca curand para crear numeros aleatorios y funcion curand_init con semilla 0
    CurandState curandState;
    curand_init(semilla, fila * numFilas + columna,0,&curandState); //curand_init(semilla, secuencia, offset, estado) secuencia dgenera diferentes secuencias de numeros aleatorio a partir de la misma semilla y offset genera numeros aleatorio s a partir de una secuencia y una semilla

    //Controlamos que los índices no se salgan del tamaño de la matriz
    if (fila < numFilas && columna < numColumnas)
    {
        tablero[fila][columna] = curand_uniform(&curandState) % 6 + 1;   //Rellena tablero con numeros aleatorios entre 1 y 6
    }
}



void main(int argc, char* argv[])
{
    //Inicializo variables
    int** h_tablero;
    int** dev_tablero;

    /*
    //Controla que no de error la llamada
    if (argc == -1)
    {
        printf("ERROR en ejecucion");
    }
    //Controla que tengamos los argumentso encesarios (tipo de ejecucion, dificultad, filas, columnas)
    if (argc < 5)
    {
        printf("ERROR: faltan argumentos de entrada");
    }
    //Controlan que no se pasen mas argumentos de los deseados
    if(argc > 5)
    {
        printf("ERROR: sobran argumentos de entrada");
    }
    else //Realiza llamada con los 4 argumemtos
    {
        //Guarda los argumentos pasadas en las respectivas variables
        std::string tipoEjecucion = argv[1];
        int dificultad = std::stoi(argv[2]);    //Guarda valor argumentos usando funcion stoi para convertirlo a int
        int numFilas = std::stoi(argv[3]);
        int numColumnas = std::stoi(argv[4]);

        int sizeFilas = numFilas * sizeof(int*); //Variable guarda tamaño matriz segun el tamaño de filas
        int sizeColumnas = numColumnas * sizeof(int*); //Variable guarda tamaño matriz segun el tamaño de filas

        //Reservamos memoria para la matriz que representa el tablero en el host
        dev_tablero = (int**)malloc(sizeFilas);
        //Bucle que recorre matriz y reservar memoria
        for (int i = 0; i < numFilas; i++)
        {
            cudaMalloc(&(dev_tablero[i]), sizeColumnas);
        }

        //Reservamos memoria para el tablero en device
        cudaMalloc(&dev_tablero, sizeFilas);


        //Pasamos tablero del host a device
        cudaMemcpy(dev_tablero, h_tablero, sizeFilas, cudaMemcpyHostToDevice);

        //Definimos el numero de bloques e hilos/bloque
        dim3 blockInGrid(1);    //numero de bloques
        dim3 threadsInBlock(numFilas, numColumnas, 1);  //hilos/bloque

        //Llamamos al Kernel
        kernelGenerarTablero<<<blockInGrid, threadsInBlock>>>(dev_tablero, numFilas, numColumnas);

        //Pasa tablero generado por el kernel del device a host
        cudaMemcpy(h_tablero, dev_tablero, sizeFilas, cudaMemcpyDeviceToHost);

        //Muestra tablero generado por pantalla
        mostrarTablero(h_tablero, numFilas, numColumnas);

    }*/

    //Guarda los argumentos pasadas en las respectivas variables
    std::string tipoEjecucion = "a";
    int dificultad = 1;    //Guarda valor argumentos usando funcion stoi para convertirlo a int
    int numFilas = 5;
    int numColumnas = 5;
    unsigned int semilla = time(NULL);	//Semilla usada para generar valores aleatorio del tablero depende del tiempo para evitar tener la misma inicializacion

    int sizeFilas = numFilas * sizeof(int*); //Variable guarda tamaño matriz segun el tamaño de filas
    int sizeColumnas = numColumnas * sizeof(int*); //Variable guarda tamaño matriz segun el tamaño de filas

    //Reservamos memoria para la matriz que representa el tablero en el host
    h_tablero = (int**)malloc(sizeFilas);
    //Bucle que recorre matriz y reservar memoria 
    for (int i = 0; i < numFilas; i++)
    {
        cudaMalloc(&(h_tablero[i]), sizeColumnas);
    }

    //Reservamos memoria para el tablero en device
    cudaMalloc(&dev_tablero, sizeFilas);


    //Pasamos tablero del host a device
    cudaMemcpy(dev_tablero, h_tablero, sizeFilas, cudaMemcpyHostToDevice);

    //Definimos el numero de bloques e hilos/bloque
    dim3 blockInGrid(1);    //numero de bloques
    dim3 threadsInBlock(numFilas, numColumnas, 1);  //hilos/bloque

    //Llamamos al Kernel
    kernelGenerarTablero<<<blockInGrid, threadsInBlock>>>(dev_tablero, numFilas, numColumnas, semilla);

    //generarTablero(numFilas, numColumnas);
    //Pasa tablero generado por el kernel del device a host
    cudaMemcpy(h_tablero, dev_tablero, sizeFilas, cudaMemcpyDeviceToHost);

    //Muestra tablero generado por pantalla
    mostrarTablero(h_tablero, numFilas, numColumnas);

}