#include "cuda_runtime.h"
#include "device_launch_parameters.h"
#include <stdio.h>
#include <cstdlib>
#include <ctime>

cudaError_t addWithCuda(int *c, const int *a, const int *b, unsigned int size);

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
void mostrarTablero(int tablero[FILAS][COLUMNAS])
{
    for (int i = 0; i < FILAS; i++)
    {
        for (int j = 0; j < COLUMNAS; j++)
        {
            printf("%d  ", tablero[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

//Crea un nuevo tablero aleatorio del tamaño indicado
int generarTablero()
{
    //Matriz que representa el tablero
    int tablero[FILAS][COLUMNAS];
    // Para que no se obtengan los mismos números aleatorios
    srand((unsigned int)time(NULL));

    // Se inicializa las matrices (matrixA y matrixB), a partir de la dimension dada, rellenada con enteros aleatorios de 0-9
    for (int i = 0; i < FILAS; i++) 
    {
        for (int j = 0; j < COLUMNAS; j++) 
        {
            tablero[i][j] = (rand() % 6) + 1;   //Rellena tablero con numeros aleatorios entre 1 y 6
        }
    }
    mostrarTablero(tablero); //Llama a funcion para mostrar tablero por pantalla
    return 0;
    //return tablero; //devuelve el tablero creado
}


void main()
{
    generarTablero();
}