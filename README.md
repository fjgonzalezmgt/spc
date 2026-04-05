# qicharts2 SPC Wrapper

![R](https://img.shields.io/badge/R-4.5+-276DC3?logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-App-75AADB?logo=rstudioide&logoColor=white)
![bslib](https://img.shields.io/badge/UI-bslib-4B5563)
![qicharts2](https://img.shields.io/badge/SPC-qicharts2-0F766E)
![Excel](https://img.shields.io/badge/Input-Excel%20%2F%20CSV-217346?logo=microsoft-excel&logoColor=white)

Aplicacion Shiny que funciona como wrapper para funciones de control estadistico de procesos de `qicharts2`, leyendo datos desde CSV o Excel y exponiendo una interfaz operativa para correr analisis, revisar resultados y exportarlos.

## Analisis soportados

- `qicharts2::qic` para run charts y cartas de control
- `qicharts2::paretochart` para diagramas de Pareto
- `qicharts2::bchart` para Bernoulli CUSUM

## Flujo

1. El usuario carga un archivo `.csv`, `.txt`, `.xls` o `.xlsx`.
2. La app inspecciona columnas y tipos disponibles.
3. El usuario elige el analisis `qicharts2` que quiere envolver.
4. La app muestra solo los parametros requeridos por esa funcion.
5. Se valida la estructura minima del dataset para ese analisis.
6. Se ejecuta la funcion correspondiente.
7. Los resultados se presentan en pestanas separadas para vista previa, resultados y grafico.
8. El usuario puede exportar un archivo Excel consolidado.

## Funcionalidades

- carga de archivos `.csv`, `.txt`, `.xls` y `.xlsx`
- deteccion automatica de columnas numericas
- panel dinamico de parametros segun la funcion `qicharts2` elegida
- soporte para `qic()`, `paretochart()` y `bchart()`
- resultados resumidos y tablas exportables por analisis
- exportacion consolidada a Excel

## Salidas

La app muestra un resumen tabular comun para todos los wrappers, tablas adicionales del objeto generado y una imagen exportable del grafico.

## Estructura

- [`global.R`](./global.R): catalogo de wrappers y funciones de ejecucion
- [`ui.R`](./ui.R): interfaz Shiny dinamica por tipo de analisis
- [`server.R`](./server.R): flujo reactivo generico
- [`gage_rr_sample.xlsx`](./gage_rr_sample.xlsx): archivo de prueba con hojas `qic_u_chart`, `pareto`, `bchart` y `README`

## Requisitos

Instala estos paquetes en R:

```r
install.packages(c(
  "shiny",
  "bslib",
  "readxl",
  "qicharts2",
  "openxlsx"
))
```

## Dependencias y rol de cada una

- `shiny`: interfaz web interactiva
- `bslib`: layout y componentes visuales
- `readxl`: lectura de archivos Excel
- `qicharts2`: motor estadistico y grafico principal
- `openxlsx`: generacion de archivos exportables

## Ejecucion

Desde la carpeta del proyecto:

```r
shiny::runApp()
```

## Uso

1. Carga un archivo CSV o Excel.
2. Revisa las columnas detectadas.
3. Selecciona el analisis `qicharts2`.
4. Ajusta columnas y parametros segun ese analisis.
5. Pulsa `Ejecutar analisis`.
6. Revisa tablas y grafico.
7. Exporta el consolidado desde la interfaz.

## Formato esperado de datos

Depende del wrapper elegido:

- `qic`: columna `y` numerica, opcionalmente `x`, `n`, facetas y notas
- `paretochart`: una columna categorica
- `bchart`: una columna binaria logica o 0/1

El archivo [`gage_rr_sample.xlsx`](./gage_rr_sample.xlsx) ya viene adaptado a esos tres flujos:

- `qic_u_chart`: serie temporal con numerador, denominador, faceta y notas
- `pareto`: categorias para priorizacion
- `bchart`: eventos binarios 0/1

## Licencia

Este proyecto se distribuye bajo la licencia Creative Commons Attribution 4.0 International (`CC BY 4.0`). Consulta [`LICENSE.md`](./LICENSE.md).

## Casos de uso

- run charts y cartas de control operativas
- analisis de atributos y tasas con denominadores
- diagramas de Pareto para priorizacion
- CUSUM Bernoulli para eventos raros

## Limitaciones y supuestos

- el motor estadistico sigue siendo `qicharts2`; la app solo envuelve sus funciones
- la app no valida todos los supuestos estadisticos de cada chart
- algunos charts de `qic()` requieren configuracion correcta de `n`, `x` o agregacion para ser interpretables
- `bchart()` requiere una variable binaria real
