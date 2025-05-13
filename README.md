
# Análisis del Parroquialismo Lingüístico

Este repositorio contiene el código, bases de datos limpias y documentación del proyecto sobre parroquialismo lingüístico en distintos idiomas.  
El análisis se basa en la posición relativa de ciudades y países en modelos de word embeddings entrenados con fastText.

## 📁 Estructura del repositorio

- `data/`: contiene las bases de datos limpias utilizadas para el análisis.
- `notebooks/`: notebooks con los pasos de procesamiento, limpieza y análisis.
- `README.md`: este archivo de presentación y guía.

## ▶️ ¿Cómo reproducir este análisis?

1. Descarga o clona el repositorio:
git clone https://github.com/VickyChavarro/proyecto-parroquialismo.git
2. Abre el notebook principal ubicado en `notebooks/` y ejecuta las celdas en orden.
3. Asegúrate de tener instaladas las bibliotecas necesarias en R o Python, según el entorno que estés usando.

## 📝 Bases de datos

- `ciudades_con_posiciones_final (1).xls`: base de ciudades con posiciones por idioma.
- `country_annotation_final_with_trust_gini.csv`: base de países con variables sociales.
- `enriched-world-city-listing-final.csv`: listado completo de ciudades con metadatos.

## 📘 Notebooks disponibles
A continuación se describen los notebooks incluidos en la carpeta `notebooks/`:

- `TraducciónCiudades.ipynb`: Traducción y normalización de nombres de ciudades a través de distintos idiomas, incluyendo variantes de escritura.
- `Unión posiciones.ipynb`: Integración de las posiciones obtenidas para cada ciudad en diferentes idiomas, consolidando la base principal.
- `Posiciones multiwords.ipynb`: Manejo de ciudades compuestas por varias palabras (como "New York") en los embeddings multilingües.
- `Word vectors.ipynb`: Carga y análisis de los archivos de fastText con vectores preentrenados para cada idioma.

## 📘 Scripts en R disponibles
Además, se incluyen scripts en R utilizados para preparación de datos y visualización:

- `R Data Ciudades.R`: Limpieza y organización inicial de la base de datos de ciudades.
- `R Data Países sin variab ad.R`: Transformación de datos de países antes de añadir variables explicativas externas.
- `Indices parroquialismo.R`: Cálculo de índices de parroquialismo como promedio de posiciones internas vs externas, RMSE, y otros.
- `visualizaciones parroquialismo.R`: Generación de gráficos y mapas para ilustrar el comportamiento del parroquialismo por idioma.

## 👩‍💻 Autora

Victoria Chavarro. Tutor: Jorge Alvarado Valencia  
Trabajo de grado – Ciencia de Datos  
Pontificia Universidad Javeriana


## 📜 Licencia

Este proyecto puede utilizarse libremente con fines académicos siempre que se cite adecuadamente.
