# Cargar las librerías necesarias
library(readxl)   # Para leer archivos Excel
library(dplyr)    # Para manipulación de datos

# -------------------- 1. Cargar los datos --------------------

# Leer la base de datos de países con información general
country_annotation <- read.csv("C:/Users/victo/Downloads/archive/country_annotation.csv", sep=";")

# Leer la base de datos de población mundial
Countries_in_the_world_by_population_2025_ <- read_excel("Countries in the world by population (2025).xlsx")

# -------------------- 2. Preparar y unir las bases de datos --------------------

# Renombrar la columna 'Country' en la base de población para que coincida con 'name'
Countries_in_the_world_by_population_2025_ <- Countries_in_the_world_by_population_2025_ %>%
  rename(name = Country)

# Unir ambas bases de datos por el nombre del país ('name')
country_annotation <- country_annotation %>%
  left_join(Countries_in_the_world_by_population_2025_, by = "name")

# Contar y mostrar cuántos países quedaron sin datos de población
na_count <- sum(is.na(country_annotation$Population))
cat("Cantidad de países sin población tras la unión:", na_count, "\n")

# Identificar los países que no tienen datos de población
countries_with_na <- country_annotation %>%
  filter(is.na(Population)) %>%
  select(name)

# Mostrar los países con NA en Population
View(countries_with_na)

# -------------------- 3. Convertir la columna Population a numérico --------------------

# La columna Population contiene comas como separadores de miles, lo que la hace texto
# Eliminamos las comas y convertimos Population a numérico
country_annotation <- country_annotation %>%
  mutate(Population = as.numeric(gsub(",", "", Population)))

# Verificar que la conversión fue exitosa
str(country_annotation$Population)  # Debe mostrar "num"
summary(country_annotation$Population)  # Revisar estadísticas de los valores

# -------------------- 4. Imputar manualmente la población de ciertos países --------------------

# Lista de países con valores de población corregidos manualmente
corrections <- data.frame(
  name = c("Democratic Republic of the Congo", "Republic of the Congo", "Cape Verde", 
           "Czech Republic", "Faroe Islands", "Myanmar [Burma]", "Palestine", 
           "São Tomé and Príncipe", "East Timor", "Wallis and Futuna", "Ivory Coast"),
  Population = c(109276265, 6332961, 524877, 10735859, 55400, 
                 54500091, 5495443, 235536, 1400638, 11277, 31934230)
)

# Unir la base de datos con las correcciones manuales y reemplazar los valores NA
country_annotation <- country_annotation %>%
  left_join(corrections, by = "name", suffix = c("_original", "_manual")) %>%
  mutate(Population = coalesce(Population_original, Population_manual)) %>%
  select(-Population_original, -Population_manual)

# Verificar que los valores manuales fueron asignados correctamente
country_annotation %>%
  filter(name %in% corrections$name) %>%
  select(name, Population)

# Contar nuevamente los países con NA en Population
na_count_after <- sum(is.na(country_annotation$Population))
cat("Cantidad de países sin población después de la corrección manual:", na_count_after, "\n")

# Ver los países que aún tienen NA en Population
countries_with_na <- country_annotation %>%
  filter(is.na(Population)) %>%
  select(name)

# Mostrar los países con NA en Population
View(countries_with_na)

# -------------------- 5. Eliminar los países sin datos de población --------------------

# Filtrar la base de datos para eliminar los países que aún tienen NA en Population
country_annotation <- country_annotation %>%
  filter(!is.na(Population))

# Verificar que ya no quedan NAs en Population
na_count_final <- sum(is.na(country_annotation$Population))
cat("Cantidad de países sin población después de eliminar los NA:", na_count_final, "\n")

# Ver cuántos países quedaron en la base final
cat("Número final de países en la base de datos:", nrow(country_annotation), "\n")

# -------------------- 6. Guardar la base de datos final --------------------

# Guardar la base de datos final sin países con NA en Population
write.csv(country_annotation, "C:/Users/victo/Downloads/country_annotation_final.csv", row.names = FALSE)

# Mensaje final
cat("La base de datos final ha sido guardada exitosamente.\n")
# view()

# -------------------- 7. Variables Adicionales  --------------------

country_annotation_final <- read.csv("C:/Users/victo/Downloads/country_annotation_final.csv")
View(country_annotation_final)

WVS_Cross.National_Wave_7_csv_v6_0 <- read.csv("C:/Users/victo/Downloads/F00011356-WVS_Cross-National_Wave_7_csv_v6_0/WVS_Cross-National_Wave_7_csv_v6_0.csv")
View(WVS_Cross.National_Wave_7_csv_v6_0)


 # install.packages("countrycode")

# Cargar librerías
library(dplyr)
library(countrycode)

# Convertir los códigos Alpha-2 a Alpha-3
country_annotation <- country_annotation_final %>%
  mutate(B_COUNTRY_ALPHA = countrycode(code, origin = "iso2c", destination = "iso3c"))

# Verificar si hay valores NA (países que no se pudieron convertir)
sum(is.na(country_annotation$B_COUNTRY_ALPHA))

View(country_annotation)

# -------------------- 2. Filtrar las preguntas de confianza (Q58-Q63) --------------------

# Seleccionar solo las columnas de interés (B_COUNTRY_ALPHA y preguntas Q58-Q63)
trust_questions <- WVS_Cross.National_Wave_7_csv_v6_0 %>%
  select(B_COUNTRY_ALPHA, Q58, Q59, Q60, Q61, Q62, Q63)

# -------------------- 3. Calcular el Trust Index por país --------------------

# Convertir las respuestas a numérico (por si hay valores no numéricos)
trust_questions[, 2:7] <- lapply(trust_questions[, 2:7], as.numeric)

# Calcular el promedio de confianza por país
trust_index <- trust_questions %>%
  group_by(B_COUNTRY_ALPHA) %>%
  summarise(trust_index = mean(c(Q58, Q59, Q60, Q61, Q62, Q63), na.rm = TRUE))  # Promedio de todas las respuestas

# -------------------- 4. Unir el Trust Index a la base country_annotation --------------------

# Unir los datos por B_COUNTRY_ALPHA
country_annotation <- country_annotation %>%
  left_join(trust_index, by = "B_COUNTRY_ALPHA")

# Verificar la nueva base de datos
View(country_annotation)

# Filtrar solo los países que tienen un Trust Index asignado
countries_with_trust <- country_annotation %>%
  filter(!is.na(trust_index)) %>%
  select(name, B_COUNTRY_ALPHA, trust_index)

# Imprimir los países con Trust Index y sus valores
print(countries_with_trust)

# Contar cuántos países tienen valores de Trust Index
num_countries_with_trust <- nrow(countries_with_trust)
cat("Número total de países con Trust Index:", num_countries_with_trust, "\n")

# -------------------- 1. Calcular el Corruption Index --------------------

# Seleccionar solo la columna de interés (B_COUNTRY_ALPHA y Q112)
corruption_data <- WVS_Cross.National_Wave_7_csv_v6_0 %>%
  select(B_COUNTRY_ALPHA, Q112)

# Convertir la respuesta a numérico (por si hay valores no numéricos)
corruption_data$Q112 <- as.numeric(corruption_data$Q112)

# Calcular el promedio de corrupción por país
corruption_index <- corruption_data %>%
  group_by(B_COUNTRY_ALPHA) %>%
  summarise(corruption_index = mean(Q112, na.rm = TRUE))  # Promedio de respuestas por país

# -------------------- 2. Unir el Corruption Index a country_annotation --------------------

# Unir los datos por B_COUNTRY_ALPHA
country_annotation <- country_annotation %>%
  left_join(corruption_index, by = "B_COUNTRY_ALPHA")

# -------------------- 3. Verificar los países con valores de corrupción --------------------

# Filtrar solo los países que tienen un Corruption Index asignado
countries_with_corruption <- country_annotation %>%
  filter(!is.na(corruption_index)) %>%
  select(name, B_COUNTRY_ALPHA, corruption_index)

# Imprimir los países con Corruption Index y sus valores
print(countries_with_corruption)

# Contar cuántos países tienen valores de Corruption Index
num_countries_with_corruption <- nrow(countries_with_corruption)
cat("Número total de países con Corruption Index:", num_countries_with_corruption, "\n")

# Ver la nueva base de datos
View(country_annotation)

# -------------------- 1. Calcular el Competition Index --------------------

# Seleccionar solo la columna de interés (B_COUNTRY_ALPHA y Q109)
competition_data <- WVS_Cross.National_Wave_7_csv_v6_0 %>%
  select(B_COUNTRY_ALPHA, Q109)

# Convertir la respuesta a numérico (por si hay valores no numéricos)
competition_data$Q109 <- as.numeric(competition_data$Q109)

# Calcular el promedio de competencia por país
competition_index <- competition_data %>%
  group_by(B_COUNTRY_ALPHA) %>%
  summarise(competition_index = mean(Q109, na.rm = TRUE))  # Promedio de respuestas por país

# -------------------- 2. Unir el Competition Index a country_annotation --------------------

# Unir los datos por B_COUNTRY_ALPHA
country_annotation <- country_annotation %>%
  left_join(competition_index, by = "B_COUNTRY_ALPHA")

# -------------------- 3. Verificar los países con valores de Competition Index --------------------

# Filtrar solo los países que tienen un Competition Index asignado
countries_with_competition <- country_annotation %>%
  filter(!is.na(competition_index)) %>%
  select(name, B_COUNTRY_ALPHA, competition_index)

# Imprimir los países con Competition Index y sus valores
print(countries_with_competition)

# Contar cuántos países tienen valores de Competition Index
num_countries_with_competition <- nrow(countries_with_competition)
cat("Número total de países con Competition Index:", num_countries_with_competition, "\n")

# Ver la nueva base de datos
View(country_annotation)

# -------------------- 1. Calcular el Migration Index --------------------

# Seleccionar solo la columna de interés (B_COUNTRY_ALPHA y Q121)
migration_data <- WVS_Cross.National_Wave_7_csv_v6_0 %>%
  select(B_COUNTRY_ALPHA, Q121)

# Convertir la respuesta a numérico (por si hay valores no numéricos)
migration_data$Q121 <- as.numeric(migration_data$Q121)

# Calcular el promedio de migración por país
migration_index <- migration_data %>%
  group_by(B_COUNTRY_ALPHA) %>%
  summarise(migration_index = mean(Q121, na.rm = TRUE))  # Promedio de respuestas por país

# -------------------- 2. Unir el Migration Index a country_annotation --------------------

# Unir los datos por B_COUNTRY_ALPHA
country_annotation <- country_annotation %>%
  left_join(migration_index, by = "B_COUNTRY_ALPHA")

# -------------------- 3. Verificar los países con valores de Migration Index --------------------

# Filtrar solo los países que tienen un Migration Index asignado
countries_with_migration <- country_annotation %>%
  filter(!is.na(migration_index)) %>%
  select(name, B_COUNTRY_ALPHA, migration_index)

# Imprimir los países con Migration Index y sus valores
print(countries_with_migration)

# Contar cuántos países tienen valores de Migration Index
num_countries_with_migration <- nrow(countries_with_migration)
cat("Número total de países con Migration Index:", num_countries_with_migration, "\n")

# Ver la nueva base de datos
View(country_annotation)

# -------------------- 1. Calcular el Openness Index --------------------

# Seleccionar solo las columnas de interés (B_COUNTRY_ALPHA y preguntas Q19, Q21, Q22, Q23, Q26)
openness_data <- WVS_Cross.National_Wave_7_csv_v6_0 %>%
  select(B_COUNTRY_ALPHA, Q19, Q21, Q22, Q23, Q26)

# Convertir las respuestas a numérico (por si hay valores no numéricos)
openness_data[, 2:6] <- lapply(openness_data[, 2:6], as.numeric)

# Calcular el promedio de apertura por país
openness_index <- openness_data %>%
  group_by(B_COUNTRY_ALPHA) %>%
  summarise(openness_index = mean(c(Q19, Q21, Q22, Q23, Q26), na.rm = TRUE))  # Promedio de todas las respuestas

# -------------------- 2. Unir el Openness Index a country_annotation --------------------

# Unir los datos por B_COUNTRY_ALPHA
country_annotation <- country_annotation %>%
  left_join(openness_index, by = "B_COUNTRY_ALPHA")

# -------------------- 3. Verificar los países con valores de Openness Index --------------------

# Filtrar solo los países que tienen un Openness Index asignado
countries_with_openness <- country_annotation %>%
  filter(!is.na(openness_index)) %>%
  select(name, B_COUNTRY_ALPHA, openness_index)

# Imprimir los países con Openness Index y sus valores
print(countries_with_openness)

# Contar cuántos países tienen valores de Openness Index
num_countries_with_openness <- nrow(countries_with_openness)
cat("Número total de países con Openness Index:", num_countries_with_openness, "\n")

# Ver la nueva base de datos
View(country_annotation)

# -------------------- Guardar la base de datos final con todos los índices --------------------

write.csv(country_annotation, "C:/Users/victo/Downloads/country_annotation_final_with_WVSindicators.csv", row.names = FALSE)

# Confirmación
cat("Archivo guardado exitosamente en: C:/Users/victo/Downloads/country_annotation_final_with_WVSindicators.csv\n")

# Ver la base de datos final
View(country_annotation)

# -------------------- 1. Cargar los datos del HDR --------------------

library(readxl)
library(dplyr)

# Leer la base de datos HDR (Human Development Report)
hdr_data <- read_excel("C:/Users/victo/Downloads/hdr-data.xlsx")

# Seleccionar solo las columnas necesarias
hdr_data <- hdr_data %>%
  select(countryIsoCode, value) %>%
  rename(B_COUNTRY_ALPHA = countryIsoCode, HDI = value)

# -------------------- 2. Unir la información del HDI a country_annotation --------------------

# Unir los datos por B_COUNTRY_ALPHA
country_annotation <- country_annotation %>%
  left_join(hdr_data, by = "B_COUNTRY_ALPHA")

# -------------------- 3. Verificar los valores NA en la columna HDI --------------------

# Contar cuántos países tienen NA en HDI
num_na_hdi <- sum(is.na(country_annotation$HDI))
cat("Número total de países sin HDI:", num_na_hdi, "\n")

# Filtrar los países que tienen NA en HDI
countries_with_na_hdi <- country_annotation %>%
  filter(is.na(HDI)) %>%
  select(name, B_COUNTRY_ALPHA)

# Imprimir los países con NA en HDI
print(countries_with_na_hdi)

# -------------------- 4. Guardar la base final --------------------

write.csv(country_annotation, "C:/Users/victo/Downloads/country_annotation_with_HDI.csv", row.names = FALSE)

# Ver la nueva base de datos
View(country_annotation)



# -------------------- 1. Cargar los datos de penetración de internet --------------------

library(readr)
library(dplyr)

# Leer la base de datos de internet penetration
internet_penetration <- read.csv("C:/Users/victo/Downloads/internet-penetration-by-country-2024.csv")

# Seleccionar solo las columnas relevantes hasta el año 2014
internet_penetration <- internet_penetration %>%
  select(country, 
         InternetPenetration_Penetration_Pct_2023,
         InternetPenetration_Penetration_Pct_2022,
         InternetPenetration_Penetration_Pct_2021,
         InternetPenetration_Penetration_Pct_2020,
         InternetPenetration_Penetration_Pct_2019,
         InternetPenetration_Penetration_Pct_2018,
         InternetPenetration_Penetration_Pct_2017,
         InternetPenetration_Penetration_Pct_2016,
         InternetPenetration_Penetration_Pct_2015,
         InternetPenetration_Penetration_Pct_2014)

# Convertir las columnas de porcentaje de penetración a numéricas
internet_penetration[, 2:ncol(internet_penetration)] <- lapply(internet_penetration[, 2:ncol(internet_penetration)], function(x) as.numeric(as.character(x)))

# -------------------- 2. Obtener el valor más reciente de penetración de internet --------------------

# Crear una nueva columna con el valor más reciente disponible para cada país
internet_penetration <- internet_penetration %>%
  mutate(`% internet penetration` = coalesce(InternetPenetration_Penetration_Pct_2023, 
                                             InternetPenetration_Penetration_Pct_2022, 
                                             InternetPenetration_Penetration_Pct_2021, 
                                             InternetPenetration_Penetration_Pct_2020, 
                                             InternetPenetration_Penetration_Pct_2019,
                                             InternetPenetration_Penetration_Pct_2018,
                                             InternetPenetration_Penetration_Pct_2017,
                                             InternetPenetration_Penetration_Pct_2016,
                                             InternetPenetration_Penetration_Pct_2015,
                                             InternetPenetration_Penetration_Pct_2014)) %>%
  select(country, `% internet penetration`)  # Mantener solo las columnas necesarias

# -------------------- 3. Unir la información a country_annotation --------------------

# Unir los datos por el nombre del país (name)
country_annotation <- country_annotation %>%
  left_join(internet_penetration, by = c("name" = "country"))


# -------------------- 4. Verificar los valores NA en la columna % internet penetration --------------------

# Contar cuántos países tienen NA en % internet penetration
num_na_internet <- sum(is.na(country_annotation$`% internet penetration`))

cat("Número total de países sin datos de penetración de internet:", num_na_internet, "\n")

# Filtrar los países que tienen NA en % internet penetration
countries_with_na_internet <- country_annotation %>%
  filter(is.na(`% internet penetration`)) %>%
  select(name, B_COUNTRY_ALPHA)

# Imprimir los países con NA en % internet penetration
print(countries_with_na_internet)

# -------------------- 5. Guardar la base final --------------------

write.csv(country_annotation, "C:/Users/victo/Downloads/country_annotation_with_internet_penetration.csv", row.names = FALSE)

# Ver la nueva base de datos
View(country_annotation) 



# -------------------- 1. Cargar las bases de datos --------------------

library(readr)
library(dplyr)

# Leer la base de datos con internet penetration
country_annotation <- read.csv("C:/Users/victo/Downloads/country_annotation_with_internet_penetration.csv")

# Leer la base de datos del Gini Coefficient
gini_coefficient <- read.csv("C:/Users/victo/Downloads/gini-coefficient-by-country-2024.csv")

# Seleccionar solo las columnas relevantes
gini_coefficient <- gini_coefficient %>%
  select(country, giniCoefficientByCountry_giniWB) %>%
  rename(name = country, gini_index = giniCoefficientByCountry_giniWB)

# -------------------- 2. Unir la información del Gini Index a country_annotation --------------------

# Unir los datos por el nombre del país (name)
country_annotation <- country_annotation %>%
  left_join(gini_coefficient, by = "name")

# -------------------- 3. Verificar los valores NA en la columna gini_index --------------------

# Contar cuántos países tienen NA en gini_index
num_na_gini <- sum(is.na(country_annotation$gini_index))
cat("Número total de países sin Gini Index:", num_na_gini, "\n")

# Filtrar los países que tienen NA en gini_index
countries_with_na_gini <- country_annotation %>%
  filter(is.na(gini_index)) %>%
  select(name, B_COUNTRY_ALPHA)

# Imprimir los países con NA en gini_index
print(countries_with_na_gini)

# -------------------- 4. Guardar la base final --------------------

write.csv(country_annotation, "C:/Users/victo/Downloads/country_annotation_final_with_gini_index.csv", row.names = FALSE)

# Ver la nueva base de datos
View(country_annotation)



# -------------------- 1. Cargar las bases de datos --------------------

library(readr)
library(dplyr)

# Leer la base de datos con el Gini Index
gini_coefficient <- read.csv("C:/Users/victo/Downloads/gini-coefficient-by-country-2024.csv")

# Seleccionar solo las columnas relevantes
gini_coefficient <- gini_coefficient %>%
  select(country, giniCoefficientByCountry_giniWB, giniCoefficientByCountry_giniCIA) %>%
  rename(name = country, gini_index_WB = giniCoefficientByCountry_giniWB, gini_index_CIA = giniCoefficientByCountry_giniCIA)

# -------------------- 2. Unir la información del Gini Index a country_annotation --------------------

# Unir los datos por el nombre del país (name)
country_annotation <- country_annotation %>%
  left_join(gini_coefficient, by = "name")

# -------------------- 3. Rellenar los valores NA en gini_index --------------------

# Si gini_index_WB es NA, tomar el valor de gini_index_CIA
country_annotation <- country_annotation %>%
  mutate(gini_index = coalesce(gini_index_WB, gini_index_CIA))

# Eliminar las columnas auxiliares usadas en la fusión
country_annotation <- country_annotation %>%
  select(-gini_index_WB, -gini_index_CIA)

# -------------------- 4. Verificar los valores NA en la columna gini_index --------------------

# Contar cuántos países tienen NA en gini_index después de la corrección
num_na_gini <- sum(is.na(country_annotation$gini_index))
cat("Número total de países sin Gini Index después de la corrección:", num_na_gini, "\n")

# Filtrar los países que aún tienen NA en gini_index
countries_with_na_gini <- country_annotation %>%
  filter(is.na(gini_index)) %>%
  select(name, B_COUNTRY_ALPHA)

# Imprimir los países con NA en gini_index después de la corrección
print(countries_with_na_gini)

# -------------------- 5. Guardar la base final --------------------

write.csv(country_annotation, "C:/Users/victo/Downloads/country_annotation_final_with_complete_gini_index.csv", row.names = FALSE)

# Ver la nueva base de datos
View(country_annotation)




# -------------------- 1. Seleccionar solo las columnas de interés --------------------

# Filtrar solo las preguntas de confianza y el identificador del país
trust_questions <- WVS_Cross.National_Wave_7_csv_v6_0 %>%
  select(B_COUNTRY_ALPHA, Q58, Q59, Q60, Q61, Q62, Q63)

# -------------------- 2. Convertir las respuestas a valores numéricos --------------------

trust_questions[, 2:7] <- lapply(trust_questions[, 2:7], as.numeric)

# -------------------- 3. Calcular el Trust Index por país --------------------

trust_index <- trust_questions %>%
  group_by(B_COUNTRY_ALPHA) %>%
  summarise(
    trust_index_in_group = mean(c(Q58, Q59, Q60), na.rm = TRUE),  # In-group trust
    trust_index_out_group = mean(c(Q61, Q62, Q63), na.rm = TRUE)  # Out-group trust
  )

# -------------------- 4. Unir los Trust Index a la base country_annotation --------------------

# Unir los datos por B_COUNTRY_ALPHA
country_annotation <- country_annotation %>%
  left_join(trust_index, by = "B_COUNTRY_ALPHA")

# -------------------- 5. Verificar los valores asignados --------------------

# Filtrar solo los países que tienen valores de Trust Index asignados
countries_with_trust <- country_annotation %>%
  filter(!is.na(trust_index_in_group) & !is.na(trust_index_out_group)) %>%
  select(name, B_COUNTRY_ALPHA, trust_index_in_group, trust_index_out_group)

# Imprimir los países con valores asignados
print(countries_with_trust)

# Contar cuántos países tienen valores en ambas columnas
num_countries_with_trust <- nrow(countries_with_trust)
cat("Número total de países con Trust Index asignado:", num_countries_with_trust, "\n")

# -------------------- 6. Guardar la base final --------------------

write.csv(country_annotation, "C:/Users/victo/Downloads/country_annotation_with_trust_index.csv", row.names = FALSE)

# Ver la nueva base de datos
View(country_annotation)

# -------------------- Guardar la base final con Trust Index y Gini Index --------------------

write.csv(country_annotation, "C:/Users/victo/Downloads/country_annotation_final_with_trust_gini.csv", row.names = FALSE)

# Confirmación
cat("Archivo guardado exitosamente en: C:/Users/victo/Downloads/country_annotation_final_with_trust_gini.csv\n")

# Ver la base de datos final
View(country_annotation)

