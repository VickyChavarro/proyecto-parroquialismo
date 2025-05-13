# Cargar las bibliotecas necesarias
library(dplyr)
library(readxl)
library(readr)

# Leer la tabla de las ciudades más pobladas
world.city.listing.table <- read.csv("C:/Users/victo/Downloads/world-city-listing-table.csv", sep=";")
# Leer la tabla de anotaciones de países
country_annotation <- read_delim("C:/Users/victo/Downloads/archive/country_annotation.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Renombrar las columnas para un mejor entendimiento en las uniones
country_annotation <- country_annotation %>%
  rename(country = name)

# Unir las tablas por el nombre del país
final_table <- left_join(world.city.listing.table, country_annotation %>% select(country, continent), by = "country")

# Mostrar la nueva tabla en RStudio para verificar
View(final_table)

# Verificar la cantidad de valores nulos en la columna de continente
num_nulos <- sum(is.na(final_table$continent))
print(paste("Número de valores nulos en la columna de continente:", num_nulos))

# Ver las filas donde el continente es nulo para saber qué ciudades tienen ese problema
ciudades_con_nulos <- final_table %>% 
  filter(is.na(continent)) %>% 
  select(city, country)

# Mostrar las ciudades con valores nulos
if(nrow(ciudades_con_nulos) > 0) {
  print("Ciudades con valores nulos en la columna de continente:")
  print(ciudades_con_nulos)
} else {
  print("No hay valores nulos en la columna de continente.")
}

# Actualizar manualmente los continentes para los países específicos con valores nulos
final_table <- final_table %>% 
  mutate(continent = case_when(
    country == "DR Congo" & is.na(continent) ~ "Africa",
    country == "Myanmar" & is.na(continent) ~ "Asia",
    country == "Haiti" & is.na(continent) ~ "North America",
    country == "Puerto Rico" & is.na(continent) ~ "North America",
    country == "Cuba" & is.na(continent) ~ "North America",
    TRUE ~ continent
  ))

# Guardar el archivo actualizado con los continentes imputados
write.csv(final_table, "C:/Users/victo/Downloads/enriched-world-city-listing-updated.csv", row.names = FALSE)

# Leer el archivo actualizado y contar nuevamente los valores nulos
final_table_updated <- read.csv("C:/Users/victo/Downloads/enriched-world-city-listing-updated.csv")
num_nulos_post_imputacion <- sum(is.na(final_table_updated$continent))
print(paste("Número de valores nulos en la columna de continente después de la imputación:", num_nulos_post_imputacion))

# Mostrar el DataFrame actualizado
View(final_table_updated)



################################### JUPYTER PARA TRANSLATION Y BACK TRANSLATION ##########################################################}



library(readxl)
df <- read_excel("Back Transalation.xlsx")
View(df)
colnames(df)

# Función para calcular la precisión
calculate_accuracy <- function(original, translated) {
  correct <- sum(tolower(original) == tolower(translated), na.rm = TRUE) # Ignora NA y compara en minúsculas
  total <- length(na.omit(translated)) # Cuenta solo los no-NA
  accuracy <- (correct / total) * 100
  return(accuracy)
}

# Aplicar la función para calcular la precisión de cada idioma
accuracy_spanish <- calculate_accuracy(df[["city"]], df[["city_from_Spanish_to_English"]])
accuracy_chinese <- calculate_accuracy(df[["city"]], df[["city_from_Chinese_to_English"]])
accuracy_arabic <- calculate_accuracy(df[["city"]], df[["city_from_Arabic_to_English"]])
accuracy_indonesian <- calculate_accuracy(df[["city"]], df[["city_from_Indonesian_to_English"]])
accuracy_swahili <- calculate_accuracy(df[["city"]], df[["city_from_Swahili_to_English"]])
accuracy_tamil <- calculate_accuracy(df[["city"]], df[["city_from_Tamil_to_English"]])
accuracy_turkish <- calculate_accuracy(df[["city"]], df[["city_from_Turkish_to_English"]])
accuracy_finnish <- calculate_accuracy(df[["city"]], df[["city_from_Finnish_to_English"]])
accuracy_japanese <- calculate_accuracy(df[["city"]], df[["city_from_Japanese_to_English"]])
accuracy_korean <- calculate_accuracy(df[["city"]], df[["city_from_Korean_to_English"]])
accuracy_vietnamese <- calculate_accuracy(df[["city"]], df[["city_from_Vietnamese_to_English"]])
accuracy_thai <- calculate_accuracy(df[["city"]], df[["city_from_Thai_to_English"]])

# Imprimir los resultados para cada idioma
cat("Accuracy for Spanish:", accuracy_spanish, "%\n")
cat("Accuracy for Chinese:", accuracy_chinese, "%\n")
cat("Accuracy for Arabic:", accuracy_arabic, "%\n")
cat("Accuracy for Indonesian:", accuracy_indonesian, "%\n")
cat("Accuracy for Swahili:", accuracy_swahili, "%\n")
cat("Accuracy for Tamil:", accuracy_tamil, "%\n")
cat("Accuracy for Turkish:", accuracy_turkish, "%\n")
cat("Accuracy for Finnish:", accuracy_finnish, "%\n")
cat("Accuracy for Japanese:", accuracy_japanese, "%\n")
cat("Accuracy for Korean:", accuracy_korean, "%\n")
cat("Accuracy for Vietnamese:", accuracy_vietnamese, "%\n")
cat("Accuracy for Thai:", accuracy_thai, "%\n")



enriched.world.city.listing.backtranslated <- read.csv("C:/Users/victo/Downloads/enriched-world-city-listing-backtranslated.csv")
View(enriched.world.city.listing.backtranslated)


# Función para calcular la precisión
calculate_accuracy <- function(original, translated) {
  correct <- sum(tolower(original) == tolower(translated), na.rm = TRUE) # Comparación en minúsculas
  total <- length(na.omit(translated)) # Solo los valores no NA
  accuracy <- (correct / total) * 100
  return(accuracy)
}

# Calcular la precisión para ruso, portugués y francés
accuracy_russian <- calculate_accuracy(
  enriched.world.city.listing.backtranslated[["city"]],
  enriched.world.city.listing.backtranslated[["city_from_Russian_to_English"]]
)

accuracy_portuguese <- calculate_accuracy(
  enriched.world.city.listing.backtranslated[["city"]],
  enriched.world.city.listing.backtranslated[["city_from_Portuguese_to_English"]]
)

accuracy_french <- calculate_accuracy(
  enriched.world.city.listing.backtranslated[["city"]],
  enriched.world.city.listing.backtranslated[["city_from_French_to_English"]]
)

# Imprimir resultados
cat("Accuracy for Russian:", accuracy_russian, "%\n")
cat("Accuracy for Portuguese:", accuracy_portuguese, "%\n")
cat("Accuracy for French:", accuracy_french, "%\n")

df <- enriched.world.city.listing.backtranslated

print_mismatches <- function(df, original_col, translated_col, language) {
  # Convertir a minúsculas
  original_lower <- tolower(df[[original_col]])
  translated_lower <- tolower(df[[translated_col]])
  
  # Identificar desajustes (evitando NA)
  mismatch_indices <- which(original_lower != translated_lower & !is.na(translated_lower))
  mismatches <- df[mismatch_indices, c(original_col, translated_col)]
  
  cat("Desajustes para", language, ":\n")
  
  # Mostrar todos los desajustes sin usar n = Inf
  print(mismatches)
}

print_mismatches(df, "city", "city_from_Russian_to_English", "Russian")



# Función revisada para imprimir todos los desajustes en las traducciones
print_mismatches <- function(df, original_col, translated_col, language) {
  # Convertir todo a minúsculas para la comparación
  original_lower <- tolower(df[[original_col]])
  translated_lower <- tolower(df[[translated_col]])
  
  # Identificar desajustes
  mismatch_indices <- which(original_lower != translated_lower)
  mismatches <- df[mismatch_indices, c(original_col, translated_col)]
  
  cat("Desajustes para", language, ":\n")
  print(mismatches, n = Inf)  # Imprimir todos los desajustes sin truncar
}


# Ejecutar la función para el indonesio
print_mismatches(df, "city", "city_from_Indonesian_to_English", "Indonesian")



# Función para calcular la tasa de error real
calculate_real_error_rate <- function(total_cities, real_errors) {
  error_rate <- (real_errors / total_cities) * 100
  return(error_rate)
}

# Contar el total de ciudades traducidas
total_cities <- nrow(df)  # Asumiendo que todas las ciudades en el dataset fueron traducidas

# Número de errores reales identificados manualmente
real_errors <- 100

# Calcular la tasa de error real
error_rate_indonesian <- calculate_real_error_rate(total_cities, real_errors)
cat("Real Error Rate for Indonesian:", error_rate_indonesian, "%\n")

################################################# REPRESENTATIVIDAD ########################################################################

# Asegurarse de que la columna de población es numérica
final_table_updated$population <- as.numeric(gsub(",", "", final_table_updated$population))

# Calcular la suma total de la población
total_population <- sum(final_table_updated$population, na.rm = TRUE)

# Imprimir el resultado
print(paste("La población total de todas las ciudades es:", total_population))

enriched.world.city.listing.updated <- read.csv("C:/Users/victo/Downloads/enriched-world-city-listing-updated.csv")
View(enriched.world.city.listing.updated)

country_annotation_final <- read.csv("C:/Users/victo/Downloads/country_annotation_final.csv")
View(country_annotation_final)


# Contar cuántas ciudades por país están representadas
city_count_by_country <- table(enriched.world.city.listing.updated$country)

# Ver los resultados
print(city_count_by_country)

# Contar el número de países únicos representados
number_of_countries <- length(unique(enriched.world.city.listing.updated$country))

# Imprimir el número de países
cat("Número total de países representados:", number_of_countries, "\n")

# Contar cuántas ciudades por continente están representadas
city_count_by_continent <- table(enriched.world.city.listing.updated$continent)

# Ver los resultados
print(city_count_by_continent)

library(dplyr)

# Obtener una muestra aleatoria de 10 ciudades
sample_cities <- sample_n(enriched.world.city.listing.updated, 10)

# Ver la muestra
View(sample_cities)


# Unir las dos bases de datos usando 'country' en la primera base de datos y 'name' en la segunda
combined_data <- merge(enriched.world.city.listing.updated, country_annotation_final, by.x = "country", by.y = "name", all.x = TRUE)

# Ver los primeros registros de la base de datos combinada para verificar la unión
View(combined_data)


# Asegurarse de que la columna de población es numérica
combined_data$population <- as.numeric(as.character(combined_data$population))
combined_data$Population <- as.numeric(as.character(combined_data$Population))  # Asegurarse de que la población total del país también es numérica

# Seleccionar al azar 5 países para revisar
set.seed(123)  # Para reproducibilidad
sample_countries <- sample(unique(combined_data$country), 5)

# Filtrar datos para esos países y calcular la suma de las poblaciones de ciudades
library(dplyr)
population_check <- combined_data %>%
  filter(country %in% sample_countries) %>%
  group_by(country) %>%
  summarise(
    total_city_population = sum(population, na.rm = TRUE),
    total_country_population = first(Population)  # Usar la columna correcta de la población total del país
  )

# Añadir una columna para verificar si la suma de las ciudades es menor o igual a la población total del país
population_check <- population_check %>%
  mutate(check = ifelse(total_city_population <= total_country_population, "OK", "Error"))

# Mostrar los resultados
print(population_check)


library(tidyverse)
library(stringr)

# Asumiendo que tu DataFrame se llama combined_data y la columna se llama languages
combined_data$languages <- str_extract_all(combined_data$languages, "\\w+")

# Expandir la lista de idiomas en filas separadas
languages_expanded <- combined_data %>%
  unnest(languages)

# Contar la cantidad de idiomas únicos en todo el DataFrame
unique_languages_count <- languages_expanded %>%
  distinct(languages) %>%
  count()

# Ver la cantidad de idiomas únicos
print(unique_languages_count)

# Opcional: Ver qué idiomas son y cuántas ciudades tienen cada idioma
language_distribution <- languages_expanded %>%
  group_by(languages) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

# Mostrar la distribución de idiomas
print(language_distribution)

# Suponiendo que language_distribution es tu tibble actual de idiomas

# Lista de idiomas que quieres verificar
idiomas_a_verificar <- c("English", "Spanish", "Chinese", "Arabic", "Indonesian", 
                         "Swahili", "Tamil", "Turkish", "Finnish", "Japanese", 
                         "Korean", "Vietnamese", "Thai")

# Verificar la presencia de cada idioma y asignar OK o NO
verificacion_idiomas <- sapply(idiomas_a_verificar, function(idioma) {
  if(idioma %in% language_distribution$languages) {
    "OK"
  } else {
    "NO"
  }
})

# Crear un data frame para mostrar los resultados
resultado_verificacion <- data.frame(Idioma = idiomas_a_verificar, 
                                     Presencia = verificacion_idiomas)

# Imprimir el resultado
print(resultado_verificacion)


####################### VERIFICACIÓN BD####################

# Cargar las bibliotecas necesarias
library(dplyr)
library(readr)

# Leer el archivo de datos
enriched_world_city_listing_complete <- read_csv("C:/Users/victo/Downloads/enriched-world-city-listing-complete.csv")

# Calcular el número de valores nulos por columna
nulos_por_columna <- sapply(enriched_world_city_listing_complete, function(x) sum(is.na(x)))

# Imprimir el número de nulos por columna
print(nulos_por_columna)


library(dplyr)
library(ggplot2)

# Asumiendo que tu DataFrame se llama enriched_world_city_listing_complete
data_grouped <- enriched_world_city_listing_complete %>%
  group_by(country, continent) %>%
  summarise(number_of_cities = n(), .groups = 'drop') %>%
  arrange(desc(number_of_cities))

# Mostrar los resultados agrupados
print(data_grouped)

# Visualizar la distribución de ciudades por continente
ggplot(data_grouped, aes(x = continent, y = number_of_cities, fill = continent)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Distribución de Ciudades por Continente",
       x = "Continente",
       y = "Número de Ciudades") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


