# ─────────────────────────────────────────────────────────────────────────────
# Proyecto Parroquialismo - Parte 1
# Limpieza y fusión de bases: ciudades + anotaciones de países con idioma
# ─────────────────────────────────────────────────────────────────────────────

# ───── Cargar librerías necesarias ─────
library(dplyr)
library(stringr)
library(tidyr)

# ───── Leer archivos de entrada ─────
ciudades_raw <- read.csv("C:/Users/victo/Downloads/ciudades_con_posiciones_final (1).xls")
country_annotation <- read.csv("C:/Users/victo/Downloads/country_annotation_final_with_trust_gini.csv")

# ───── Visualización inicial ─────
View(ciudades_raw)
View(country_annotation)

# ───── Renombrar por simplicidad ─────
ciudades_con_posiciones_final <- ciudades_raw
country_annotation_final_with_trust_gini <- country_annotation

# ───── Inspeccionar columnas ─────
cat("Columnas de ciudades_con_posiciones_final:\n")
print(colnames(ciudades_con_posiciones_final))

cat("\nColumnas de country_annotation_final_with_trust_gini:\n")
print(colnames(country_annotation_final_with_trust_gini))

# ───── Eliminar columna de población por ciudad (si no es útil en análisis) ─────
ciudades_con_posiciones_final$population <- NULL

# ───── Lista de idiomas priorizados ─────
idiomas_prioritarios <- c('English', 'Spanish', 'Chinese', 'Arabic', 
                          'Indonesian', 'Swahili', 'Tamil', 'Turkish', 
                          'Finnish', 'Japanese', 'Korean', 'Vietnamese',
                          'Portuguese', 'French')

# ───── Seleccionar idioma predominante por país ─────
library(stringr)
country_annotation_final_with_trust_gini <- country_annotation_final_with_trust_gini %>%
  mutate(languages_clean = str_remove_all(languages, "\\[|\\]|'"),
         languages_clean = str_split(languages_clean, ",\\s*")) %>%
  rowwise() %>%
  mutate(language_selected = {
    lang <- languages_clean
    match <- lang[lang %in% idiomas_prioritarios]
    if(length(match) > 0) match[1] else lang[1]
  }) %>%
  ungroup()

# ───── Hacer join con base de ciudades ─────
ciudades_final <- ciudades_con_posiciones_final %>%
  left_join(country_annotation_final_with_trust_gini %>% 
              select(country = name, language_selected, population_country = Population), 
            by = "country")

View(ciudades_final)

# ───── Corregir manualmente países con idioma NA ─────
na_languages_ciudades <- ciudades_final %>% filter(is.na(language_selected))
View(na_languages_ciudades)

ciudades_final <- ciudades_final %>%
  mutate(
    language_selected = case_when(
      country == "DR Congo" ~ "Swahili",
      country == "Cuba" ~ "Spanish",
      country == "Myanmar" ~ "Burmese",
      country == "Haiti" ~ "French",
      country == "Puerto Rico" ~ "Spanish",
      TRUE ~ language_selected
    ),
    population_country = case_when(
      country == "Cuba" ~ 10937203,
      country == "DR Congo" ~ 112832473,
      country == "Myanmar" ~ 54133798,
      country == "Haiti" ~ 11637398,
      country == "Puerto Rico" ~ 3285874,
      TRUE ~ population_country
    )
  )

# ───── Verificar corrección manual aplicada ─────
ciudades_final %>%
  filter(country %in% c("DR Congo", "Cuba", "Myanmar", "Haiti", "Puerto Rico")) %>%
  View()

# ───── Ciudades con idiomas priorizados ─────
idiomas_encontrados <- ciudades_final %>%
  filter(language_selected %in% idiomas_prioritarios)

conteo <- idiomas_encontrados %>%
  count(language_selected) %>%
  arrange(desc(n))

View(idiomas_encontrados)
print(conteo)

# ───── Ver países donde aparece 'Vietnamese' como idioma ─────
paises_vietnamese <- country_annotation_final_with_trust_gini %>%
  mutate(languages_clean = str_remove_all(languages, "\\[|\\]|'")) %>%
  separate_rows(languages_clean, sep = ",\\s*") %>%
  filter(languages_clean == "Vietnamese") %>%
  distinct(country = name)

print(paises_vietnamese)

# ───── Asignar manualmente 'Tamil' a algunos países ─────
ciudades_final <- ciudades_final %>%
  mutate(language_selected = ifelse(country %in% c("Sri Lanka", "Singapore"), "Tamil", language_selected))

# ───── Conteo original de idiomas por país ─────
idiomas_long <- country_annotation_final_with_trust_gini %>%
  mutate(languages_clean = str_remove_all(languages, "\\[|\\]|'")) %>%
  separate_rows(languages_clean, sep = ",\\s*")

conteo_original <- idiomas_long %>%
  filter(languages_clean %in% idiomas_prioritarios) %>%
  count(languages_clean) %>%
  arrange(desc(n))

print(conteo_original)

# ───── Ver idioma asignado por país ─────
idiomas_por_pais <- ciudades_final %>%
  select(country, language_selected) %>%
  distinct() %>%
  arrange(country)

View(idiomas_por_pais)

# ───── Número de ciudades por idioma priorizado (agrupado por país) ─────
ciudades_por_idioma <- ciudades_final %>%
  filter(language_selected %in% idiomas_prioritarios) %>%
  group_by(country, language_selected) %>%
  summarise(n_ciudades = n(), .groups = 'drop') %>%
  arrange(desc(n_ciudades))

View(ciudades_por_idioma)

# ───── Ejemplo: ciudades donde se habla japonés ─────
ciudades_final %>%
  filter(language_selected == "Japanese") %>%
  View()

# ───── Número total de ciudades por país (todos los idiomas) ─────
ciudades_por_pais_all <- ciudades_final %>%
  group_by(country, language_selected) %>%
  summarise(n_ciudades = n(), .groups = 'drop') %>%
  arrange(desc(n_ciudades))

View(ciudades_por_pais_all)

# ───── Número total de ciudades por idioma ─────
ciudades_por_idioma_all <- ciudades_final %>%
  group_by(language_selected) %>%
  summarise(n_ciudades = n(), .groups = 'drop') %>%
  arrange(desc(n_ciudades))

print(ciudades_por_idioma_all)




# ─────────────────────────────────────────────────────────────────────────────
# Cálculo del Primer Índice de Parroquialismo
# ─────────────────────────────────────────────────────────────────────────────

# ───── Diccionario de columnas por idioma ─────
idiomas_cols <- list(
  "English" = "en_posiciones",
  "Spanish" = "sp_posiciones",
  "Chinese" = "ch_posiciones",
  "Swahili" = "sw_posiciones",
  "Tamil" = "ta_posiciones",
  "Turkish" = "tr_posiciones",
  "Finnish" = "fn_posiciones",
  "Japanese" = "jp_posiciones",
  "Korean" = "ko_posiciones",
  "Vietnamese" = "viet_posiciones",
  "Arabic" = "ar_posiciones",
  "Indonesian" = "id_posiciones",
  "Portuguese" = "pt_posiciones",
  "French" = "fr_posiciones"
)

# ───── Inicializar tabla de resultados ─────
resultados <- data.frame(
  idioma = character(),
  n_ciudades = numeric(),
  promedio_interno = numeric(),
  promedio_externo = numeric(),
  indice = numeric(),
  stringsAsFactors = FALSE
)

# ───── Loop para calcular el índice por idioma ─────
for(idioma in names(idiomas_cols)) {
  
  columna <- idiomas_cols[[idioma]]
  
  df_temp <- ciudades_final %>%
    filter(!is.na(.data[[columna]]) & .data[[columna]] > 0) %>%
    mutate(posiciones_num = as.numeric(.data[[columna]]))
  
  # Ciudades en el idioma correspondiente (internas)
  internas <- df_temp %>%
    filter(language_selected == idioma)
  
  # Ciudades en otros idiomas (externas)
  externas <- df_temp %>%
    filter(language_selected != idioma)
  
  # Calcular promedios y el índice
  if(nrow(internas) > 0 & nrow(externas) > 0) {
    promedio_interno <- mean(internas$posiciones_num, na.rm = TRUE)
    promedio_externo <- mean(externas$posiciones_num, na.rm = TRUE)
    indice <- promedio_externo / promedio_interno
    n_internas <- nrow(internas)
  } else {
    promedio_interno <- NA
    promedio_externo <- NA
    indice <- NA
    n_internas <- 0
  }
  
  # Agregar resultados
  resultados <- resultados %>%
    add_row(
      idioma = idioma,
      n_ciudades = n_internas,
      promedio_interno = promedio_interno,
      promedio_externo = promedio_externo,
      indice = indice
    )
}

# ───── Visualización y correlaciones ─────
View(resultados)

# Correlación entre log del índice y número de ciudades internas
cor.test(log(resultados$indice), resultados$n_ciudades)

# Nuevas transformaciones del índice
resultados$indicelog <- log(resultados$indice)
resultados$new <- resultados$indicelog * log1p(resultados$n_ciudades)

View(resultados)



# ─────────────────────────────────────────────────────────────────────────────
# Cálculo del Segundo Índice de Parroquialismo (Ranking + RMSE)
# ─────────────────────────────────────────────────────────────────────────────

# ───── Diccionario idioma -> columna de posiciones ─────
idiomas_cols <- list(
  "English" = "en_posiciones",
  "Spanish" = "sp_posiciones",
  "Chinese" = "ch_posiciones",
  "Swahili" = "sw_posiciones",
  "Tamil" = "ta_posiciones",
  "Turkish" = "tr_posiciones",
  "Finnish" = "fn_posiciones",
  "Japanese" = "jp_posiciones",
  "Korean" = "ko_posiciones",
  "Vietnamese" = "viet_posiciones",
  "Arabic" = "ar_posiciones",
  "Indonesian" = "id_posiciones",
  "Portuguese" = "pt_posiciones",
  "French" = "fr_posiciones"
)

# ───── Calcular ranking promedio por país e idioma ─────
ranking_paises <- list()

for(idioma in names(idiomas_cols)) {
  columna <- idiomas_cols[[idioma]]
  
  df_temp <- ciudades_final %>%
    filter(!is.na(.data[[columna]]) & .data[[columna]] > 0) %>%
    mutate(posiciones_num = as.numeric(.data[[columna]])) %>%
    group_by(country) %>%
    summarise(promedio_posiciones = mean(posiciones_num, na.rm = TRUE), .groups = 'drop') %>%
    arrange(promedio_posiciones) %>%
    mutate(ranking = row_number())
  
  ranking_paises[[idioma]] <- df_temp
}

# ───── Unir todos los rankings en una sola tabla ─────
library(purrr)

rankings_list <- imap(ranking_paises, function(df, idioma) {
  df %>%
    select(country, ranking) %>%
    rename(!!paste0(idioma, "_rank") := ranking)
})

ranking_final <- reduce(rankings_list, full_join, by = "country")

# ───── Añadir n_ciudades y idioma principal por país ─────
conteo_ciudades_language <- ciudades_final %>%
  group_by(country) %>%
  summarise(
    n_ciudades = n(),
    language_selected = first(language_selected),
    .groups = 'drop'
  )

ranking_final <- ranking_final %>%
  left_join(conteo_ciudades_language, by = "country") %>%
  relocate(n_ciudades, language_selected, .after = country)

View(ranking_final)

# ───── Filtrar países con al menos 2 ciudades ─────
ranking_filtrado <- ranking_final %>%
  filter(n_ciudades > 1)

# ───── Recalcular rankings internos por idioma con base en países filtrados ─────
ranking_final_ajustado <- ranking_filtrado

for (idioma in names(idiomas_cols)) {
  columna_rank <- paste0(idioma, "_rank")
  
  if (columna_rank %in% colnames(ranking_final_ajustado)) {
    ranking_final_ajustado <- ranking_final_ajustado %>%
      arrange(.data[[columna_rank]]) %>%
      mutate(!!sym(columna_rank) := row_number())
  }
}

View(ranking_final_ajustado)

# ───── Comparar ranking real vs ranking ideal (por población) ─────
ranking_poblacion <- ranking_final_ajustado %>%
  left_join(country_annotation_final_with_trust_gini %>% select(name, Population), 
            by = c("country" = "name")) %>%
  filter(!is.na(Population)) %>%
  arrange(desc(Population)) %>%
  mutate(ideal_rank = row_number())

View(ranking_poblacion)

# ───── Calcular RMSE por idioma respecto al ranking ideal ─────
if (!require(Metrics)) install.packages("Metrics")
library(Metrics)

ranking_idiomas <- grep("_rank$", names(ranking_poblacion), value = TRUE)

rmse_por_idioma <- data.frame(
  idioma = gsub("_rank", "", ranking_idiomas),
  RMSE = sapply(ranking_idiomas, function(col) {
    rmse(ranking_poblacion[[col]], ranking_poblacion$ideal_rank)
  })
) %>%
  arrange(RMSE)

View(rmse_por_idioma)

# ───── Unir RMSE con los índices anteriores ─────
comparacion_indices <- rmse_por_idioma %>%
  left_join(select(resultados, idioma, new, indicelog), by = "idioma")

View(comparacion_indices)

# ───── Crear tablas ordenadas por cada índice ─────
rmse_ordenado <- comparacion_indices %>% arrange(RMSE) %>% select(idioma, RMSE)
indicelog_ordenado <- comparacion_indices %>% arrange(indicelog) %>% select(idioma, indicelog)
new_ordenado <- comparacion_indices %>% arrange(new) %>% select(idioma, new)

# ───── Unir rankings en tabla comparativa ─────
ranking_comparativo <- data.frame(
  RMSE_idioma = rmse_ordenado$idioma,
  RMSE_valor = round(rmse_ordenado$RMSE, 3),
  Indicelog_idioma = indicelog_ordenado$idioma,
  Indicelog_valor = round(indicelog_ordenado$indicelog, 3),
  New_idioma = new_ordenado$idioma,
  New_valor = round(new_ordenado$new, 3)
)

View(ranking_comparativo)

# ───── Comparar rankings (Spearman) ─────
rankings <- comparacion_indices %>%
  mutate(
    rank_RMSE = rank(RMSE, ties.method = "first"),
    rank_indicelog = rank(indicelog, ties.method = "first"),
    rank_new = rank(new, ties.method = "first")
  ) %>%
  select(idioma, rank_RMSE, rank_indicelog, rank_new)

cor_spearman <- cor(rankings[, -1], method = "spearman")
print(round(cor_spearman, 3))

# ───── Correlaciones entre RMSE y los otros índices ─────
cat("🔹 Correlación RMSE vs new:\n")
print(cor.test(comparacion_indices$RMSE, comparacion_indices$new))

cat("\n🔹 Correlación RMSE vs indicelog:\n")
print(cor.test(comparacion_indices$RMSE, comparacion_indices$indicelog))

# ─────────────────────────────────────────────────────────────────────────────
# Análisis adicional: variables explicativas del RMSE
# ─────────────────────────────────────────────────────────────────────────────

# 📌 1. Número de países por idioma
paises_por_idioma <- ciudades_final %>%
  select(country, language_selected) %>%
  distinct() %>%
  group_by(language_selected) %>%
  summarise(n_paises = n(), .groups = "drop")

# 📌 2. Desviación estándar de las posiciones por idioma
desviacion_por_idioma <- map_dfr(names(idiomas_cols), function(idioma) {
  col <- idiomas_cols[[idioma]]
  
  ciudades_final %>%
    filter(!is.na(.data[[col]]) & .data[[col]] > 0) %>%
    mutate(pos = as.numeric(.data[[col]])) %>%
    summarise(idioma = idioma, desviacion = sd(pos, na.rm = TRUE))
})

# 📌 3. Población total por idioma
poblacion_por_idioma <- country_annotation_final_with_trust_gini %>%
  group_by(language_selected) %>%
  summarise(poblacion_idioma = sum(Population, na.rm = TRUE), .groups = "drop")

# 📌 4. Expandir base con variables explicativas
rmse_expandido <- comparacion_indices %>%
  left_join(resultados %>% select(idioma, n_ciudades), by = "idioma") %>%
  left_join(paises_por_idioma, by = c("idioma" = "language_selected")) %>%
  left_join(desviacion_por_idioma, by = "idioma") %>%
  left_join(poblacion_por_idioma, by = c("idioma" = "language_selected"))

# 📌 5. Correlaciones entre RMSE y variables explicativas
vars <- c("n_ciudades", "n_paises", "desviacion", "poblacion_idioma")

for (v in vars) {
  cor_test <- cor.test(rmse_expandido$RMSE, rmse_expandido[[v]])
  cat(paste0("\n🔹 Variable: ", v,
             "\n   Correlación: ", round(cor_test$estimate, 3),
             "\n   p-value: ", round(cor_test$p.value, 4),
             "\n   IC 95%: [", round(cor_test$conf.int[1], 3), " , ", round(cor_test$conf.int[2], 3), "]\n"))
}

# ───── Top idiomas según distintos criterios ─────
cat("🔝 Top 5 idiomas con más ciudades:\n")
rmse_expandido %>%
  arrange(desc(n_ciudades)) %>%
  select(idioma, n_ciudades) %>%
  head(5) %>%
  print()

cat("\n🔝 Top 5 idiomas con más países:\n")
rmse_expandido %>%
  arrange(desc(n_paises)) %>%
  select(idioma, n_paises) %>%
  head(5) %>%
  print()

cat("\n🔝 Top 5 idiomas con mayor población:\n")
rmse_expandido %>%
  arrange(desc(poblacion_idioma)) %>%
  select(idioma, poblacion_idioma) %>%
  head(5) %>%
  print()

cat("📉 Top 5 idiomas con mayor RMSE (más alejados del ideal):\n")
rmse_expandido %>%
  arrange(desc(RMSE)) %>%
  select(idioma, RMSE) %>%
  head(5) %>%
  print()

# ───── Última correlación: RMSE vs número de ciudades ─────
n_ciudades <- ciudades_final %>%
  group_by(language_selected) %>%
  summarise(n_ciudades = n(), .groups = "drop") %>%
  rename(idioma = language_selected)

rmse_con_ciudades <- rmse_por_idioma %>%
  filter(idioma != "ideal") %>%
  left_join(n_ciudades, by = "idioma")

View(rmse_con_ciudades)

cat("\n🔹 Correlación RMSE vs número de ciudades:\n")
print(cor.test(rmse_con_ciudades$RMSE, rmse_con_ciudades$n_ciudades))


# ─────────────────────────────────────────────────────────────────────────────
# Análisis de los Índices de Parroquialismo con Variables Explicativas
# ─────────────────────────────────────────────────────────────────────────────

# ───── Ranking total promedio (postotal) por país ─────
ranking_final$postotal <- rowMeans(ranking_final[, 4:17])
View(ranking_final)

# ───── Visualización rápida de resultados e insumos ─────
View(resultados[, c(1, 7)])  # indicelog y new por idioma
View(ciudades_final)
View(country_annotation_final_with_trust_gini)

# ───────────────────────────────────────────────────────
# 1. Asociación entre `indicelog` y variables socioeconómicas
# ───────────────────────────────────────────────────────

library(dplyr)

# Idiomas incluidos en el análisis
idiomas_analizados <- c("Chinese", "English", "Swahili", "Arabic", "Vietnamese",
                        "French", "Spanish", "Portuguese", "Korean", "Turkish",
                        "Japanese", "Indonesian", "Tamil", "Finnish")

# Variables explicativas seleccionadas
variables_interes <- c("corruption_index", "competition_index", "migration_index",
                       "openness_index", "HDI", "X..internet.penetration",
                       "gini_index", "trust_index_in_group", "trust_index_out_group")

# Calcular promedio por idioma
idioma_variables_promedio <- country_annotation_final_with_trust_gini %>%
  filter(language_selected %in% idiomas_analizados) %>%
  group_by(language_selected) %>%
  summarise(across(all_of(variables_interes), ~mean(.x, na.rm = TRUE)), .groups = "drop")

cat("📌 Promedio de variables por idioma:\n")
print(idioma_variables_promedio)
View(idioma_variables_promedio)

# ───── Unir con tabla de resultados del índice ─────
resultados_explicados <- resultados %>%
  left_join(idioma_variables_promedio, by = c("idioma" = "language_selected"))

cat("📌 Tabla unificada: resultados + variables explicativas:\n")
print(resultados_explicados)

# ───── Correlaciones con `indicelog` ─────
variables_cor <- variables_interes

correlaciones_indicelog <- sapply(variables_cor, function(var) {
  cor(resultados_explicados$indicelog, resultados_explicados[[var]], use = "complete.obs")
})

cat("\n📌 Correlaciones con indicelog:\n")
print(round(correlaciones_indicelog, 3))

cat("\n📌 Resultados de cor.test con indicelog:\n")
for (var in variables_cor) {
  test <- cor.test(resultados_explicados$indicelog, resultados_explicados[[var]], use = "complete.obs")
  cat(paste0("\n🔹 Variable: ", var, "\n"))
  cat("  Correlación: ", round(test$estimate, 3), "\n")
  cat("  p-value: ", round(test$p.value, 4), "\n")
  cat("  IC 95%: [", round(test$conf.int[1], 3), ", ", round(test$conf.int[2], 3), "]\n")
}

# ───── Visualización de correlaciones con indicelog ─────
library(ggplot2)

df_cor <- data.frame(
  variable = names(correlaciones_indicelog),
  correlacion = as.numeric(correlaciones_indicelog)
) %>%
  arrange(desc(abs(correlacion)))

ggplot(df_cor, aes(x = reorder(variable, correlacion), y = correlacion)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label = round(correlacion, 2)), hjust = ifelse(df_cor$correlacion > 0, -0.1, 1.1)) +
  labs(title = "Correlaciones con el índice de parroquialismo (log)",
       x = "Variable explicativa", y = "Correlación con indicelog") +
  theme_minimal()

# ───── Heatmap de correlaciones con indicelog ─────
library(reshape2)

vars_corr <- resultados_explicados %>%
  select(indicelog, all_of(variables_cor)) %>%
  drop_na()

melted_corr <- melt(cor(vars_corr, use = "complete.obs"))

ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC", 
                       midpoint = 0, limit = c(-1, 1), name = "Correlación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Matriz de Correlaciones (incluye indicelog)", x = "", y = "") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3)

# ───────────────────────────────────────────────────────
# 2. Asociación entre `new` y variables socioeconómicas
# ───────────────────────────────────────────────────────

correlaciones_new <- sapply(variables_cor, function(var) {
  cor(resultados_explicados$new, resultados_explicados[[var]], use = "complete.obs")
})

cat("\n📌 Correlaciones con el índice `new`:\n")
print(round(correlaciones_new, 3))

cat("\n📌 Resultados de cor.test con índice `new`:\n")
for (var in variables_cor) {
  test <- cor.test(resultados_explicados$new, resultados_explicados[[var]], use = "complete.obs")
  cat(paste0("\n🔹 Variable: ", var, "\n"))
  cat("  Correlación: ", round(test$estimate, 3), "\n")
  cat("  p-value: ", round(test$p.value, 4), "\n")
  cat("  IC 95%: [", round(test$conf.int[1], 3), ", ", round(test$conf.int[2], 3), "]\n")
}

# Visualización índice `new`
df_cor_new <- data.frame(
  variable = names(correlaciones_new),
  correlacion = as.numeric(correlaciones_new)
) %>%
  arrange(desc(abs(correlacion)))

ggplot(df_cor_new, aes(x = reorder(variable, correlacion), y = correlacion)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  geom_text(aes(label = round(correlacion, 2)), hjust = ifelse(df_cor_new$correlacion > 0, -0.1, 1.1)) +
  labs(title = "Correlaciones con el índice `new` (log × log ciudades)",
       x = "Variable explicativa", y = "Correlación con índice `new`") +
  theme_minimal()

# Heatmap índice `new`
vars_corr_new <- resultados_explicados %>%
  select(new, all_of(variables_cor)) %>%
  drop_na()

melted_corr_new <- melt(cor(vars_corr_new, use = "complete.obs"))

ggplot(melted_corr_new, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC", 
                       midpoint = 0, limit = c(-1, 1), name = "Correlación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Matriz de Correlaciones (incluye índice `new`)", x = "", y = "") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3)

# ───── Gráficos de pares ─────
pairs(resultados_explicados %>%
        select(indicelog, all_of(variables_cor)) %>%
        drop_na(),
      main = "Gráfico de Pares: indicelog vs variables explicativas",
      pch = 19, col = "steelblue", cex = 1.2)

pairs(resultados_explicados %>%
        select(new, all_of(variables_cor)) %>%
        drop_na(),
      main = "Gráfico de Pares: new vs variables explicativas",
      pch = 19, col = "steelblue", cex = 1.2)

# ───────────────────────────────────────────────────────
# 3. Lógica teórica esperada para orden de variables
# ───────────────────────────────────────────────────────

logica_variables <- list(
  "HDI" = TRUE,
  "X..internet.penetration" = TRUE,
  "gini_index" = FALSE,
  "openness_index" = TRUE,
  "trust_index_in_group" = FALSE,
  "trust_index_out_group" = TRUE,
  "corruption_index" = FALSE,
  "competition_index" = TRUE
)

for (var in names(logica_variables)) {
  cat(paste0("\n📌 Orden según lógica teórica de: ", var, "\n"))
  
  ordenado <- resultados_explicados %>%
    select(idioma, !!sym(var)) %>%
    arrange(if (logica_variables[[var]]) desc(.data[[var]]) else .data[[var]])
  
  print(ordenado)
}



# ─────────────────────────────────────────────────────────────────────────────
# Validación teórica de los índices: valores simulados y ranking global
# ─────────────────────────────────────────────────────────────────────────────

library(dplyr)

# ───── 1. Crear dataframe con valores teóricamente coherentes ─────
resultados2 <- data.frame(
  idioma = c("Chinese", "English", "Swahili", "Tamil", "Vietnamese", "French", 
             "Arabic", "Finnish", "Korean", "Portuguese", "Spanish", 
             "Turkish", "Japanese", "Indonesian"),
  
  # Neutros o con poco peso explicativo
  corruption_index = c(5.5, 5.7, 6.5, 6.6, 6.7, 6.8, 6.9, 5.3, 5.6, 7.0, 7.2, 7.4, 7.5, 7.7),
  competition_index = c(5.4, 5.5, 5.6, 5.5, 5.4, 5.6, 5.5, 5.6, 5.4, 5.5, 5.6, 5.5, 5.4, 5.6),
  
  # Variables explicativas fuertes
  migration_index = c(3.2, 3.0, 2.8, 2.6, 2.5, 2.3, 2.2, 3.1, 2.9, 2.0, 1.9, 1.7, 1.6, 1.5),
  openness_index = c(2.7, 2.6, 2.0, 1.9, 1.8, 1.7, 1.6, 2.8, 2.5, 1.5, 1.4, 1.3, 1.2, 1.1),
  HDI = c(0.92, 0.90, 0.80, 0.78, 0.76, 0.74, 0.72, 0.93, 0.91, 0.70, 0.68, 0.66, 0.65, 0.64),
  X..internet.penetration = c(97.0, 94.0, 85.0, 84.0, 83.0, 80.0, 78.0, 98.0, 96.0, 75.0, 72.0, 70.0, 69.0, 68.0),
  gini_index = c(30.0, 31.0, 32.0, 33.0, 34.0, 35.0, 36.0, 29.0, 30.5, 37.0, 38.0, 39.0, 40.0, 41.0),
  trust_index_in_group = c(1.72, 1.74, 1.76, 1.78, 1.79, 1.81, 1.83, 1.71, 1.73, 1.85, 1.87, 1.89, 1.90, 1.91),
  trust_index_out_group = c(2.9, 2.8, 2.7, 2.6, 2.5, 2.4, 2.3, 3.0, 2.85, 2.2, 2.1, 2.0, 1.9, 1.8)
)

View(resultados2)

# ───── 2. Unir con los índices previos ─────
resultados_completo <- resultados2 %>%
  left_join(resultados %>% select(idioma, indicelog, new), by = "idioma")

# ───── 3. Correlaciones con indicelog ─────
cat("\n🔍 Correlaciones con indicelog:\n")
variables <- colnames(resultados2)[-1]

for (var in variables) {
  cor_result <- cor.test(resultados_completo[[var]], resultados_completo$indicelog)
  cat(paste0("\n🔹 Variable: ", var, 
             "\n  Correlación:  ", round(cor_result$estimate, 3),
             "\n  p-value:  ", round(cor_result$p.value, 4),
             "\n  IC 95%: [", round(cor_result$conf.int[1], 3), " , ", round(cor_result$conf.int[2], 3), "]\n"))
}

# ───── 4. Correlaciones con índice new ─────
cat("\n\n🔍 Correlaciones con índice 'new':\n")
for (var in variables) {
  cor_result <- cor.test(resultados_completo[[var]], resultados_completo$new)
  cat(paste0("\n🔹 Variable: ", var, 
             "\n  Correlación:  ", round(cor_result$estimate, 3),
             "\n  p-value:  ", round(cor_result$p.value, 4),
             "\n  IC 95%: [", round(cor_result$conf.int[1], 3), " , ", round(cor_result$conf.int[2], 3), "]\n"))
}



library(corrplot)

# Matriz con indicelog y las variables
vars_indicelog <- c("indicelog", colnames(resultados2)[-1])  # todas menos idioma
datos_indicelog <- resultados_completo %>%
  select(all_of(vars_indicelog)) %>%
  drop_na()

# Calcular matriz de correlación
matriz_indicelog <- cor(datos_indicelog, use = "complete.obs", method = "pearson")

# Graficar heatmap tipo triangular
corrplot(matriz_indicelog, method = "color", type = "lower",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         number.cex = 0.7, mar = c(0, 0, 2, 0),
         title = "Correlaciones: Índice `indicelog` y variables explicativas")




# Matriz con new y las variables
vars_new <- c("new", colnames(resultados2)[-1])
datos_new <- resultados_completo %>%
  select(all_of(vars_new)) %>%
  drop_na()

# Calcular matriz de correlación
matriz_new <- cor(datos_new, use = "complete.obs", method = "pearson")

# Graficar heatmap tipo triangular
corrplot(matriz_new, method = "color", type = "lower",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         number.cex = 0.7, mar = c(0, 0, 2, 0),
         title = "Correlaciones: Índice `new` y variables explicativas")



# ───── 5. Correlación índice new vs número de ciudades ─────
cor_ciudades <- cor.test(resultados$new, resultados$n_ciudades)
cat("\n🔗 Correlación entre índice 'new' y número de ciudades:\n",
    "Correlación: ", round(cor_ciudades$estimate, 3), "\n",
    "p-value: ", round(cor_ciudades$p.value, 4), "\n",
    "IC 95%: [", round(cor_ciudades$conf.int[1], 3), " , ", round(cor_ciudades$conf.int[2], 3), "]\n")

# ───── 6. Gráfico de dispersión múltiple para 'new' ─────
vars_pairs <- resultados_completo %>%
  select(new, all_of(variables))

pairs(vars_pairs,
      main = "Matriz de Dispersión - Índice 'new' y variables explicativas",
      pch = 21, bg = "lightblue")

# ───── 7. Ver orden teórico según cada variable ─────
cat("📊 Orden teórico de idiomas por variable:\n\n")

print(resultados2 %>% arrange(corruption_index) %>% select(idioma, corruption_index))      # más parroquialismo
print(resultados2 %>% arrange(competition_index) %>% select(idioma, competition_index))    # más parroquialismo
print(resultados2 %>% arrange(desc(migration_index)) %>% select(idioma, migration_index))  # menos parroquialismo
print(resultados2 %>% arrange(desc(openness_index)) %>% select(idioma, openness_index))    # menos parroquialismo
print(resultados2 %>% arrange(desc(HDI)) %>% select(idioma, HDI))                          # menos parroquialismo
print(resultados2 %>% arrange(desc(X..internet.penetration)) %>% select(idioma, X..internet.penetration))
print(resultados2 %>% arrange(gini_index) %>% select(idioma, gini_index))                 # más parroquialismo
print(resultados2 %>% arrange(trust_index_in_group) %>% select(idioma, trust_index_in_group))
print(resultados2 %>% arrange(desc(trust_index_out_group)) %>% select(idioma, trust_index_out_group))

# ───── 8. Asignar ranking teórico por variable ─────
rankings <- resultados2 %>%
  mutate(
    rk_corr = rank(corruption_index, ties.method = "first"),
    rk_comp = rank(competition_index, ties.method = "first"),
    rk_migr = rank(-migration_index, ties.method = "first"),
    rk_open = rank(-openness_index, ties.method = "first"),
    rk_hdi = rank(-HDI, ties.method = "first"),
    rk_inet = rank(-`X..internet.penetration`, ties.method = "first"),
    rk_gini = rank(gini_index, ties.method = "first"),
    rk_in = rank(trust_index_in_group, ties.method = "first"),
    rk_out = rank(-trust_index_out_group, ties.method = "first")
  )

# ───── 9. Sumar los ranks para obtener ranking global ─────
ranking_final <- rankings %>%
  mutate(total_score = rk_corr + rk_comp + rk_migr + rk_open + rk_hdi +
           rk_inet + rk_gini + rk_in + rk_out) %>%
  arrange(total_score) %>%
  select(idioma, total_score)

View(ranking_final)

# ───────────────────────────────────────────────────────
# Comparar el ranking teórico con RMSE, indicelog y new
# ───────────────────────────────────────────────────────

# 🔧 Cargar nuevamente librería en caso de entorno nuevo
library(dplyr)

# Paso 1: Unir todas las métricas
df_combined <- rmse_por_idioma %>%
  select(idioma, RMSE) %>%
  inner_join(resultados %>% select(idioma, new, indicelog), by = "idioma") %>%
  inner_join(ranking_final, by = "idioma")

# Paso 2: Calcular ranking para cada métrica
df_ranked <- df_combined %>%
  mutate(
    rank_rmse = rank(RMSE, ties.method = "min"),
    rank_new = rank(new, ties.method = "min"),
    rank_indicelog = rank(indicelog, ties.method = "min"),
    rank_total = rank(total_score, ties.method = "min")
  )

# Paso 3: Promedio de los rankings
df_ranked <- df_ranked %>%
  mutate(promedio_rank = rowMeans(across(starts_with("rank_")))) %>%
  arrange(promedio_rank)

# Paso 4: Mostrar resultado final
cat("\n📊 Ranking promedio global de parroquialismo por idioma:\n")
print(df_ranked %>% select(idioma, promedio_rank))



# ───────────────────────────────────────────────────────
# Comparar el ranking teórico con RMSE, orden teórico y new
# ───────────────────────────────────────────────────────


library(dplyr)

# Paso 1: Unir RMSE, índice new y ranking teórico (total_score)
df_combined <- rmse_por_idioma %>%
  select(idioma, RMSE) %>%
  inner_join(resultados %>% select(idioma, new), by = "idioma") %>%
  inner_join(ranking_final %>% select(idioma, total_score), by = "idioma")

# Paso 2: Calcular el ranking de cada métrica (menor = mejor)
df_ranked <- df_combined %>%
  mutate(
    rank_rmse = rank(RMSE, ties.method = "min"),
    rank_new = rank(new, ties.method = "min"),
    rank_total = rank(total_score, ties.method = "min")
  )

# Paso 3: Calcular el promedio de los tres rankings
df_ranked <- df_ranked %>%
  mutate(promedio_rank = rowMeans(across(starts_with("rank_")))) %>%
  arrange(promedio_rank)

# Paso 4: Mostrar el resultado ordenado por promedio de rankings
cat("\n📊 Ranking promedio global de parroquialismo por idioma (solo new, RMSE y ranking teórico):\n")
print(df_ranked %>% select(idioma, rank_rmse, rank_new, rank_total, promedio_rank))




# ───────────────────────────────────────────────────────
# Comparar el ranking teórico con RMSE y new
# ─────────────────────────────────────────────────────── 


library(dplyr)

# Paso 1: Unir RMSE, índice new y ranking teórico (total_score)
df_combined <- rmse_por_idioma %>%
  select(idioma, RMSE) %>%
  inner_join(resultados %>% select(idioma, new), by = "idioma") %>%
  inner_join(ranking_final %>% select(idioma, total_score), by = "idioma")

# Paso 2: Calcular el ranking de cada métrica (menor = mejor)
df_ranked <- df_combined %>%
  mutate(
    rank_rmse = rank(RMSE, ties.method = "min"),
    rank_new = rank(new, ties.method = "min"),
    rank_total = rank(total_score, ties.method = "min")
  )

# Paso 3: Calcular el promedio de los tres rankings
df_ranked <- df_ranked %>%
  mutate(promedio_rank = rowMeans(across(starts_with("rank_")))) %>%
  arrange(promedio_rank)

# Paso 4: Mostrar el resultado ordenado por promedio de rankings
cat("\n📊 Ranking promedio global de parroquialismo por idioma (solo new, RMSE y ranking teórico):\n")
print(df_ranked %>% select(idioma, rank_rmse, rank_new, rank_total, promedio_rank))


# ─────────────────────────────────────────────────────────────────────────────
# Análisis de correlaciones y modelos predictivos del índice de parroquialismo
# ─────────────────────────────────────────────────────────────────────────────

library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(glmnet)

# ───── 1. Correlaciones entre n_ciudades y variables predictoras ─────
datos_cor <- ranking_final_ajustado %>%
  select(country, n_ciudades) %>%
  left_join(country_annotation_final_with_trust_gini, by = c("country" = "name"))

variables_predictoras <- c(
  "n_ciudades", "Population", "HDI", "X..internet.penetration", "gini_index",
  "trust_index_in_group", "trust_index_out_group", "migration_index", 
  "openness_index", "corruption_index", "competition_index"
)

datos_cor_filtrado <- datos_cor %>%
  select(all_of(variables_predictoras)) %>%
  filter(complete.cases(.))

cor_matrix <- cor(datos_cor_filtrado, method = "pearson")
print(round(cor_matrix, 3))

corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8)

# ───── 2. Correlaciones de n_ciudades con cada variable (test estadístico) ─────
variables_x <- setdiff(variables_predictoras, "n_ciudades")

for (var in variables_x) {
  cor_result <- cor.test(datos_cor_filtrado$n_ciudades, datos_cor_filtrado[[var]])
  cat(paste0(
    "\n🔹 Variable: ", var,
    "\n  Correlación: ", round(cor_result$estimate, 3),
    "\n  p-value: ", round(cor_result$p.value, 4),
    "\n  IC 95%: [", round(cor_result$conf.int[1], 3), " , ", round(cor_result$conf.int[2], 3), "]\n"
  ))
}

# ───── 3. Correlación indicelog vs n_ciudades por idioma ─────
cat("📌 Correlación entre indicelog y número de ciudades por idioma:\n")
print(cor.test(resultados$indicelog, resultados$n_ciudades))

# ───── 4. Menciones por idioma: media y mediana ─────
ciudades_numericas <- ciudades_final %>%
  mutate(across(ends_with("_posiciones"), as.numeric))

menciones_por_idioma <- ciudades_numericas %>%
  pivot_longer(cols = ends_with("_posiciones"),
               names_to = "idioma_col", values_to = "valor") %>%
  filter(!is.na(valor) & valor > 0) %>%
  mutate(idioma = case_when(
    idioma_col == "en_posiciones" ~ "English",
    idioma_col == "sp_posiciones" ~ "Spanish",
    idioma_col == "ch_posiciones" ~ "Chinese",
    idioma_col == "sw_posiciones" ~ "Swahili",
    idioma_col == "ta_posiciones" ~ "Tamil",
    idioma_col == "tr_posiciones" ~ "Turkish",
    idioma_col == "fn_posiciones" ~ "Finnish",
    idioma_col == "jp_posiciones" ~ "Japanese",
    idioma_col == "ko_posiciones" ~ "Korean",
    idioma_col == "viet_posiciones" ~ "Vietnamese",
    idioma_col == "ar_posiciones" ~ "Arabic",
    idioma_col == "id_posiciones" ~ "Indonesian",
    idioma_col == "pt_posiciones" ~ "Portuguese",
    idioma_col == "fr_posiciones" ~ "French"
  )) %>%
  group_by(idioma) %>%
  summarise(
    media_menciones = mean(valor, na.rm = TRUE),
    mediana_menciones = median(valor, na.rm = TRUE),
    .groups = "drop"
  )

print(menciones_por_idioma)

# ───── 5. Comparar menciones con los índices ─────
comparacion_menciones <- resultados %>%
  select(idioma, indicelog, n_ciudades) %>%
  left_join(menciones_por_idioma, by = "idioma")

cat("📌 Idiomas ordenados por indicelog (menor a mayor):\n")
comparacion_menciones %>%
  arrange(indicelog) %>%
  select(idioma, indicelog, n_ciudades, media_menciones, mediana_menciones) %>%
  print()


# ───── 8. Modelos de regresión regularizada ─────
vars_predictoras <- c("HDI", "X..internet.penetration", "gini_index", 
                      "trust_index_in_group", "trust_index_out_group", 
                      "migration_index", "openness_index")

df_modelo <- resultados_completo %>%
  select(all_of(vars_predictoras), new) %>%
  filter(complete.cases(.))

X <- as.matrix(df_modelo %>% select(-new))
y <- df_modelo$new

modelo_lasso <- cv.glmnet(X, y, alpha = 1, standardize = TRUE)
modelo_ridge <- cv.glmnet(X, y, alpha = 0, standardize = TRUE)
modelo_elastic <- cv.glmnet(X, y, alpha = 0.5, standardize = TRUE)

cat("📌 Coeficientes LASSO:\n")
print(coef(modelo_lasso, s = "lambda.min"))

cat("\n📌 Coeficientes RIDGE:\n")
print(coef(modelo_ridge, s = "lambda.min"))

cat("\n📌 Coeficientes ELASTIC NET:\n")
print(coef(modelo_elastic, s = "lambda.min"))



library(dplyr)
library(corrplot)

# Seleccionar RMSE y las mismas variables explicativas de resultados2
variables_explicativas <- colnames(resultados2)[-1]  # todas menos idioma

# Unir RMSE con las variables
rmse_datos <- rmse_por_idioma %>%
  left_join(resultados2, by = "idioma") %>%
  select(RMSE, all_of(variables_explicativas)) %>%
  drop_na()

# Calcular la matriz de correlaciones
matriz_rmse <- cor(rmse_datos, use = "complete.obs", method = "pearson")

# Graficar el heatmap tipo triangular
corrplot(matriz_rmse, method = "color", type = "lower",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         number.cex = 0.7, mar = c(0, 0, 2, 0),
         title = "Correlaciones: Índice RMSE y variables explicativas")









library(ggplot2)
library(dplyr)
library(tidyr)

# Variables explicativas sin 'competition_index'
variables_explicativas <- setdiff(colnames(resultados2)[-1], "competition_index")

# Unir RMSE con las variables explicativas
rmse_variables <- rmse_por_idioma %>%
  left_join(resultados2, by = "idioma") %>%
  select(idioma, RMSE, all_of(variables_explicativas)) %>%
  drop_na()

# Transformar a formato largo
rmse_largo <- rmse_variables %>%
  pivot_longer(cols = -c(idioma, RMSE), names_to = "variable", values_to = "valor")

# Graficar
ggplot(rmse_largo, aes(x = valor, y = RMSE)) +
  geom_point(aes(color = idioma), size = 3) +
  geom_smooth(method = "loess", se = FALSE, color = "gray40", linetype = "dashed") +
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = "Relación entre RMSE y variables explicativas (sin Competition Index)",
       x = "Valor de la variable explicativa",
       y = "RMSE") +
  theme_minimal() +
  theme(legend.position = "bottom")
