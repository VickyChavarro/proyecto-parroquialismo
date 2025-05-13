
# ─────────────────────────────────────────────────────────────────────
# CARGAR LIBRERÍAS NECESARIAS
# ─────────────────────────────────────────────────────────────────────
library(sf)
library(dplyr)
library(ggplot2)
library(cartogram)
library(countrycode)
library(leaflet)



# ─────────────────────────────────────────────────────────────────────
# CARGAR LIBRERÍAS NECESARIAS (por si no están)
# ─────────────────────────────────────────────────────────────────────
library(sf)
library(dplyr)
library(ggplot2)
library(countrycode)


# ─────────────────────────────────────────────────────────────────────
# LEER EL SHAPEFILE MUNDIAL
# ─────────────────────────────────────────────────────────────────────
# (asumo que ya cargaste tu shapefile "world")

# ─────────────────────────────────────────────────────────────────────
# PREPARAR BASE: Calcular 1/ideal_rank y agrupar en quintiles
# ─────────────────────────────────────────────────────────────────────
ranking_poblacion <- ranking_poblacion %>%
  mutate(
    ISO_A3 = countrycode(country, origin = "country.name", destination = "iso3c"),
    ideal_rank_invertido = 1 / ideal_rank
  ) %>%
  mutate(
    grupo_ranking_ideal = ntile(ideal_rank_invertido, 5),
    grupo_ranking_ideal = factor(grupo_ranking_ideal,
                                 levels = 1:5,
                                 labels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))
  )

# ─────────────────────────────────────────────────────────────────────
# UNIR BASE CON SHAPEFILE
# ─────────────────────────────────────────────────────────────────────
world_ideal <- left_join(world, ranking_poblacion, by = "ISO_A3")

# ─────────────────────────────────────────────────────────────────────
# CREAR EL MAPA
# ─────────────────────────────────────────────────────────────────────
colores_importancia <- c(
  "Muy bajo"  = "#ffffb2",
  "Bajo"      = "#fecc5c",
  "Medio"     = "#fd8d3c",
  "Alto"      = "#f03b20",
  "Muy alto"  = "#bd0026"
)

ggplot() +
  geom_sf(data = world_ideal, fill = "gray90", color = "gray60", size = 0.1) +
  
  geom_sf(
    data = filter(world_ideal, !is.na(grupo_ranking_ideal)),
    aes(fill = grupo_ranking_ideal), color = "white", size = 0.2
  ) +
  
  scale_fill_manual(
    values = colores_importancia,
    name = "Importancia ideal (1/ranking)"
  ) +
  
  labs(
    title = "Escenario ideal de importancia mundial (sin parroquialismo)",
    subtitle = "Colores indican quintiles de importancia teórica ideal",
    caption = "Fuente: ranking_poblacion"
  ) +
  theme_minimal() +
  theme(legend.position = "right")
























View(ranking_final_ajustado)
# ─────────────────────────────────────────────────────────────────────
# LEER EL SHAPEFILE MUNDIAL
# ─────────────────────────────────────────────────────────────────────
# Cambia esta ruta si tu archivo está en otro lugar
world <- st_read("C:/Users/victo/Downloads/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

# ─────────────────────────────────────────────────────────────────────
# BASE DE RANKING CON LOS DATOS NECESARIOS (mínimo country y *_rank)
# ─────────────────────────────────────────────────────────────────────
# Debe contener columnas como English_rank, Spanish_rank, etc.
# Calcular ISO 3 y ranking invertido para idioma inglés
ranking_final_ajustado <- ranking_final_ajustado %>%
  mutate(
    ISO_A3 = countrycode(country, origin = "country.name", destination = "iso3c"),
    English_rank_invertido = 1 / English_rank
  )


# ─────────────────────────────────────────────────────────────────────────────
# GGPLOT MAPAMUNDI ESTÁTICO 
# ─────────────────────────────────────────────────────────────────────────────

ggplot_mapa_mundial_por_idioma <- function(idioma, datos_ranking, world_shapefile) {
  library(dplyr)
  library(ggplot2)
  library(countrycode)
  library(sf)
  
  # 📌 Crear nombres dinámicos de columnas
  columna_rank <- paste0(idioma, "_rank")
  columna_inv <- paste0(idioma, "_rank_invertido")
  
  # 🎯 Recalcular ranking invertido y agrupar por quintiles
  datos_ranking <- datos_ranking %>%
    mutate(
      ISO_A3 = countrycode(country, origin = "country.name", destination = "iso3c"),
      !!columna_inv := 1 / .data[[columna_rank]]
    ) %>%
    mutate(
      grupo_ranking = ntile(.data[[columna_inv]], 5),
      grupo_ranking = factor(grupo_ranking,
                             levels = 1:5,
                             labels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))
    )
  
  # 🔗 Unir con shapefile
  world_data <- left_join(world_shapefile, datos_ranking, by = "ISO_A3")
  
  # 🎨 Paleta de colores personalizada
  colores_importancia <- c(
    "Muy bajo"  = "#ffffb2",
    "Bajo"      = "#fecc5c",
    "Medio"     = "#fd8d3c",
    "Alto"      = "#f03b20",
    "Muy alto"  = "#bd0026"
  )
  
  # 🗺️ Visualización
  ggplot() +
    # Base: todos los países
    geom_sf(data = world_data, fill = "gray90", color = "gray60", size = 0.1) +
    
    # Capa con datos
    geom_sf(
      data = filter(world_data, !is.na(grupo_ranking)),
      aes(fill = grupo_ranking), color = "white", size = 0.2
    ) +
    
    scale_fill_manual(
      values = colores_importancia,
      name = "Importancia (1/ranking)"
    ) +
    
    labs(
      title = paste("Importancia de países en idioma", idioma, "(grupos de ranking)"),
      subtitle = "Colores indican quintiles de importancia según 1 / ranking",
      caption = "Fuente: ranking_final_ajustado"
    ) +
    theme_minimal() +
    theme(legend.position = "right")
}


ggplot_mapa_mundial_por_idioma("English", ranking_final_ajustado, world)
ggplot_mapa_mundial_por_idioma("Spanish", ranking_final_ajustado, world)
ggplot_mapa_mundial_por_idioma("Portuguese", ranking_final_ajustado, world)
ggplot_mapa_mundial_por_idioma("French", ranking_final_ajustado, world)
ggplot_mapa_mundial_por_idioma("Chinese", ranking_final_ajustado, world)
ggplot_mapa_mundial_por_idioma("Arabic", ranking_final_ajustado, world)
ggplot_mapa_mundial_por_idioma("Indonesian", ranking_final_ajustado, world)
ggplot_mapa_mundial_por_idioma("Swahili", ranking_final_ajustado, world)
ggplot_mapa_mundial_por_idioma("Tamil", ranking_final_ajustado, world)
ggplot_mapa_mundial_por_idioma("Turkish", ranking_final_ajustado, world)
ggplot_mapa_mundial_por_idioma("Finnish", ranking_final_ajustado, world)
ggplot_mapa_mundial_por_idioma("Japanese", ranking_final_ajustado, world)
ggplot_mapa_mundial_por_idioma("Korean", ranking_final_ajustado, world)
ggplot_mapa_mundial_por_idioma("Vietnamese", ranking_final_ajustado, world)



# ─────────────────────────────────────────────────────────────────────────────
# LEAFLET MAPAMUNDI INTERACTIVO
# ─────────────────────────────────────────────────────────────────────────────



preparar_datos_leaflet <- function(idioma, datos_ranking, world_shapefile) {
  columna_rank <- paste0(idioma, "_rank")
  columna_inv <- paste0(idioma, "_rank_invertido")
  
  datos_ranking <- datos_ranking %>%
    mutate(
      ISO_A3 = countrycode(country, origin = "country.name", destination = "iso3c"),
      !!columna_inv := 1 / .data[[columna_rank]]
    ) %>%
    mutate(
      grupo_ranking = ntile(.data[[columna_inv]], 5),
      grupo_ranking = factor(grupo_ranking, levels = 1:5,
                             labels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))
    )
  
  # Unir con shapefile
  world_data <- left_join(world_shapefile, datos_ranking, by = "ISO_A3")
  return(world_data)
}


# Preparar los datos con ranking para el idioma English
world_con_datos <- preparar_datos_leaflet("Spanish", ranking_final_ajustado, world)

leaflet_por_idioma <- function(idioma, datos_mundiales) {
  library(leaflet)
  library(dplyr)
  library(htmltools)
  
  # Variables dinámicas
  columna_rank <- paste0(idioma, "_rank")
  columna_grupo <- "grupo_ranking"
  
  # Asegurar orden del factor
  niveles_ordenados <- c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")
  datos_mundiales[[columna_grupo]] <- factor(datos_mundiales[[columna_grupo]], levels = niveles_ordenados)
  
  # Paleta de colores
  colores_importancia <- c(
    "Muy bajo"  = "#ffffb2",
    "Bajo"      = "#fecc5c",
    "Medio"     = "#fd8d3c",
    "Alto"      = "#f03b20",
    "Muy alto"  = "#bd0026"
  )
  
  pal <- colorFactor(
    palette = colores_importancia[niveles_ordenados],
    domain = factor(niveles_ordenados, levels = niveles_ordenados),
    ordered = TRUE,
    na.color = "#f0f0f0"
  )
  
  # Crear popup usando pull
  datos_mundiales <- datos_mundiales %>%
    mutate(popup = ifelse(
      is.na(!!sym(columna_rank)),
      paste0("<strong>", NAME, "</strong><br>Sin datos de ranking"),
      paste0("<strong>", NAME, "</strong><br>Ranking: ", !!sym(columna_rank), "<br>Grupo: ", grupo_ranking)
    ))
  
  # Mapa interactivo
  leaflet(datos_mundiales) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    # Base: todos los países
    addPolygons(
      fillColor = "#f0f0f0",
      color = "gray80",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.5
    ) %>%
    
    # Países con datos
    addPolygons(
      data = filter(datos_mundiales, !is.na(grupo_ranking)),
      fillColor = ~pal(grupo_ranking),
      color = "white",
      weight = 1,
      fillOpacity = 0.9,
      highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE),
      label = ~lapply(popup, HTML)
    ) %>%
    
    # Leyenda
    addLegend(
      position = "bottomright",
      pal = pal,
      values = datos_mundiales$grupo_ranking,
      title = paste("Importancia (1/ranking) -", idioma),
      opacity = 0.8
    )
}



# Pasar ese nuevo objeto a la función leaflet
leaflet_por_idioma("Spanish", world_con_datos)
# ─────────────────────────────────────────────────────────────────────────────
# GUARDAR LEAFLET MAPAMUNDI INTERACTIVO 
# ─────────────────────────────────────────────────────────────────────────────

# Instalar si no tienes
install.packages("htmlwidgets")
library(htmlwidgets)

# Guardar el mapa en un objeto
mapa_ingles <- leaflet(world_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = "#f0f0f0", color = "gray80", weight = 1, opacity = 1, fillOpacity = 0.5
  ) %>%
  addPolygons(
    data = filter(world_data, !is.na(grupo_ranking)),
    fillColor = ~pal(grupo_ranking),
    color = "white",
    weight = 1,
    fillOpacity = 0.9,
    highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE),
    label = ~lapply(popup, htmltools::HTML)
  ) %>%
  addLegend("bottomright", pal = pal, values = ~grupo_ranking,
            title = "Importancia (grupo)", opacity = 0.8)

# Guardar como archivo HTML interactivo
saveWidget(mapa_ingles, file = "C:/Users/victo/Downloads/mapa_ranking_ingles.html", selfcontained = FALSE)




# ─────────────────────────────────────────────────────────────────────────────
# CARTOGRAM POR CONTINENTE 
# ─────────────────────────────────────────────────────────────────────────────


cartograma_por_idioma <- function(idioma, continente, datos_ranking, world_shapefile) {
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(cartogram)
  library(countrycode)
  
  # 📌 Preparar nombres de variables
  columna_rank <- paste0(idioma, "_rank")
  columna_inv <- paste0(idioma, "_rank_invertido")
  columna_reforzado <- paste0(idioma, "_rank_reforzado")
  
  # 📥 1. Preparar ranking (solo si las columnas aún no existen)
  datos_ranking <- datos_ranking %>%
    mutate(
      ISO_A3 = countrycode(country, origin = "country.name", destination = "iso3c"),
      !!columna_inv := 1 / .data[[columna_rank]],
      !!columna_reforzado := (.data[[columna_inv]])^2
    ) %>%
    mutate(
      grupo_ranking = ntile(.data[[columna_inv]], 5),
      grupo_ranking = factor(grupo_ranking, levels = 1:5,
                             labels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))
    )
  
  # 🔗 2. Unir shapefile con ranking
  world_data <- left_join(world_shapefile, datos_ranking, by = "ISO_A3")
  
  # 🌎 3. Filtrar continente (uno o varios)
  continente_data <- world_data %>%
    filter(CONTINENT %in% continente)
  
  # 🔍 4. Separar países con y sin datos
  con_dato <- continente_data %>%
    filter(!is.na(.data[[columna_reforzado]]), .data[[columna_reforzado]] > 0) %>%
    st_transform(3857)
  
  sin_dato <- continente_data %>%
    filter(is.na(.data[[columna_reforzado]])) %>%
    st_transform(3857)
  
  # 🧼 5. Validar geometrías
  con_dato <- st_make_valid(con_dato[!st_is_empty(con_dato), ])
  sin_dato <- st_make_valid(sin_dato[!st_is_empty(sin_dato), ])
  
  # 🗺️ 6. Cartograma con ranking reforzado
  cartograma <- cartogram_cont(con_dato, columna_reforzado, itermax = 5)
  cartograma <- st_transform(cartograma, st_crs(world_shapefile))
  sin_dato <- st_transform(sin_dato, st_crs(world_shapefile))
  
  # 🎨 7. Colores
  colores_importancia <- c(
    "Muy bajo"  = "#ffffb2",
    "Bajo"      = "#fecc5c",
    "Medio"     = "#fd8d3c",
    "Alto"      = "#f03b20",
    "Muy alto"  = "#bd0026"
  )
  
  # ✅ 8. Asignar grupo_ranking desde original (mantiene colores consistentes)
  cartograma$grupo_ranking <- con_dato$grupo_ranking
  
  # 📊 9. Visualización
  ggplot() +
    geom_sf(data = sin_dato, fill = "white", color = "gray60", linewidth = 0.2) +
    geom_sf(data = cartograma, aes(fill = grupo_ranking), color = "white", linewidth = 0.2) +
    scale_fill_manual(
      values = colores_importancia,
      name = paste("Importancia en idioma", tolower(idioma))
    ) +
    labs(title = paste("Cartograma - Importancia en idioma", idioma, "(", paste(continente, collapse = ", "), ")")) +
    theme_void() +
    theme(
      text = element_text(color = "#22211d"),
      plot.background = element_rect(fill = "#f5f5f4", color = NA),
      panel.background = element_rect(fill = "#f5f5f4", color = NA),
      legend.background = element_rect(fill = "#f5f5f4", color = NA),
      plot.title = element_text(size = 20, hjust = 0.5, color = "#4e4d47"),
      legend.position = c(0.2, 0.25)
    )
}





# Llamar la función 
cartograma_por_idioma(
  idioma = "English",
  continente = "Africa", #c("North America", "South America"),
  datos_ranking = ranking_final_ajustado,
  world_shapefile = world
)








# PONERLE LOS NOMBRES A LOS MAS ALTOS


cartograma_por_idiomatags <- function(idioma, continente, datos_ranking, world_shapefile) {
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(cartogram)
  library(countrycode)
  library(ggrepel)
  
  # 📌 Columnas dinámicas
  columna_rank <- paste0(idioma, "_rank")
  columna_inv <- paste0(idioma, "_rank_invertido")
  columna_reforzado <- paste0(idioma, "_rank_reforzado")
  
  # 🧮 Preparar ranking
  datos_ranking <- datos_ranking %>%
    mutate(
      ISO_A3 = countrycode(country, origin = "country.name", destination = "iso3c"),
      !!columna_inv := 1 / .data[[columna_rank]],
      !!columna_reforzado := (.data[[columna_inv]])^2
    ) %>%
    mutate(
      grupo_ranking = ntile(.data[[columna_inv]], 5),
      grupo_ranking = factor(grupo_ranking, levels = 1:5,
                             labels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))
    )
  
  # 🔗 Unión con shapefile
  world_data <- left_join(world_shapefile, datos_ranking, by = "ISO_A3")
  
  # 🌎 Filtrar continente
  continente_data <- world_data %>%
    filter(CONTINENT %in% continente)
  
  # 🔍 Separar con/sin datos
  con_dato <- continente_data %>%
    filter(!is.na(.data[[columna_reforzado]]), .data[[columna_reforzado]] > 0) %>%
    st_transform(3857)
  
  sin_dato <- continente_data %>%
    filter(is.na(.data[[columna_reforzado]])) %>%
    st_transform(3857)
  
  # 🧼 Validar geometrías
  con_dato <- st_make_valid(con_dato[!st_is_empty(con_dato), ])
  sin_dato <- st_make_valid(sin_dato[!st_is_empty(sin_dato), ])
  
  # 🗺️ Crear cartograma
  cartograma <- cartogram_cont(con_dato, columna_reforzado, itermax = 5)
  cartograma <- st_transform(cartograma, st_crs(world_shapefile))
  sin_dato <- st_transform(sin_dato, st_crs(world_shapefile))
  
  # 🎨 Paleta de colores
  colores_importancia <- c(
    "Muy bajo"  = "#ffffb2",
    "Bajo"      = "#fecc5c",
    "Medio"     = "#fd8d3c",
    "Alto"      = "#f03b20",
    "Muy alto"  = "#bd0026"
  )
  
  # ✅ Reasignar columnas necesarias para etiquetas
  cartograma$grupo_ranking <- con_dato$grupo_ranking
  cartograma[[columna_rank]] <- con_dato[[columna_rank]]
  cartograma$NAME <- con_dato$NAME
  
  # 🔎 Detectar el grupo de mayor nivel presente
  niveles <- levels(cartograma$grupo_ranking)
  grupos_presentes <- unique(cartograma$grupo_ranking)
  niveles_en_orden <- rev(niveles)  # de "Muy alto" a "Muy bajo"
  grupo_top <- niveles_en_orden[niveles_en_orden %in% grupos_presentes][1]
  
  # 🎯 Etiquetas solo para el grupo más alto presente
  etiquetas <- cartograma %>%
    filter(grupo_ranking == grupo_top) %>%
    st_make_valid() %>%
    filter(st_is_valid(geometry), !st_is_empty(geometry)) %>%
    mutate(
      label = paste0(NAME, "\n(Rank: ", round(.data[[columna_rank]], 0), ")"),
      geometry_centroid = suppressWarnings(st_centroid(geometry)),
      coords = st_coordinates(geometry_centroid)
    )
  
  # 🔔 Mensaje
  message("🧩 Etiquetando países con grupo: ", grupo_top)
  
  # 📊 Visualización
  ggplot() +
    geom_sf(data = sin_dato, fill = "white", color = "gray60", linewidth = 0.2) +
    geom_sf(data = cartograma, aes(fill = grupo_ranking), color = "white", linewidth = 0.2) +
    scale_fill_manual(
      values = colores_importancia,
      name = paste("Importancia en idioma", tolower(idioma))
    ) +
    geom_text_repel(
      data = etiquetas,
      aes(x = coords[,1], y = coords[,2], label = label),
      size = 3.5, fontface = "bold", color = "#22211d", bg.color = "white", bg.r = 0.1,
      max.overlaps = Inf, segment.color = "gray40"
    ) +
    labs(title = paste("Cartograma - Importancia en idioma", idioma, "(", paste(continente, collapse = ", "), ")")) +
    theme_void() +
    theme(
      text = element_text(color = "#22211d"),
      plot.background = element_rect(fill = "#f5f5f4", color = NA),
      panel.background = element_rect(fill = "#f5f5f4", color = NA),
      legend.background = element_rect(fill = "#f5f5f4", color = NA),
      plot.title = element_text(size = 20, hjust = 0.5, color = "#4e4d47"),
      legend.position = c(0.25, 0.50)
    )
}


library(sf)

world_valid <- st_make_valid(world)

cartograma_por_idiomatags(
  idioma = "Tamil",
  continente = "Oceania", #c("Europe","Asia"),
  datos_ranking = ranking_final_ajustado,
  world_shapefile = world
)





# casos especiales


ver_valores_cartograma_idioma <- function(idioma, continente, datos_ranking, world_shapefile) {
  library(dplyr)
  library(countrycode)
  
  # 📌 Columnas dinámicas
  columna_rank <- paste0(idioma, "_rank")
  columna_inv <- paste0(idioma, "_rank_invertido")
  columna_reforzado <- paste0(idioma, "_rank_reforzado")
  
  # 🧮 Preparar ranking
  datos_ranking <- datos_ranking %>%
    mutate(
      ISO_A3 = countrycode(country, origin = "country.name", destination = "iso3c"),
      !!columna_inv := 1 / .data[[columna_rank]],
      !!columna_reforzado := (.data[[columna_inv]])^2,
      grupo_ranking = ntile(.data[[columna_inv]], 5),
      grupo_ranking = factor(grupo_ranking,
                             levels = 1:5,
                             labels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))
    )
  
  # 🔗 Unir shapefile con ranking
  world_data <- left_join(world_shapefile, datos_ranking, by = "ISO_A3")
  
  # 🌎 Filtrar continente
  continente_data <- world_data %>%
    filter(CONTINENT %in% continente)
  
  # 📤 Extraer valores para tabla
  tabla_valores <- continente_data %>%
    st_drop_geometry() %>%
    select(
      Pais = NAME,
      Ranking = all_of(columna_rank),
      Invertido = all_of(columna_inv),
      Reforzado = all_of(columna_reforzado),
      Grupo = grupo_ranking
    ) %>%
    arrange(desc(Reforzado))
  
  return(tabla_valores)
}



ver_valores_cartograma_idioma(
  idioma = "Tamil",
  continente = c("Europe","Asia"),
  datos_ranking = ranking_final_ajustado,
  world_shapefile = world
)


