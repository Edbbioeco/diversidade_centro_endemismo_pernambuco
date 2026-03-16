# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

library(ggview)

# Dados ----

## Estados ----

### Importando ----

estados <- geobr::read_state(year = 2019) |>
  dplyr::filter(abbrev_state %in% c("AL", "PE", "PB", "RN")) |>
  sf::st_union()

### Visualizando ----

estados

estados |>
  ggplot() +
  geom_sf(color = "black", fill = "gold")

## Mata Atlântica ----

### Importando ----

ma <- geobr::read_biomes() |>
  dplyr::filter(name_biome == "Mata Atlântica")

## Visualizando ----

ma

ma |>
  ggplot() +
  geom_sf(color = "black", fill = "green4")

## Centro de Endemismo Pernambuco ----

### Checando os crs ----

sf::st_crs(ma) == sf::st_crs(estados)

### Recortando ----

cep <- ma |> sf::st_intersection(estados)

### Visualizando ----

cep

cep |>
  ggplot() +
  geom_sf(color = "black", fill = "yellowgreen")

# Grade ----

## Valor dos graqus para 10 km ----

graus_10km <- (10*1)/111.3194

graus_10km

## Criando a grade ----

grade <- sf::st_make_grid(cep,
                 cellsize = graus_10km)

## Visualizando ----

grade

grade |>
  ggplot() +
  geom_sf(color = "black", fill = "green4") +
  geom_sf(data = cep, color = "red", fill = "transparent")

## Recorte ----

### Recortando ----

grade_cep <- grade |>
  sf::st_sf() |>
  sf::st_join(cep,
              join = st_intersects) |>
  tidyr::drop_na() |>
  dplyr::mutate(FID = paste0("c", dplyr::row_number()))

### Visualizando ----

grade_cep

ggplot() +
  geom_sf(data = grade_cep, color = "black", fill = "green4") +
  geom_sf(data = cep, color = "red", fill = "transparent")

## Exportando ----

grade_cep |>
  sf::st_write("cep_grade.shp",
               append = TRUE)
