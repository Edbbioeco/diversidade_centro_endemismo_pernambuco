# Pacotes ----

library(readxl)

library(tidyverse)

library(parzer)

library(geobr)

library(sf)

library(geodata)

library(here)

library(terra)

library(tidyterra)

# Dados ----

## Coordenadas ----

### Importando ----

trajeto <- readxl::read_xlsx("Sapo mancudo xD.xlsx",
                             sheet = 2)

### Visualizando ----

trajeto

### Tratando ----

trajeto_trat <- trajeto |>
  dplyr::select(Coordenadas) |>
  tidyr::drop_na() |>
  tidyr::separate(into = c("Latitude", "Longitude"),
                  sep = " ",
                  col = Coordenadas) |>
  dplyr::mutate(Latitude = Latitude |> parzer::parse_lat(),
                Longitude = Longitude |> parzer::parse_lon()) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING")

trajeto_trat

ggplot() +
  geom_sf(data = trajeto_trat)

## Shapefiles ----

### Importando ----

br <- geobr::read_state(year = 2019)

### Visualizando ----

br

ggplot() +
  geom_sf(data = br, color = "black")

## Coordenadas ----

### Importando ----

coords <- br |> sf::st_sample(size = 500, method = "random")

### Visualizando ----

coords

ggplot() +
  geom_sf(data = coords, color = "black")

## Rasters ----

### Importando ----

br_trat <- br |>
  dplyr::summarise(geom = geom |> sf::st_union())

br_trat

alt <- geodata::elevation_30s(country = "BRA",
                              res = 0.5,
                              path = here::here())  |>
  terra::crop(br_trat) |>
  terra::mask(br_trat)

### Visualizando ----

alt

ggplot() +
  tidyterra::geom_spatraster(data = alt)

# Mapas ----

## Coordenadas ----

ggplot() +
  geom_sf(data = coords, size = 5) +
  coord_sf(label_graticule = "NSEW") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15, face = "bold"),
        panel.border = element_rect(color = "black", linewidth = 1))

ggsave(filename = "mapa_coords.png", height = 12, width = 10)

## shapefiles ----

ggplot() +
  geom_sf(data = br, color = "black", fill = "gold", linewidth = 0.5) +
  coord_sf(label_graticule = "NSEW") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15, face = "bold"),
        panel.border = element_rect(color = "black", linewidth = 1))

ggsave(filename = "mapa_shapefiles.png", height = 12, width = 10)

## rasters ----

ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  scale_fill_viridis_c(na.value = "transparent") +
  coord_sf(label_graticule = "NSEW") +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5,
                                barwidth = 35,
                                barheigth = 15,
                                frame.colour = "black",
                                frame.linewidth = 1,
                                ticks.colour = "black",
                                ticks.linewidth = 1)) +
  labs(fill = "Altitude") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 15, face = "bold"),
        legend.title = element_text(color = "black", size = 15, face = "bold"),
        panel.border = element_rect(color = "black", linewidth = 1),
        legend.position = "bottom")

ggsave(filename = "mapa_rasters.png", height = 12, width = 10)

## Mínimo polígono convexo ----

br_pais <- geobr::read_country(year = 2019)

br_convexo <- br_pais |>
  sf::st_convex_hull()

br_convexo

ggplot() +
  geom_sf(data = br_convexo, color = "black", fill = "cyan4", linewidth = 0.5) +
  geom_sf(data = br_pais,
          color = "black", fill = "gold", linewidth = 0.5) +
  coord_sf(label_graticule = "NSEW") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15, face = "bold"),
        panel.border = element_rect(color = "black", linewidth = 1))

ggsave(filename = "mapa_polígono_convexo.png", height = 12, width = 10)

br_pais_2 <- br_pais |>
  sf::st_coordinates() |>
  as.data.frame() |>
  sf::st_as_sf(coords = c("X", "Y"),
               crs = br_pais |> sf::st_crs()) |>
  sf::st_union()

br_convexo2 <- br_pais_2 |>
  sf::st_convex_hull()

ggplot() +
  geom_sf(data = br_convexo2,
          color = "black", fill = "cyan4", linewidth = 0.5) +
  geom_sf(data = br_pais_2,
          color = "black", fill = "gold", linewidth = 0.5) +
  coord_sf(label_graticule = "NSEW") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15, face = "bold"),
        panel.border = element_rect(color = "black", linewidth = 1))

ggsave(filename = "mapa_polígono_convexo2.png", height = 12, width = 10)

## Biomas ----

biomas <- geobr::read_biomes(year = 2019) |>
  sf::st_make_valid()

pontos_biomas <- biomas %>%
  sf::st_sample(size = 3)

ggplot() +
  geom_sf(data = biomas |>
            tidyr::drop_na(), color = "black", aes(fill = name_biome)) +
  geom_sf(data = pontos_biomas, size = 2.5) +
  coord_sf(label_graticule = "NSEW") +
  scale_fill_manual(values = c("darkgreen",
                               "gold",
                               "orange",
                               "green",
                               "cyan4",
                               "brown")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15, face = "bold"),
        panel.border = element_rect(color = "black", linewidth = 1),
        legend.position = "none")

ggsave(filename = "mapa_biomas.png", height = 10, width = 12)

## Trajeto ----

mapa_trajeto <- ggplot() +
  geom_sf(data = trajeto_trat,
          color = "black", linewidth = 0.5) +
  coord_sf(label_graticule = "NSEW") +
  scale_x_continuous(breaks = seq(-51.83147, -51.83144, 0.00001)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15, face = "bold"),
        panel.border = element_rect(color = "black", linewidth = 1))

imagem <- "Boana faber.png"

cowplot::ggdraw() +
  cowplot::draw_plot(mapa_trajeto) +
  cowplot::draw_image(imagem,  x = 0.3, y = 0.4, scale = 0.2)

ggsave(filename = "mapa_trajeto.png", height = 12, width = 10)
