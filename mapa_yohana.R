# Pacotes ----

library(tidyverse)

library(geosphere)

library(sf)

# Dados ----

## Vetores ----

### Distâncias em metros ----

distancias <- c(97,	90,	104,	28,	54,	40,	45,	56,	60,	50)/100

distancias

### Graus de Azimute (0-360°) ----

angulos <- c(30,140, 80,50, 110, 100, 30, 50, 20,50)

angulos

### Altura em métros ----

alturas <- c(26, 36, 80, 96, 69, 62, 68, 97, 20, 0)/100

alturas

## Coordenadas ----

lat_ini <- -25.0728306

lon_ini <- -51.8309999

lat_final <- -25.0727972

lon_final <- -51.8309944

## Dataframe das coordenadas inicais do trajeto ----

trajeto <- tibble::tibble(lon = lon_ini, lat = lat_ini, altura = 0)

trajeto

# Análise dde trajeto ----

## Calculando as novas coordenadas ----

for (i in 1:length(distancias)) {

  destino <- geosphere::destPoint(c(trajeto$lon[i],
                                    trajeto$lat[i]),
                                  angulos[i],
                                  distancias[i])

  trajeto <- dplyr::bind_rows(trajeto,
                                   tibble::tibble(lat = destino[2],
                                                  lon = destino[1],
                                                  altura = alturas[i]))

}

trajeto

## Deslocamento e tortuosidade ----

### Criando o shapefiles ----

trajeto_total <- trajeto |>
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = 4674) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING")

trajeto_minimo <- trajeto[c(1, nrow(trajeto)), ] |>
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = 4674) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING")

ggplot() +
  geom_sf(data = trajeto_total, linewidth = 1) +
  geom_sf(data = trajeto_minimo, linewidth = 1, color = "red")


### Calculando ----

tortuosidade <- sf::st_length(trajeto_total) / sf::st_length(trajeto_minimo)

tortuosidade

## Deslocamento vertical ----

desloc_vert <- sum(diff(alturas)[diff(alturas) > 0], na.rm = TRUE)

desloc_vert

# Mapa ----

pontos <- trajeto |>
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = 4674)

ggplot() +
  geom_sf(data = trajeto_total, linewidth = 1) +
  geom_sf(data = pontos,
          size = 5,
          shape = 21,
          color = "black",
          aes(fill = altura)) +
  scale_fill_viridis_c(limits = c(0, 1)) +
  coord_sf(label_graticule = "NSWE") +
  labs(fill = "Heigth (m)",
       caption = paste0("Deslocation length: ",
                        sf::st_length(trajeto_total) |> round(2),
                        "m | Vertical deslocation: ",
                        desloc_vert,
                        "m")) +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5,
                                barwidth = 20,
                                frame.colour = "black",
                                ticks.colour = "black")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 12),
        legend.position = "top",
        plot.caption = element_text(color = "black", size = 12))

ggsave(filename = "mapa_yohana.png",
       height = 10,
       width = 12)
