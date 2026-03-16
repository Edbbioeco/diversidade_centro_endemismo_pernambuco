# Pacotes ----

library(tidyverse)

library(sf)

library(writexl)

# Dados ----

## Identificação das espécies ----

### Importando ----

id_sps <- readr::read_csv("C:/Users/LENOVO/OneDrive/Documentos/curso_diversidade_taxonomica_simpzoo/ATLANTIC_AMPHIBIANS_species.csv")

### visualizando ----

id_sps

id_sps |> dplyr::glimpse()

### Tratando ----

id_sps_trat <- id_sps |>
  dplyr::select(id, valid_name) |>
  dplyr::filter(!valid_name |> is.na()) |>
  dplyr::rename("species" = valid_name)

id_sps_trat

## Ocorrências ----

### Importando ----

oc_vancine <- readr::read_csv("C:/Users/LENOVO/OneDrive/Documentos/curso_diversidade_taxonomica_simpzoo/ATLANTIC_AMPHIBIANS_sites.csv")

### Visualizando -----

oc_vancine

oc_vancine |> dplyr::glimpse()

### Tratando ----

oc_gbif_trat <- oc_vancine |>
  dplyr::select(species, stateProvince, decimalLatitude:decimalLongitude) |>
  dplyr::rename("Latitude" = decimalLatitude,
                "Longitude" = decimalLongitude) |>
  dplyr::mutate(Longitude = Longitude |>
                  stringr::str_replace("^(-?\\d{2})(\\d+)$", "\\1.\\2") |>
                  as.numeric(),
                Latitude = case_when(stringr::str_detect(
                  as.character(Latitude),
                  "^(-?[1-2])") ~ str_replace(as.character(Latitude),
                                              "^(-?\\d{2})(\\d+)$", "\\1.\\2"),
                  stringr::str_detect(
                    as.character(Latitude),
                    "^(-?[3-9])") ~ stringr::str_replace(as.character(Latitude),
                                                         "^(-?\\d{1})(\\d+)$", "\\1.\\2"),
                  TRUE ~ as.character(Latitude)) |>
                  as.numeric()) |>
  dplyr::filter(!is.na(species) &
                  !is.na(Latitude) &
                  !is.na(Longitude) &
                  !species |> stringr::str_detect(" sp| cf| af") &
                  species |> stringr::word(2) != "NA") |>
  dplyr::distinct(species, Longitude, Latitude, .keep_all = TRUE)

oc_gbif_trat |> dplyr::glimpse()

oc_gbif_trat

## Grade ----

### Importando ----

grade_cep <- sf::st_read("cep_grade.shp")

### Visualizando ----

grade_cep

grade_cep |>
  ggplot() +
  geom_sf(color = "black", fill = "green4")
