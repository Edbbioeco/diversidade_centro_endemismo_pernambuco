# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(writexl)

# Dados ----

## Registros de ocorrência ----

### Importando -----

importar_registros <- function(registro, fonte){

  dado_registro <- readxl::read_xlsx(registro)

  assign(paste0("registro_", fonte),
         dado_registro,
         envir = globalenv())

}

registro <- list.files(pattern = "^registros_")

registro

fonte <- registro |>
  stringr::str_replace("_", " ") |>
  stringr::str_remove(".xlsx") |>
  stringr::word(2)

fonte

purrr::map2(registro, fonte, importar_registros)

### Visualizando ----

ls(pattern = "registro_") |>
  mget(envir = globalenv())

## Grade ----

### Importando ----

grade_cep <- sf::st_read("cep_grade.shp")

### Visualizando ----

grade_cep

grade_cep |>
  ggplot() +
  geom_sf(color = "black", fill = "green4")

# Dados unidos ----

## Unindo os dados de registro ----

registros <- ls(pattern = "registro_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

registros

## Checando as espécies ----

### Lista de espécies ----

registros$species |> unique()

# species == "Allobates olfersioides" ~ "Dryadobates alagoanus"
# species == "Rhinella margaritifera" ~ "Rhinella hoogmoedi"
# species %in% c("Rhinella jimi", "Rhinella marina", "Rhinella schneideri") ~ "Rhinella diptycha"
# species %in% c("Dendropsophus werneri", "Dendropsophus rubicundulus") ~ "Dendropsophus branneri"
# species == "Dendropsophus leucophyllatus" ~ "Dendropsophus elegans"
# species %in%  c("Leptodactylus labyrinthicus", "Leptodactylus pentadactylus", "Leptodactylus pentadactylus labyrinthicus")  ~ "Leptodactylus vastus"
# species == "Hypsiboas raniceps" ~ "Boana raniceps"
# species == "Phyllomedusa nordestina" ~ "Pithecopus gonzagai"
# species == "Hypsiboas albomarginatus" ~ "Boana albomarginata"
# species %in% c("Colostethus alagoanus", "Allobates alagoanus") ~ "Allobates olfersioides"
# species == "Hyla raniceps" ~ "Boana raniceps"
# species == "Hypsiboas semilineatus" ~ "Boana semilineata"
# species == "Hypsiboas atlanticus" ~ "Boana atlantica"
# species == "Hypsiboas exastis" ~ "Boana exastis"
# species == "Hypsiboas freicanecae" ~ "Boana freicanecae"
# species == "Rana paradoxa" ~ "Lithobates palmipes"
# species == "Leptodactylus ocellatus" ~ "Leptodactylus macrosternum"
# species %in% c("Bufo granulosus granulosus", "Rhinella mirandaribeiroi") ~ "Rhinella granulosa"
# species == "Ischnocnema ramagii" ~ "Pristimantis ramagii"
# species %in% c("Ololygon v-signata", "Scinax similis", "Osteopilus ocellatus") ~ "Scinax x-signatus"
# species %in% c("Phyllomedusa hypocondrialis", "Phyllomedusa hypochondrialis") ~ "Pithecopus gonzagai"
# species %in% c("Hypsiboas crepitans", "Boana pardalis") ~ "Boana crepitans"
# species == "Chiasmocleis alagoanus" ~ "Chiasmocleis alagoana"
# species == "Scinax skuki" ~ "Ololygon skuki"
# species == "Hypsiboas faber" ~ "Boana faber"
# species == "Scinax muriciensis" ~ "Ololygon muriciensis"
# species == "Scinax agilis" ~ "Ololygon agilis"
# species == "Elachistocleis ovalis" ~ "Elachistocleis cesarii"
# species == "Elachistocleis ovalis" ~ "Elachistocleis cesarii"
# species == "Dendropsophus decioiens" ~ "Dendropsophus decipiens"
# species == "Leptodactylus marmoratus"" ~ "Adenomera hylaedactyla"

### Corrigindo a taxonomia ----

registros <- registros |>
  dplyr::mutate(species = dplyr::case_match(
    species,
    "Rhinella margaritifera" ~ "Rhinella hoogmoedi",
    c("Rhinella jimi",
      "Rhinella marina",
      "Rhinella schneideri") ~ "Rhinella diptycha",
    c("Dendropsophus werneri",
      "Dendropsophus rubicundulus") ~ "Dendropsophus branneri",
    "Dendropsophus leucophyllatus" ~ "Dendropsophus elegans",
    c("Leptodactylus labyrinthicus",
      "Leptodactylus pentadactylus",
      "Leptodactylus pentadactylus labyrinthicus") ~ "Leptodactylus vastus",
    "Hypsiboas raniceps" ~ "Boana raniceps",
    "Phyllomedusa nordestina" ~ "Pithecopus gonzagai",
    "Hypsiboas albomarginatus" ~ "Boana albomarginata",
    c("Colostethus alagoanus",
      "Allobates alagoanus",
      "Allobates olfersioides") ~ "Dryadobates alagoanus",
    "Hyla raniceps" ~ "Boana raniceps",
    "Hypsiboas semilineatus" ~ "Boana semilineata",
    "Hypsiboas atlanticus" ~ "Boana atlantica",
    "Hypsiboas exastis" ~ "Boana exastis",
    c("Hypsiboas freicanecae",
      "Boana freicanecaee") ~ "Boana freicanecae",
    "Rana paradoxa" ~ "Lithobates palmipes",
    "Leptodactylus ocellatus" ~ "Leptodactylus macrosternum",
    c("Bufo granulosus granulosus",
      "Rhinella mirandaribeiroi") ~ "Rhinella granulosa",
    "Ischnocnema ramagii" ~ "Pristimantis ramagii",
    c("Ololygon v-signata",
      "Scinax similis",
      "Osteopilus ocellatus") ~ "Scinax x-signatus",
    c("Phyllomedusa hypocondrialis",
      "Phyllomedusa hypochondrialis") ~ "Pithecopus gonzagai",
    c("Hypsiboas crepitans",
      "Boana pardalis") ~ "Boana crepitans",
    "Chiasmocleis alagoanus" ~ "Chiasmocleis alagoana",
    "Scinax skuki" ~ "Ololygon skuki",
    "Hypsiboas faber" ~ "Boana faber",
    "Scinax muriciensis" ~ "Ololygon muriciensis",
    "Scinax agilis" ~ "Ololygon agilis",
    "Elachistocleis ovalis" ~ "Elachistocleis cesarii",
    "Dendropsophus decioiens" ~ "Dendropsophus decipiens",
    "Leptodactylus marmoratus" ~ "Adenomera hylaedactyla",
    "Agalychnis granulosa" ~ "Hylomantis granulosa",
    c("Adelophrnne nordestina",
      "Adelophrynne nordestina") ~ "Adelophrynne nordestina",
    "Vitreorana balionma" ~ "Vitreorana baliomma",
    .default = species
  )) |>
  dplyr::filter(!species %in% c("Breviceps gibbosus", "Vitreorana baliomma"))

registros

## Criando uma matriz de composição ----

### Matriz com todas as comunidades ----

matriz <- registros |>
  dplyr::rename("Assemblage" = FID) |>
  dplyr::group_by(Assemblage, species) |>
  dplyr::summarise(presence = max(presence, na.rm = TRUE),
                   .groups = "drop") |>
  tidyr::pivot_wider(names_from = species,
                     values_from = presence,
                     values_fill = 0) |>
  dplyr::left_join(registros |>
                     dplyr::rename("Assemblage" = FID) |>
                     dplyr::select(1, 4:6),
                   by = "Assemblage") |>
  dplyr::relocate(Latitude:Source,
                  .before = `Leptodactylus fuscus`) |>
  dplyr::distinct(Assemblage, .keep_all = TRUE)

matriz

ggplot() +
  geom_sf(data = grade_cep, color = "black", fill = "green4") +
  geom_point(data = matriz, aes(Longitude, Latitude))

### Removendo as comunidades com menos de 5 espécies ----

matriz <- matriz |> as.data.frame()

rownames(matriz) <- matriz$Assemblage

comunidades <- matriz |>
  dplyr::select(5:108) |>
  vegan::specnumber() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::filter(. >= 5) |>
  dplyr::pull(rowname)

comunidades

matriz_trat <- matriz |>
  dplyr::filter(Assemblage %in% comunidades) |>
  tibble::as_tibble()

matriz_trat

ggplot() +
  geom_sf(data = grade_cep, color = "black", fill = "green4") +
  geom_point(data = matriz_trat, aes(Longitude, Latitude))

### Removendo possíveies espécies sem registro ----

especies_retirar <- matriz_trat |>
  tidyr::pivot_longer(cols = 5:108,
                      names_to = "Espécie",
                      values_to = "Presença") |>
  dplyr::summarise(Abundancia = Presença |> sum(),
                   .by = Espécie) |>
  dplyr::filter(Abundancia == 0) |>
  dplyr::pull(Espécie)

especies_retirar

matriz_trat <- matriz_trat |>
  dplyr::select(-especies_retirar)

matriz_trat

# Exportando ----

## Registros ----

registros |>
  dplyr::rename("Assemblage" = FID) |>
  openxlsx::write.xlsx("registros.xlsx")

## Matriz ----

matriz |>
  openxlsx::write.xlsx("matriz.xlsx")

matriz_trat |>
  openxlsx::write.xlsx("matriz_trat.xlsx")
