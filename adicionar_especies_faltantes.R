# Pacotes ----

library(ape)

library(tidyverse)

library(phytools)

library(ggtree)

# Dados ----

## Árvore filogenética ----

### Importando ----

tree <- ape::read.tree("tree_cons.phy")

### Visualizando ----

tree

tree |> ape::plot.phylo(type = "fan",
                        show.tip.label = TRUE,
                        edge.color = "blue",
                        edge.width = 1.5,
                        tip.color = "black",
                        cex = 0.45,
                        label.offset = 0.001)

## Espécies totais ----

### Importando ----

sinonimios <- readxl::read_xlsx("matriz_sinonimios.xlsx")

### Visualizando ----

sinonimios |> as.data.frame()

sinonimios |> dplyr::glimpse()

# Tratando a filogenia ----

## Nome atualizado das espécies da filogenia ----

sin_trat <- sinonimios |>
  dplyr::filter(!`vertlife name` |> is.na()) |>
  dplyr::arrange(`vertlife name` = `vertlife name` |>
                   forcats::fct_relevel(tree$tip.label |>
                                          stringr::str_replace("_", " ")))

sin_trat |> as.data.frame()

sps_atualizado <- sin_trat |>
  dplyr::pull(species) |>
  stringr::str_replace(" ", "_")

sps_atualizado

## Atualizando o nome das espécies na filogenia ----

tree$tip.label <- sps_atualizado

tree$tip.label

tree |> ape::plot.phylo(type = "fan",
                        show.tip.label = TRUE,
                        edge.color = "blue",
                        edge.width = 1.5,
                        tip.color = "black",
                        cex = 0.45,
                        label.offset = 0.001)


setdiff(tree$tip.label, sin_trat |>
          dplyr::pull(`vertlife name`) |>
          stringr::str_replace(" ", "_"))















## Espécies que faltam ----

sps_faltam <- sinonimios |>
  dplyr::filter(`Está no VertLife` == "Não") |>
  dplyr:::pull(Sinonímio)

sps_faltam

novas_especies <- data.frame(especie = sps_faltam |> stringr::str_replace(" ", "_"),
                             genero = sps_faltam |> stringr::word(1))

novas_especies

## Adicionando espécies faltantes ----

tree$tip.label

tree$tip.label |> length()

adicionar_sps <- function(id){

  tree <<- phytools::add.species.to.genus(tree |>
                                            phytools::force.ultrametric(),
                                          novas_especies$especie[id])
}

id <- 1:nrow(novas_especies)

id

purrr::map(id, adicionar_sps)

tree$tip.label

tree$tip.label |> length()

## Corrigindo o nome das espécies ----

sinonimios <- sinonimios |>
  dplyr::mutate(Espécie = Espécie |>  stringr::str_replace(" ", "_"),
                Sinonímio = Sinonímio |> stringr::str_replace(" ", "_"),
                Sinonímio = Sinonímio |> factor(levels = tree$tip.label)) |>
  dplyr::arrange(Sinonímio)

sinonimios$Sinonímio

sinonimios$Espécie

tree$tip.label

corrigir_tax <- function(id){

  if(tree$tip.label[id] != sinonimios$Espécie[id]){

    tree$tip.label[id] <<- sinonimios$Espécie[id]

  }

}

id <- 1:length(tree$tip.label)

purrr::map(id, corrigir_tax)

## Visualizando ----

ggtree::ggtree(tree, layout = "circular") +
  ggtree::geom_tiplab(color = "black",
                      size = 2,
                      fontface = "bold.italic")  +
  ggtree::theme_tree()

# Exportando ----

tree |> ape::write.tree("tree_cep.phy")
