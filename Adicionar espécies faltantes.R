# Pacotes ----

library(ape)

library(tidyverse)

library(phytools)

library(ggtree)

# Dados ----

## Árvore filogenética ----

### Importando ----

tree <- ape::read.nexus("output3.nex")

### Calculando o consenso ----

tree_cons <- tree %>%
  consensus.edges() %>%
  ape::root(outgroup = "Pipa_carvalhoi", resolve.root = TRUE)

tree_cons %>% class()

tree_cons %>% is.rooted()

### Visualizando ----

tree_cons$tip.label

ggtree::ggtree(tree_cons, layout = "circular") +
  ggtree::geom_tiplab(color = "black", size = 2.5, offset = 0.25, fontface = "bold.italic")  +
  ggtree::theme_tree()

## Espécies totais ----

### Importando ----

sinonimios <- readxl::read_xlsx("lista_sinonimos.xlsx", sheet = 2) %>%
  dplyr::distinct(Espécie, .keep_all = TRUE)

### Visualizando ----

sinonimios %>% dplyr::glimpse()

sinonimios$Sinonímio

# Corrigindo ----

## Espécies que faltam ----

tree_cons2 <- tree_cons

tree_cons2$tip.label

tree_cons2$tip.label[41] <- "Ololygon_muriciensis"

tree_cons2$tip.label[46] <- "Ololygon_agilis"

tree_cons2$tip.label[29] <- "Vitreorana_baliomma"

sps_filo <- tree_cons2$tip.label

sps_filo

sps_faltam <- sinonimios %>%
  dplyr::filter(`Está no VertLife` == "Não") %>%
  dplyr:::pull(Sinonímio)

sps_faltam

novas_especies <- data.frame(especie = sps_faltam %>% stringr::str_replace(" ", "_"),
                             genero = sps_faltam %>% stringr::word(1))

novas_especies

## Adicionando ----

### Adicionando ----

tree_cons2$tip.label

tree_cons2$tip.label %>% length()

tree_cons3 <- phytools::add.species.to.genus(tree_cons2,
                                             novas_especies$especie[1])

tree_cons4 <- phytools::add.species.to.genus(tree_cons3 %>% force.ultrametric(),
                                             novas_especies$especie[2])

tree_cons5 <- phytools::add.species.to.genus(tree_cons4,
                                             novas_especies$especie[3])

tree_cons6 <- phytools::add.species.to.genus(tree_cons5,
                                             novas_especies$especie[4])

tree_cons7 <- phytools::add.species.to.genus(tree_cons6,
                                             novas_especies$especie[5])

tree_cons8 <- phytools::add.species.to.genus(tree_cons7,
                                             novas_especies$especie[6])

tree_cons9 <- tree_cons8

sub_pipa <- keep.tip(tree_cons9, "Pipa_carvalhoi")

sub <- bind.tree(tree_cons9, sub_pipa, where = "root")

ggtree::ggtree(tree_cons8, layout = "circular") +
  ggtree::geom_tiplab(color = "black",
                      size = 2.5,
                      offset = 0.25,
                      fontface = "bold.italic")  +
  ggtree::theme_tree()

### Testando ----

df <- data.frame(sps = tree_cons8$tip.label) %>%
  dplyr::left_join(data.frame(sps = sinonimios$Sinonímio %>% stringr::str_replace(" ", "_"),
                 sps2 = sinonimios$Espécie %>% stringr::str_replace(" ", "_")),
                   by = "sps") %>%
  dplyr::mutate(igual = dplyr::case_when(sps == sps2 ~ "sim",
                                         .default = "não")) %>%
  dplyr::mutate(sps2 = dplyr::case_when(sps2  %>% is.na() ~ "Vitreorana baliomma",
                                        .default = sps2))

df

data.frame(s1 = df$sps,
           s2 = c(tree_cons8$tip.label)) %>%
  dplyr::mutate(igual = dplyr::case_when(s1 == s2 ~ "sim",
                                         .default = "não"))

## Corrigindo ----

tree_cons9 <- tree_cons8

tree_cons9$tip.label <- df$sps2

tree_cons9 %>% is.rooted()

## Exportando ----

tree_cons9 %>%
  ape::write.tree("arvore.tre")


sps1 <- tree_cons8$tip.label

sps2 <- ecodados::filogenia_anuros %>% .$tip.label

setdiff(sps1, sps2)

sps <- data.frame(sps = sps1) %>%
  dplyr::left_join(data.frame(sps = sps2,
           tem = "sim")) %>%
  dplyr::filter(tem %>% is.na()) %>%
  dplyr::pull(sps)

sps
filo_anuros <- ecodados::filogenia_anuros
filo_anuros <- add.species.to.genus(filo_anuros, sps[8], where = "random")
filo_anuros$tip.label %>% sort()
filo_anuros %>% tidytree::as.treedata()
