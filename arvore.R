library(ggtree)

tree_trat <- tree

tree_trat2 <- tree_trat %<+% dados_familias

tree_trat$node.label

tree_trat$tip.label <- tree$tip.label %>% stringr::str_replace("_", " ")

dados_familias <- data.frame(Espécie = tree_trat$tip.label) %>%
  dplyr::left_join(sinonimios,
                   by = "Espécie") %>%
  dplyr::select(1, 3) %>%
  dplyr::rename("label" = Espécie) %>%
  dplyr::distinct(label, Família)

tree_final <- tidytree::as.treedata(tree_trat) %>%
  dplyr::left_join(dados_familias, by = "label")

tree_final

ggtree::ggtree(tree_final, layout = "circular") +
  geom_tiplab(color = "black", size = 3, offset = 0.25, fontface = "bold") +
  geom_tippoint(aes(fill = Família), shape = 21, color = "black", size = 3.5) +
  scale_fill_viridis_d(option = "turbo") +
  xlim(-0.1, 8) +
  theme(legend.title = element_blank())

ggsave(filename = "arvore.png", height = 10, width = 12)
