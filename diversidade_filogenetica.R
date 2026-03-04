# Pacotes ----

library(ape)

library(readxl)

library(tidyverse)

library(picante)

library(phytools)

library(betapart)

# Dados ----

## Variáveis ambientais ----

### Importando ----

riqueza <- readxl::read_xlsx("valores_riqueza.xlsx")

### Visualizando ----

riqueza |> dplyr::glimpse()

riqueza

## Árvore filogenética ----

### Importando ----

tree <- ape::read.tree("arvore.tre")

### Visualizando ----

tree$tip.label

tree |> ape::plot.phylo(type = "fan",
                              show.tip.label = TRUE,
                              edge.color = "blue",
                              edge.width = 1.5,
                              tip.color = "black",
                              cex = 0.75,
                              label.offset = 0.001)


## Matriz de composição ----

### Importando ----

matriz <- readxl::read_xlsx("registros_finais.xlsx")

### Tratando ----

matriz_trat <- matriz |>
  dplyr::filter(Assemblage != 341) |>
  dplyr::select(6:105) |>
  as.data.frame()

rownames(matriz_trat) <- matriz$Assemblage[matriz$Assemblage != 341] |> as.character()

colnames(matriz_trat) <- matriz_trat |> names() |>
  stringr::str_replace(" ", "_") |>
  stringr::str_replace("Boana_creptans", "Boana_crepitans") |>
  stringr::str_replace("Adelophrynne_nordestina", "Adelophryne_nordestina") |>
  stringr::str_replace("Olygon_melanodactyla", "Ololygon_melanodactyla") |>
  stringr::str_replace("Pleurodema_diplolistris", "Pleurodema_diplolister")

### Visualizando ----

matriz_trat |> dplyr::glimpse()

matriz_trat

# Diversidade filogenética ----

## Alfa ----

### Diversidade filogenética ----

tree$tip.label

ape::is.rooted(tree)

matriz_trat_phy <- picante::match.phylo.comm(phy = tree,
                                             comm = matriz_trat)$comm

matriz_trat_phy

pd <- picante::pd(matriz_trat_phy,
                  tree) |>
  tibble::rownames_to_column() |>
  dplyr::rename("Assemblage" = rowname) |>
  dplyr::select(-3)

pd

### Mean Nearest Taxon Distance (MNTD) ----

mntd <- picante::mntd(matriz_trat_phy,
                      cophenetic(tree),
                      abundance.weighted = FALSE)

mntd

## Beta ----

comp_filo <- betapart::phylo.beta.pair(matriz_trat,
                                       tree,
                                       index.family = "jaccard")

comp_filo

# Modelos lineares ----

## Multicolinearidade ----

riqueza_cor <- riqueza |>
  dplyr::select(2:6) |>
  cor(method = "spearman")

riqueza_cor[upper.tri(riqueza_cor)] <- NA

riqueza_cor |>
  reshape2::melt() |>
  tidyr::drop_na() |>
  dplyr::mutate(igual = dplyr::case_when(Var1 == Var2 ~ "sim",
                                         .default = "Não"),
                value = value |> round(2)) |>
  dplyr::filter(igual == "Não") |>
  dplyr::select(-igual) |>
  ggplot(aes(Var1, Var2, fill = value, label = value)) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text() +
  coord_equal() +
  scale_fill_gradientn(colours = c(viridis::viridis(n = 10) |> rev(), viridis::viridis(n = 10)),
                       limits = c(-1, 1),
                       breaks = seq(-1, 1, 0.2)) +
  labs(x = NULL,
       y = NULL) +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5,
                                barwidth = 25,
                                barheight = 1.5,
                                frame.colour = "black",
                                ticks.colour = "black",
                                ticks.linewidth = 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "bottom")

## Unindo os dados ----

pd_trat <- pd |>
  dplyr::mutate(MNTD =mntd) |>
  dplyr::left_join(riqueza,
                   by = "Assemblage") |>
  tidyr::drop_na()

pd_trat

## Modelo para PD ----

### Criando o modelo ----

pd_trat2 <- pd_trat

colnames(pd_trat2) <- pd_trat |> names |>
  stringr::str_replace_all(" ", "_") |>
  stringr::str_remove_all("%_|'")

pd_trat2

gls_pd_altprec <- nlme::gls(PD ~ `Altitude` +
                              `Precipitação`,
                            data = pd_trat2,
                            correlation = ape::corBrownian(1, tree))

gls_pd_paisagem <- nlme::gls(PD ~ `de_corpos_dágua` +
                               `de_vegetação_nativa` +
                               `Diversidade_da_paisagem`,
                             data = pd_trat2,
                             correlation = ape::corBrownian(1, tree))

### Avaliando o modelo ----

gls_pd_altprec |> summary()

gls_pd_paisagem |> summary()

### Gráfico ----

pd_trat_gg <- pd_trat |>
  tidyr::pivot_longer(cols = c(Altitude:`Diversidade da paisagem`),
                      names_to = "Preditor",
                      values_to = "Valor Preditor")

pd_trat_gg$Preditor |> unique()

pd_trat_gg |>
  dplyr::mutate(Preditor = Preditor |>
                  forcats::fct_relevel(pd_trat_gg$Preditor |> unique())) |>
  ggplot(aes(`Valor Preditor`, PD, color = Preditor, fill = Preditor)) +
  geom_point(size = 5, shape = 21, color = "black", stroke = 1) +
  geom_smooth(data = pd_trat_gg |>
                dplyr::mutate(Preditor = Preditor |>
                                forcats::fct_relevel(pd_trat_gg$Preditor |>
                                                       unique())) |>
                dplyr::filter(!Preditor %in% c("% de corpos d'água", "Diversidade da paisagem")),
              method = "lm",
              se = FALSE,
              linewidth = 2) +
  facet_wrap(~ Preditor, scales = "free_x") +
  scale_fill_manual(values = c("gold", "cyan3", "skyblue4", "green2", "violet")) +
  scale_color_manual(values = c("gold4", "cyan4", "green4")) +
  theme_bw() +
  theme(text = element_text(face = "bold"),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1),
        legend.position = "none")

ggsave(filename = "modelo_pd.png", height = 10, width = 12)

### Sumário ----

sumario_pd <- gls_pd_altprec |>
  summary() |>
  .$tTable |>
  tibble::as_tibble() |>
  dplyr::mutate(Modelo = "Variável ambiental") |>
  dplyr::bind_rows(gls_pd_paisagem |>
                     summary() |>
                     .$tTable |>
                     tibble::as_tibble() |>
                     dplyr::mutate(Modelo = "Métrica de Paisagem")) |>
  dplyr::mutate(Preditor = c("Intercépto", pd_trat[c(4, 6:7)] |> names(), "Intercépto", pd_trat[c(8:9)] |> names())) |>
  dplyr::relocate(c(Modelo, Preditor), .before = Value)

sumario_pd

### Tabela das estátísticas ----

#### Criando ----

pd_tabela <- sumario_pd |>
  dplyr::mutate(Value = Value |> round(3),
                Std.Error = Std.Error |> round(3),
                `t-value` = `t-value` |> round(3),
                `p-value` = dplyr::case_when(`p-value` < 0.01 ~ "< 0.01",
                                                                       .default =`p-value` |>
                                               round(3) |>
                                               as.character()),
                `Graus de Liberdade` = 38) |>
  dplyr::rename("t" = `t-value`,
                "p" = `p-value`) |>
  dplyr::relocate(`Graus de Liberdade`, .before = t) |>
  tidyr::unite(col = "Coeficiente estimado ± Erro Padrão",
               Value:Std.Error,
               sep = " ± ") |>
  flextable::flextable() |>
  flextable::width(width = 1.5) |>
  flextable::align(align = "center", part = "all") |>
  flextable::bold(part = "header")

pd_tabela

#### Exportando ----

pd_tabela |>
  flextable::save_as_docx(path = "tabela_pd.docx")

## Modelo para MNTD ----

### Criando o modelo ----

gls_mntd_altprec <- nlme::gls(MNTD ~ Altitude +
                              Precipitação,
                            data = pd_trat2,
                            correlation = ape::corBrownian(1, tree))

gls_mntd_paisagem <- nlme::gls(MNTD ~ `de_corpos_dágua` +
                               `de_vegetação_nativa` +
                               `Diversidade_da_paisagem`,
                             data = pd_trat2,
                             correlation = ape::corBrownian(1, tree))

### Avaliando o modelo ----

gls_mntd_altprec |> summary()

gls_mntd_paisagem |> summary()

### Gráfico ----

pd_trat_gg

pd_trat_gg |>
  dplyr::mutate(Preditor = Preditor |>
                  forcats::fct_relevel(pd_trat_gg$Preditor |>
                                         unique())) |>
  ggplot(aes(`Valor Preditor`, MNTD, color = Preditor, fill = Preditor)) +
  geom_point(size = 5, shape = 21, color = "black", stroke = 1) +
  facet_wrap(~ Preditor, scales = "free_x") +
  geom_smooth(data = pd_trat_gg |>
                dplyr::mutate(Preditor = Preditor |>
                                forcats::fct_relevel(pd_trat_gg$Preditor |>
                                                       unique())) |>
                dplyr::filter(!Preditor %in% c("% de corpos d'água")),
              method = "lm",
              se = FALSE,
              linewidth = 2) +
  facet_wrap(~ Preditor, scales = "free_x") +
  scale_fill_manual(values = c("gold", "cyan3", "skyblue4", "green2", "violet")) +
  scale_color_manual(values = c("gold4", "cyan4", "green4", "purple4")) +
  theme_bw() +
  theme(text = element_text(face = "bold"),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1),
        legend.position = "none")

ggsave(filename = "modelo_mntd.png", height = 10, width = 12)

## Para Phylosor ----

### ALtitude e Precipitação ----

filo_dbrda_altprec <- vegan::capscale(comp_filo$phylo.beta.jac ~ Altitude +
                  Precipitação,
                data = pd_trat)

filo_dbrda_altprec

filo_dbrda_altprec |> vegan::anova.cca(by = "term")

vegan::RsquareAdj(filo_dbrda_altprec)

variaveis_filodbrdaaltprec <- filo_dbrda_altprec$CCA$biplot |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::mutate(rowname = rowname |> stringr::str_remove_all("`"))

variaveis_filodbrdaaltprec

coords_filodbrdaaltprec <- filo_dbrda_altprec$Ybar |>
  as.data.frame() |>
  dplyr::mutate(Assemblage = matriz$Assemblage[matriz$Assemblage != 341])

coords_filodbrdaaltprec

filo_dbrda_altprec |> plot()

ggplot() +
  geom_label(data = coords_filodbrdaaltprec, aes(Dim1, Dim2,
                                                 label = Assemblage),
             size = 5, color = "black", fill = "gold", label.size = 1,
             fontface = "bold") +
  geom_text(data = variaveis_filodbrdaaltprec, aes(CAP1, CAP2,
                                                   label = rowname,
                                                   fontface = "bold"),
            color = "black", size = 5)+
  geom_segment(data = variaveis_filodbrdaaltprec, aes(x = 0, y = 0,
                                                      xend = CAP1,
                                                      yend = CAP2),
               color = "black", linewidth = 1) +
  scale_x_continuous(limits = c(-0.725, 0.86)) +
  labs(x = "CAP 1 (47,97%)",
       y = "CAP2 (16,20%)") +
  theme_bw() +
  theme(text = element_text(face = "bold"),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "none",
        panel.border = element_rect(color = "black", linewidth = 1))

ggsave(filename = "modelo_composicao_alt_prec_filo.png",
       height = 10, width = 12)

### Paisagem ----

filo_dbrda_paisagem <- vegan::capscale(comp_filo$phylo.beta.jac ~ `% de corpos d'água` +
                                        `% de vegetação nativa` +
                                        `Diversidade da paisagem`,
                                      data = pd_trat)

filo_dbrda_paisagem

filo_dbrda_paisagem |> vegan::anova.cca(by = "term")

vegan::RsquareAdj(filo_dbrda_paisagem)

variaveis_filodbrdapaisagem <- filo_dbrda_paisagem$CCA$biplot |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::mutate(rowname = rowname |> stringr::str_remove_all("`"))

variaveis_filodbrdapaisagem

coords_filodbrdapaisagem <- filo_dbrda_paisagem$Ybar |>
  as.data.frame() |>
  dplyr::mutate(Assemblage = matriz$Assemblage[matriz$Assemblage != 341])

coords_filodbrdapaisagem

ggplot() +
  geom_label(data = coords_filodbrdapaisagem, aes(Dim1, Dim2,
                                                  label = Assemblage),
             size = 5, color = "black", fill = "gold", label.size = 1,
             fontface = "bold") +
  geom_text(data = variaveis_filodbrdapaisagem, aes(CAP1, CAP2,
                                                    label = rowname,
                                                    fontface = "bold"),
            color = "black", size = 5)+
  geom_segment(data = variaveis_filodbrdapaisagem, aes(x = 0, y = 0,
                                                       xend = CAP1,
                                                       yend = CAP2),
               color = "black",
               linewidth = 1) +
  scale_x_continuous(limits = c(-0.725, 0.9)) +
  labs(x = "CAP 1 (48,44%)",
       y = "CAP2 (21,07%)") +
  theme_bw() +
  theme(text = element_text(face = "bold"),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "none",
        panel.border = element_rect(color = "black", linewidth = 1))

ggsave(filename = "modelo_composicao_paisagem_filo.png",
       height = 10,
       width = 12)

