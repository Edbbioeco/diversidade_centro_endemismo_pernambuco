# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(ggvegan)

library(betapart)

# Dados ----

## Registros ----

### Importando ----

registros <- readxl::read_xlsx("matriz_registros.xlsx")

### Visualizando ----

registros

registros |> dplyr::glimpse()

## Outros ----

### Importando ----

outros <- readxl::read_xlsx("registros_repteis.xlsx")

### Visualizando ----

outros

outros |> dplyr::glimpse()

### Tratando ----

outros <- outros |>
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      values_to = "Presença",
                      names_to = "Município") |>
  dplyr::relocate(Espécie, .before = Presença)

outros

# Unindo os dados ----

## Unindo ----

especies_presença <- dplyr::bind_rows(registros, outros)

especies_presença


## Matriz de composição ----

matriz <- especies_presença |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Presença,
                     values_fn = max,
                     values_fill = 0)

matriz

# Diversidade ----

## Curva de acumulação de espécies -----

curva <- matriz |>
  dplyr::select(dplyr::where(is.numeric)) |>
  vegan::poolaccum(permutations = 100) |>
  summary(display = c("S", "boot"))

curva

## Dataframe ----

curva_df <- curva$S |>
  as.data.frame() |>
  dplyr::rename("Richness" = S) |>
  dplyr::mutate(tipo = "Observed") |>
  dplyr::bind_rows(curva$boot |>
                     as.data.frame() |>
                     dplyr::rename("Richness" = Bootstrap) |>
                     dplyr::mutate(tipo = "Estimated"))

curva_df

## Gráfico ----

curva_df |>
  ggplot(aes(N, Richness, color = tipo, fill = tipo)) +
  geom_ribbon(aes(x = N,
                  ymin = Richness - Std.Dev,
                  ymax = Richness + Std.Dev),
              alpha = 0.3,
              color = "transparent") +
  geom_line(linewidth = 1) +
  geom_point(size = 5, shape = 21, color = "black") +
  scale_fill_manual(values = c("gold", "cyan2")) +
  scale_color_manual(values = c("gold4", "cyan4")) +
  theme_bw()

## nMDS ----

### Calculando a matriz de distância ----

distancia <- matriz |>
  dplyr::select(dplyr::where(is.numeric)) |>
  vegan::vegdist(method = "jaccard")

### nMDS ----

nmds <- distancia |>
  vegan::metaMDS(distance = "jaccard")

nmds

### Gráfico ----

nmds |>
  vegan::scores() |>
  tibble::as_tibble() |>
  dplyr::mutate(locais = matriz$Município) |>
  ggplot(aes(NMDS1, NMDS2, label = locais)) +
  geom_label(size = 5, fill = "gold") +
  scale_x_continuous(expand = c(0.075, 0.075)) +
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        panel.border = element_rect(color = "black"))

## Distância entre as comunidades ----

### Matriz ----

