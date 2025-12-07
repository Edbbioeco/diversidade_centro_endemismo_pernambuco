library(sf)
library(spatstat.geom)   # Para criação do objeto ppp
library(spatstat.core)   # Para análise do padrão pontual
library(spatstat.explore) # Para K function etc
library(ggplot2)

# Suponha que 'br' já está carregado e projetado (se não estiver, reprojete)
# br <- st_read("seu_shapefile.shp")
# br <- st_transform(br, 31984) # Exemplo de projeção UTM no Brasil

# 1. Gerar pontos aleatórios dentro do polígono 'br' - digamos 100 pontos
set.seed(42)
pontos_sf <- st_sample(br |>
                         sf::st_transform(crs = 31984), size = 100)

# 2. Extrair bounding box e converter para janela 'owin' do spatstat
# st_as_sfc() retorna a geometria, transformamos para o objeto 'owin' de spatstat
w <- as.owin(st_as_sfc(st_bbox(br |>
                                 sf::st_transform(crs = 31984))))
pontos <- dismo::gbif(genus = "Scinax", species = "x-signatus") |>
  dplyr::select(lon, lat) |>
  tidyr::drop_na() |>
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = 4674) |>
  sf::st_intersection(br)

# 3. Converter pontos para objeto ppp
# Para isso, extrair coordenadas dos pontos
coords <- st_coordinates(pontos |>
                           sf::st_transform(crs = 31984))
ppp_obj <- ppp(x = coords[,1], y = coords[,2], window = w)

# 4. Visualizar o padrão pontual
plot(ppp_obj, main = "Padrão pontual gerado com st_sample()")

# 5. Estimar densidade kernel
bw.scott(ppp_obj)

dens <- density(ppp_obj, kernel = "gaussian") # bw.ppl calcula banda ótima
plot(dens, main = "Estimativa de densidade kernel")
plot(ppp_obj, add=TRUE, pch=16, cex=0.5)

# 6. Calcular e plotar função K de Ripley para verificar agregação/regularidade
K <- Kest(ppp_obj)
plot(K, main = "Função K de Ripley")

# 7. Calcular função G (distância ao vizinho mais próximo)
G <- Gest(ppp_obj)
plot(G, main = "Função G")

# 8. Teste de CSR (Complete Spatial Randomness)
# Teste de quadrados quadrantes (quadrant test) - exemplo
quadrat.test(ppp_obj)

# Fim

dens |>
  terra::rast() |>
  terra::crop(br |>
                sf::st_transform(crs = 31984)) |>
  terra::mask(br |>
                sf::st_transform(crs = 31984)) |>
  plot()
