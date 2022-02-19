
library(dplyr)
library(tidyr)
library(highcharter)
library(rvest)
library(janitor)
library(writexl)
library(webshot)
library(stringr)

##### Scraping e exportação do csv #####

# Definindo o site e fazendo o scraping

site <- "https://www.sport-histoire.fr/pt/Geografia/Paises_por_area.php"

paises <- read_html(site)

paises <- paises %>%
  html_element('body > div > div:nth-child(2) > div.col.d-block > div.row.main > div > table') %>%
  html_table()

# limpando os nomes das colunas

paises <- paises %>%
  janitor::clean_names()

# filtrando as propagandas que vieram no scraping

paises <- paises %>%
  filter(pais != "(adsbygoogle = window.adsbygoogle || []).push({});")

# tratando as strings para converter para numeric

paises$area_km2 <- str_replace_all(str_replace(paises$area_km2,pattern = ",",replacement = "."), pattern = " ", replacement = "")

paises <- paises %>%
  mutate(area_km2 = as.numeric(area_km2))

# gerando o csv

write.csv(paises,"paises_area.csv")

##### Gerando os gráficos #####

### 1o gráfico: 10 maiores países do mundo por área

paises_10 <- paises %>%
  arrange(desc(area_km2)) 

# pegando os 10 maiores paises e passando a unidade para 1,000 de km²

paises_10 <- paises_10[1:10,] %>%
  mutate(area_km2 = round(area_km2/1000))

# gráfico para os 10 maiores países do mundo e salvando em png

hchart(paises_10,type="column",hcaes(x="pais",y="area_km2"),dataLabels = list(enabled = T),color="darkblue") %>%
  hc_title(
    text = "<b>Dez Maiores Países do Mundo em 1,000 Km²</b>",
    margin = 20,
    align = "center",
    style = list(color = "dark blue", useHTML = TRUE)
  )

### 2o gráfico: maiores países por continente ###

# pegando os maiores países por continente

continentes <- paises %>% 
  group_by(continente) %>%
  slice_max(area_km2) %>%
  mutate(area_km2 = round(area_km2/1000))

# plotando o gráfico

hchart(continentes,type="column",hcaes(x="pais",y="area_km2",group = "continente"),dataLabels = list(enabled = T)) %>%
  hc_colors(colors=c("green","grey","#cc0000","blue","yellow")) %>%
  hc_title(
    text = "<b>Maiores Países por Continente em 1,000 Km²</b>",
    margin = 20,
    align = "center",
    style = list(color = "dark blue", useHTML = TRUE)
  )
  