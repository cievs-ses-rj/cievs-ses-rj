# Carregar bibliotecas
library(readxl)
library(dplyr)
library(scatterpie)
library(readr)
library(spdep)
library(stringr)
library(maptools)
library(rgdal)
library(tmap)
library(XML)
library(cartogram)
library(mapplots)
library(readxl)
sarampo <- read_excel("C:/Users/eduardo.peixoto/Desktop/sarampo.xlsx", 
                      sheet = "consolidado")

# Ler os dados espaciais
shape2 <- st_read("https://raw.githubusercontent.com/tbrugz/geodata-br/master/geojson/geojs-33-mun.json")


# Gerar dados de amostra / atribuir taxa no banco shape
shape2$taxa <- sarampo$not_neg
shape2$mun<-sarampo$MUNICÍPIO


shape2$taxa_cat<-NA
shape2$taxa_cat[shape2$taxa>=0.8]<-"Maior ou igual 80 %"
shape2$taxa_cat[shape2$taxa<0.8]<-"13%-79%"
shape2$taxa_cat[shape2$taxa==0]<-"Não informado"

shape2$taxa_cat<-as.factor(shape2$taxa_cat)
levels(shape2$taxa_cat)<-c( "Maior ou igual 80 %" ,"13%-79%"  ,"Não informado" )

# Mapa de notificação negativa

ggplot(shape2) +
  aes(fill = taxa_cat) +
  geom_sf(size = 1.2) +
  scale_fill_manual(
    values = c(`Maior ou igual 80 %` = "#80BD85",
               `13%-79%` = "#B283C7",
               `Não informado` = "#BCB9BB")
  ) +
  labs(fill = "Notificação Negativa") +
  theme_minimal()


# Carregue a biblioteca tmap
library(tmap)


shape2$prospectivo<-"Não informado"
table(shape2$mun)
shape2$prospectivo[shape2$mun=="Rio de Janeiro"]<-"2022 e 2023"
shape2$prospectivo[shape2$mun=="Tanguá"]<-"2023"
shape2$prospectivo[shape2$mun=="Nova Iguaçu"]<-"2023"

# Crie o objeto de mapa com a shape2
map <- tm_shape(shape2) + tm_borders()

# Crie a rosa dos ventos (Wind Rose) na parte superior direita
wind_rose <- tm_compass(position = c("right","top"))

# Defina o fator com diferentes níveis (substitua '`Busca Ativa Prospectiva`' pelo seu fator real)
shape2$`Busca Ativa Prospectiva` <- factor(shape2$prospectivo)

# Defina a ordem dos níveis do fator
levels(shape2$`Busca Ativa Prospectiva`) <- c( "2022 e 2023","2023", "Não informado")

# Escolha uma paleta de cores vermelhas para o fator
red_palette <- c("2023" = "pink", "2022 e 2023" = "red", "Não informado" = "white")

# Adicione o fator ao mapa com a escala de vermelho
map <- map + tm_fill(col = "Busca Ativa Prospectiva", palette = red_palette)

# Adicione a rosa dos ventos na parte superior direita
map <- map + wind_rose

# Adicione a barra de escala na parte inferior direita
map <- map + tm_scale_bar(position = c("center", "bottom"))

# Renderize o mapa
map



#########
# Carregar bibliotecas
library(readxl)
library(dplyr)
library(scatterpie)
library(readr)
library(spdep)
library(stringr)
library(maptools)
library(rgdal)
library(tmap)
library(XML)
library(cartogram)
library(mapplots)
library(readxl)
sarampo <- read_excel("C:/Users/eduardo.peixoto/Desktop/sarampo.xlsx", 
                      sheet = "consolidado")

# Ler os dados espaciais
shape2 <- st_read("https://raw.githubusercontent.com/tbrugz/geodata-br/master/geojson/geojs-33-mun.json")


# Gerar dados de amostra / atribuir taxa no banco shape
shape2$taxa <- sarampo$not_neg2_nao_certa
shape2$mun<-sarampo$MUNICÍPIO


shape2$taxa_cat<-NA
shape2$taxa_cat[shape2$taxa>=0.8]<-"Maior ou igual 80 %"
shape2$taxa_cat[shape2$taxa<0.8]<-"13%-79%"
shape2$taxa_cat[shape2$taxa==0]<-"Não informado"

shape2$taxa_cat<-as.factor(shape2$taxa_cat)
levels(shape2$taxa_cat)<-c( "Maior ou igual 80 %" ,"13%-79%"  ,"Não informado" )



#######

# Crie o objeto de mapa com a shape2
map <- tm_shape(shape2) + tm_borders()

# Crie a rosa dos ventos (Wind Rose) na parte superior direita
wind_rose <- tm_compass(position = c("right","top"))

# Defina o fator com diferentes níveis (substitua '`Busca Ativa Prospectiva`' pelo seu fator real)
shape2$`Notificação Negativa` <- factor(shape2$taxa_cat)

# Defina a ordem dos níveis do fator

# Escolha uma paleta de cores vermelhas para o fator
red_palette <- c("Maior ou igual 80 %" = "#80BD85",  "13%-79%" = "#B283C7",
                 "Não informado" = "#BCB9BB")

# Adicione o fator ao mapa com a escala de vermelho
map <- map + tm_fill(col = "Notificação Negativa", palette = red_palette)

# Adicione a rosa dos ventos na parte superior direita
map <- map + wind_rose

# Adicione a barra de escala na parte inferior direita
map <- map + tm_scale_bar(position = c("center", "bottom"))

# Renderize o mapa
map


###################################



#########
# Carregar bibliotecas
library(readxl)
library(dplyr)
library(scatterpie)
library(readr)
library(spdep)
library(stringr)
library(maptools)
library(rgdal)
library(tmap)
library(XML)
library(cartogram)
library(mapplots)
library(readxl)
sarampo <- read_excel("C:/Users/eduardo.peixoto/Desktop/sarampo.xlsx", 
                      sheet = "consolidado")

# Ler os dados espaciais
shape2 <- st_read("https://raw.githubusercontent.com/tbrugz/geodata-br/master/geojson/geojs-33-mun.json")


# Gerar dados de amostra / atribuir taxa no banco shape
shape2$taxa <- sarampo$detec
shape2$mun<-sarampo$MUNICÍPIO


shape2$taxa_cat[shape2$taxa>=2]<-">=2%"
shape2$taxa_cat[shape2$taxa<2]<-"<2"

shape2$taxa_cat<-factor(shape2$taxa_cat)

levels(shape2$taxa_cat)<-c(">=2%" ,"<2" )



#######

# Crie o objeto de mapa com a shape2
map <- tm_shape(shape2) + tm_borders()

# Crie a rosa dos ventos (Wind Rose) na parte superior direita
wind_rose <- tm_compass(position = c("right","top"))

# Defina o fator com diferentes níveis (substitua '`Busca Ativa Prospectiva`' pelo seu fator real)
shape2$`Taxa de detecção` <- factor(shape2$taxa_cat)

# Defina a ordem dos níveis do fator

# Escolha uma paleta de cores vermelhas para o fator
red_palette <- c(">=2%" = "aquamarine", "<2" = "pink",
                 "Não informado" = "#BCB9BB")

# Adicione o fator ao mapa com a escala de vermelho
map <- map + tm_fill(col = "Taxa de detecção", palette = red_palette)

# Adicione a rosa dos ventos na parte superior direita
map <- map + wind_rose

# Adicione a barra de escala na parte inferior direita
map <- map + tm_scale_bar(position = c("center", "bottom"))

# Renderize o mapa
map
