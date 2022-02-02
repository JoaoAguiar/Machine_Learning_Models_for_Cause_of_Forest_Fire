setwd("C:/Users/João Aguiar/Desktop/Data Mining")

library(na.tools)              # Remover NA's
library(dplyr)                 # 
library(ggplot2)               # Graficos
library(lubridate)             # Datas
library(biogeo)                # dms2dd()
library(ggmap)
library(stringr)               # str_split_fixed()
library(maps)                  # Criar Mapa Grafico
library(mapproj)               # Criar Mapa Grafico
library(mapdata)               #
library(scales)                #
library(rgdal)                 #
library(tidyverse)             # 
library(caret)                 # 
library(naivebayes)            # Naive Bayes
library(randomForest)          # Random Forest
library(adabag)                # Bagging
library(rpart.plot)            # Grafico Arvore
library(data.table)            # fwrite()

data(worldMapEnv)

source("getTemperatureNOAA.R") # Obter a temp-max, temp-min e precipitação

########## Data Importation, Cleanup and Pre-Processing ##########
##### Remove Attributes And NA's #####
fires_train <- read.csv("fires_train.csv", header=TRUE)
fires_test <- read.csv("fires_test.csv", header=TRUE)

# Remover colunas que não interessam para o nosso objetivo
fires_train <- subset(fires_train, select=-c(alert_source, extinction_hour, firstInterv_hour, parish))
fires_test <- subset(fires_test, select=-c(alert_source, extinction_hour, firstInterv_hour, parish))

# Verificar se tem NA's
fires_train %>% any_na()
# Numero de NA's
fires_train %>% n_na()
# Remove linhas com NA's
fires_train <- fires_train %>% na.rm()
##### Character Cleanup #####
# Caso region=="TrÃ¡s-os-Montes" seja True, altera para "Tras-dos-Montes"
fires_train <- mutate(fires_train, region=(case_when(region=="TrÃ¡s-os-Montes" ~ "Tras-dos-Montes", TRUE ~ region)))
fires_test <- mutate(fires_test, region=(case_when(region=="TrÃ¡s-os-Montes" ~ "Tras-dos-Montes", TRUE ~ region)))

fires_train <- mutate(fires_train, district=(case_when(district=="Ã???vora" ~ "Évora", TRUE ~ district)))
fires_train <- mutate(fires_train, district=(case_when(district=="BraganÃ§a" ~ "Bragança", TRUE ~ district)))
fires_train <- mutate(fires_train, district=(case_when(district=="SantarÃ©m" ~ "Santarém", TRUE ~ district)))
fires_train <- mutate(fires_train, district=(case_when(district=="SetÃºbal" ~ "Setubal", TRUE ~ district)))
fires_train <- mutate(fires_train, district=(case_when(district=="Viana Do Castelo" ~ "Viana do Castelo", TRUE ~ district)))
fires_test <- mutate(fires_test, district=(case_when(district=="Ã???vora" ~ "Évora", TRUE ~ district)))
fires_test <- mutate(fires_test, district=(case_when(district=="BraganÃ§a" ~ "Bragança", TRUE ~ district)))
fires_test <- mutate(fires_test, district=(case_when(district=="SantarÃ©m" ~ "Santarém", TRUE ~ district)))
fires_test <- mutate(fires_test, district=(case_when(district=="SetÃºbal" ~ "Setubal", TRUE ~ district)))
fires_test <- mutate(fires_test, district=(case_when(district=="Viana Do Castelo" ~ "Viana do Castelo", TRUE ~ district)))

fires_train <- mutate(fires_train, municipality=(case_when(municipality=="BraganÃ§a" ~ "Bragança", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Freixo de Espada Ã Cinta" ~ "Freixo de Espada á Cinta", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Ã???vora" ~ "Évora", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="SertÃ£" ~ "Sertá", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="FundÃ£o" ~ "Fundão", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="CovilhÃ£" ~ "Covilhá", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="PedrÃ³gÃ£o Grande" ~ "Pedrogão Grande", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="SÃ£o BrÃ¡s de Alportel" ~ "São Brás de Alportel", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="OlhÃ£o" ~ "Olhão", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="MelgaÃ§o" ~ "Melgaço", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="ValenÃ§a" ~ "Valença", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Vila Nova de FamalicÃ£o" ~ "Vila Nova de Famalição", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="GuimarÃ£es" ~ "Guimarães", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Oliveira de AzemÃ©is" ~ "Oliveira de Azeméis", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Santiago do CacÃ©m" ~ "Santiago do Cacém", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="CinfÃ£es" ~ "Cinfães", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="GolegÃ£" ~ "Golegá", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="SantarÃ©m" ~ "Santarém", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="ValpaÃ§os" ~ "Valpaços", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="PÃ³voa de Lanhoso" ~ "Pôvoa de Lanhoso", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Santa Comba DÃ£o" ~ "Santa Comba Dão", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="NazarÃ©" ~ "Nazaré", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="BaiÃ£o" ~ "Baião", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Ã\"bidos" ~ "Óbidos", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Ãgueda" ~ "Ilhavo", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Ãlhavo" ~ "Bragança", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="AlcÃ¡cer do Sal" ~ "Alcaçer do Sal", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Vila ViÃ§osa" ~ "Vila Viçosa", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Vila Velha De RÃ³dÃ£o" ~ "Vila Velha de Rodão", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Vila Velha de RÃ³dÃ£o" ~ "Vila Velha de Rodão", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Vila Real de Santo AntÃ³nio" ~ "Vila Real de Santo António", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Vila Nova De Foz CÃ´a" ~ "Vila Nova de Foz Côa", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Viana Do Castelo" ~ "Viana do Castelo", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="TROFA" ~ "Trofa", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Terras De Bouro" ~ "Terras de Bouro", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="TabuaÃ§o" ~ "Tabuaço", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="SetÃºbal" ~ "Setubal", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Santa Marta De PenaguiÃ£o" ~ "Santa Marta de Penaguião", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Santa Marta de PenaguiÃ£o" ~ "Santa Marta de Penaguião", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="SÃ£o Pedro do Sul" ~ "São Pedro do Sul", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="SÃ£o JoÃ£o da Madeira" ~ "São João da Madeira", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="SÃ¡tÃ£o" ~ "Sátão", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Ribeira De Pena" ~ "Ribeira de Pena", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Porto De MÃ³s" ~ "Porto de Mós", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Porto de MÃ³s" ~ "Porto de Mós", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="PortimÃ£o" ~ "Portimão", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Peso da RÃ©gua" ~ "Peso da Régua", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Penalva Do Castelo" ~ "Penalva do Castelo", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="PaÃ§os De Ferreira" ~ "Paços de Ferreira", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="PaÃ§os de Ferreira" ~ "Paços de Ferreira", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="PÃ³voa de Varzim" ~ "Pôvoa de Varzim", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="OurÃ©m" ~ "Ourém", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Freixo De Espada Ã Cinta" ~ "Freixo de Espada a Cinta", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Oliveira De AzemÃ©is" ~ "Oliveira De Azeméis", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="ODIVELAS" ~ "Odivelas", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="MurÃ§a" ~ "Murça", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="MesÃ£o Frio" ~ "Mesão Frio", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="MesÃ£o frio" ~ "Mesão Frio", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="MarvÃ£o" ~ "Marvão", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="MaÃ§Ã£o" ~ "Mação", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="LousÃ£" ~ "Lousã", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="LourinhÃ£" ~ "Lourinhã", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="LoulÃ©" ~ "Loulé", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="GrÃ¢ndola" ~ "Grandola", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="GaviÃ£o" ~ "Gavião", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Ferreira do ZÃªzere" ~ "Ferreira do Zézere", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Castanheira de PÃªra" ~ "Castanheira de Pêra", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="MurÃ§a" ~ "Murça", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Carrazeda de AnsiÃ£es" ~ "Carrazeda de Ansiães", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="AnsiÃ£o" ~ "Ansião", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="AlvaiÃ¡zere" ~ "Alvaiázere", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Alter do ChÃ£o" ~ "Alter do Chão", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="AlpiarÃ§a" ~ "Alpiarca", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="AlmodÃ´var" ~ "Almodôvar", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="AlijÃ³" ~ "Alijó", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="AlfÃ¢ndega da FÃ©" ~ "Alfândega da Fé", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="AlcobaÃ§a" ~ "Alcobaça", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="AlcÃ¡cer Do Sal" ~ "Alcaçer do Sal", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="AlcÃ¡cer do Sal" ~ "Alcaçer do Sal", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="MÃ©rtola" ~ "Mértola", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="TÃ¡bua" ~ "Tábua", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="MortÃ¡gua" ~ "Mortágua", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="ConstÃ¢ncia" ~ "Constância", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="MonÃ§Ã£o" ~ "Monção", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="MÃªda" ~ "Mêda", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="GÃ³is" ~ "Góis", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Arcos De Valdevez" ~ "Arcos de Valdevez", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Cabeceiras De Basto" ~ "Cabeceiras de Basto", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Carregal Do Sal" ~ "Carregal do Sal", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Celorico Da Beira" ~ "Celorico da Beira", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Celorico De Basto" ~ "Celorico de Basto", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Figueira Da Foz" ~ "Figueira da Foz", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="FigueirÃ³ dos Vinhos" ~ "Figueiró dos Vinhos", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Macedo De Cavaleiros" ~ "Macedo de Cavaleiros", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Marco De Canaveses" ~ "Marco de Canaveses", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Macedo De Cavaleiros" ~ "Macedo de Cavaleiros", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="MourÃ£o" ~ "Mourão", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Oliveira Do Hospital" ~ "Oliveira do Hospital", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Ponte Da Barca" ~ "Ponte da Barca", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Ponte De Lima" ~ "Ponte de Lima", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Ponte De Sor" ~ "Ponte de Sor", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="ProenÃ§a-a-Nova" ~ "Proença-a-Nova", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="SÃ£o JoÃ£o da Pesqueira" ~ "São João da Pesqueira", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Salvaterra De Magos" ~ "Salvaterra de Magos", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Vila Nova De FamalicÃ£o" ~ "Vila Nova de Famalicão", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Vila Pouca De Aguiar" ~ "Vila Pouca de Aguiar", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="VIZELA" ~ "Vizela", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Paredes De Coura" ~ "Paredes de Coura", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Montemor-o-velho" ~ "Montemor-o-velho", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Paredes De Coura" ~ "Paredes de Coura", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Montemor-o-velho" ~ "Montemor-o-Velho", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Caldas Da Rainha" ~ "Caldas da Rainha", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Castelo De Vide" ~ "Castelo de Vide", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Idanha-a-nova" ~ "Idanha-a-Nova", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Murtosa" ~ "Murtosa", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Sobral De Monte AgraÃ§o" ~ "Sobral de Monte Agraço", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Torre De Moncorvo" ~ "Torre de Moncorvo", TRUE ~ municipality)))
fires_train <- mutate(fires_train, municipality=(case_when(municipality=="Oliveira De Azeméis" ~ "Oliveira de Azeméis", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Oliveira De Azeméis" ~ "Oliveira de Azeméis", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Caldas Da Rainha" ~ "Caldas da Rainha", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Castelo De Vide" ~ "Castelo de Vide", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Idanha-a-nova" ~ "Idanha-a-Nova", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Murtosa" ~ "Murtosa", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Sobral De Monte AgraÃ§o" ~ "Sobral de Monte Agraço", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Torre De Moncorvo" ~ "Torre de Moncorvo", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Paredes De Coura" ~ "Paredes de Coura", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Montemor-o-velho" ~ "Montemor-o-Velho", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="BraganÃ§a" ~ "Bragança", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Freixo de Espada Ã Cinta" ~ "Freixo de Espada á Cinta", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Ã???vora" ~ "Évora", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="SertÃ£" ~ "Sertá", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="FundÃ£o" ~ "Fundão", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="CovilhÃ£" ~ "Covilhá", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="PedrÃ³gÃ£o Grande" ~ "Pedrogão Grande", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="SÃ£o BrÃ¡s de Alportel" ~ "São Brás de Alportel", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="OlhÃ£o" ~ "Olhão", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="MelgaÃ§o" ~ "Melgaço", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="ValenÃ§a" ~ "Valença", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Vila Nova de FamalicÃ£o" ~ "Vila Nova de Famalição", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="GuimarÃ£es" ~ "Guimarães", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Oliveira de AzemÃ©is" ~ "Oliveira de Azeméis", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Santiago do CacÃ©m" ~ "Santiago do Cacém", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="CinfÃ£es" ~ "Cinfães", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="GolegÃ£" ~ "Golegá", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="SantarÃ©m" ~ "Santarém", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="ValpaÃ§os" ~ "Valpaços", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="PÃ³voa de Lanhoso" ~ "Pôvoa de Lanhoso", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Santa Comba DÃ£o" ~ "Santa Comba Dão", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="NazarÃ©" ~ "Nazaré", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="BaiÃ£o" ~ "Baião", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Ã\"bidos" ~ "Óbidos", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Ãgueda" ~ "Ilhavo", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Ãlhavo" ~ "Bragança", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="AlcÃ¡cer do Sal" ~ "Alcaçer do Sal", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Vila ViÃ§osa" ~ "Vila Viçosa", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Vila Velha De RÃ³dÃ£o" ~ "Vila Velha de Rodão", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Vila Velha de RÃ³dÃ£o" ~ "Vila Velha de Rodão", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Vila Real de Santo AntÃ³nio" ~ "Vila Real de Santo António", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Vila Nova De Foz CÃ´a" ~ "Vila Nova de Foz Côa", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Viana Do Castelo" ~ "Viana do Castelo", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="TROFA" ~ "Trofa", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Terras De Bouro" ~ "Terras de Bouro", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="TabuaÃ§o" ~ "Tabuaço", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="SetÃºbal" ~ "Setubal", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Santa Marta De PenaguiÃ£o" ~ "Santa Marta de Penaguião", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Santa Marta de PenaguiÃ£o" ~ "Santa Marta de Penaguião", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="SÃ£o Pedro do Sul" ~ "São Pedro do Sul", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="SÃ£o JoÃ£o da Madeira" ~ "São João da Madeira", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="SÃ¡tÃ£o" ~ "Sátão", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Ribeira De Pena" ~ "Ribeira de Pena", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Porto De MÃ³s" ~ "Porto de Mós", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Porto de MÃ³s" ~ "Porto de Mós", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="PortimÃ£o" ~ "Portimão", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Peso da RÃ©gua" ~ "Peso da Régua", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Penalva Do Castelo" ~ "Penalva do Castelo", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="PaÃ§os De Ferreira" ~ "Paços de Ferreira", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="PaÃ§os de Ferreira" ~ "Paços de Ferreira", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="PÃ³voa de Varzim" ~ "Pôvoa de Varzim", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="OurÃ©m" ~ "Ourém", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Freixo De Espada Ã Cinta" ~ "Freixo de Espada a Cinta", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Oliveira De AzemÃ©is" ~ "Oliveira De Azeméis", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="ODIVELAS" ~ "Odivelas", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="MurÃ§a" ~ "Murça", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="MesÃ£o Frio" ~ "Mesão Frio", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="MesÃ£o frio" ~ "Mesão Frio", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="MarvÃ£o" ~ "Marvão", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="MaÃ§Ã£o" ~ "Mação", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="LousÃ£" ~ "Lousã", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="LourinhÃ£" ~ "Lourinhã", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="LoulÃ©" ~ "Loulé", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="GrÃ¢ndola" ~ "Grandola", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="GaviÃ£o" ~ "Gavião", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Ferreira do ZÃªzere" ~ "Ferreira do Zézere", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Castanheira de PÃªra" ~ "Castanheira de Pêra", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="MurÃ§a" ~ "Murça", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Carrazeda de AnsiÃ£es" ~ "Carrazeda de Ansiães", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="AnsiÃ£o" ~ "Ansião", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="AlvaiÃ¡zere" ~ "Alvaiázere", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Alter do ChÃ£o" ~ "Alter do Chão", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="AlpiarÃ§a" ~ "Alpiarca", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="AlmodÃ´var" ~ "Almodôvar", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="AlijÃ³" ~ "Alijó", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="AlfÃ¢ndega da FÃ©" ~ "Alfândega da Fé", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="AlcobaÃ§a" ~ "Alcobaça", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="AlcÃ¡cer Do Sal" ~ "Alcaçer do Sal", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="AlcÃ¡cer do Sal" ~ "Alcaçer do Sal", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="MÃ©rtola" ~ "Mértola", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="TÃ¡bua" ~ "Tábua", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="MortÃ¡gua" ~ "Mortágua", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="ConstÃ¢ncia" ~ "Constância", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="MonÃ§Ã£o" ~ "Monção", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="MÃªda" ~ "Mêda", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="GÃ³is" ~ "Góis", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Arcos De Valdevez" ~ "Arcos de Valdevez", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Cabeceiras De Basto" ~ "Cabeceiras de Basto", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Carregal Do Sal" ~ "Carregal do Sal", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Celorico Da Beira" ~ "Celorico da Beira", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Celorico De Basto" ~ "Celorico de Basto", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Figueira Da Foz" ~ "Figueira da Foz", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="FigueirÃ³ dos Vinhos" ~ "Figueiró dos Vinhos", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Macedo De Cavaleiros" ~ "Macedo de Cavaleiros", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Marco De Canaveses" ~ "Marco de Canaveses", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Macedo De Cavaleiros" ~ "Macedo de Cavaleiros", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="MourÃ£o" ~ "Mourão", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Oliveira Do Hospital" ~ "Oliveira do Hospital", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Ponte Da Barca" ~ "Ponte da Barca", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Ponte De Lima" ~ "Ponte de Lima", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Ponte De Sor" ~ "Ponte de Sor", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="ProenÃ§a-a-Nova" ~ "Proença-a-Nova", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="SÃ£o JoÃ£o da Pesqueira" ~ "São João da Pesqueira", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Salvaterra De Magos" ~ "Salvaterra de Magos", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Vila Nova De FamalicÃ£o" ~ "Vila Nova de Famalicão", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Vila Pouca De Aguiar" ~ "Vila Pouca de Aguiar", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="VIZELA" ~ "Vizela", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Paredes De Coura" ~ "Paredes de Coura", TRUE ~ municipality)))
fires_test <- mutate(fires_test, municipality=(case_when(municipality=="Montemor-o-velho" ~ "Montemor-o-velho", TRUE ~ municipality)))

fires_train <- mutate(fires_train, region=(case_when(district=="Aveiro" ~ "Beira Litoral", district=="Beja" ~ "Alentejo", district=="Braga" ~ "Entre Douro e Minho", district=="Bragança" ~ "Tras-dos-Montes", district=="Castelo Branco" ~ "Beira Interior", district=="Coimbra" ~ "Beira Litoral", district=="Évora" ~ "Alentejo", district=="Guarda" ~ "Beira Interior", district=="Leiria" ~ "Beira Litoral", district=="Lisboa" ~ "Ribatejo e Oeste", district=="Porto" ~ "Entre Douro e Minho", district=="Santarém" ~ "Ribatejo e Oeste", district=="Viana do Castelo" ~ "Entre Douro e Minho", district=="Vila Real" ~ "Tras-dos-Montes", district=="Viseu" ~ "Beira Litoral", district=="Setubal" ~ "Ribatejo e Oeste", district=="Portalegre" ~ "Alentejo", district=="Faro" ~ "Algarve", TRUE ~ region)))
fires_test <- mutate(fires_test, region=(case_when(district=="Aveiro" ~ "Beira Litoral", district=="Beja" ~ "Alentejo", district=="Braga" ~ "Entre Douro e Minho", district=="Bragança" ~ "Tras-dos-Montes", district=="Castelo Branco" ~ "Beira Interior", district=="Coimbra" ~ "Beira Litoral", district=="Évora" ~ "Alentejo", district=="Guarda" ~ "Beira Interior", district=="Leiria" ~ "Beira Litoral", district=="Lisboa" ~ "Ribatejo e Oeste", district=="Porto" ~ "Entre Douro e Minho", district=="Santarém" ~ "Ribatejo e Oeste", district=="Viana do Castelo" ~ "Entre Douro e Minho", district=="Vila Real" ~ "Tras-dos-Montes", district=="Viseu" ~ "Beira Litoral", district=="Setubal" ~ "Ribatejo e Oeste", district=="Portalegre" ~ "Alentejo", district=="Faro" ~ "Algarve", TRUE ~ region)))
##### Lat and Lon Cleanup #####
# Caso encontre "''", alterar para ""
fires_train <- mutate(fires_train, lat=(case_when(grepl("''", fires_train$lat, fixed=TRUE) ~ sub("''", "", lat, fixed=TRUE), TRUE ~ lat)))
fires_train <- mutate(fires_train, lon=(case_when(grepl("''", fires_train$lon, fixed=TRUE) ~ sub("''", "", lon, fixed=TRUE), TRUE ~ lon)))
fires_train <- mutate(fires_train, lat=(case_when(grepl("'", fires_train$lat, fixed=TRUE) ~ sub("'", ":", lat, fixed=TRUE), TRUE ~ lat)))
fires_train <- mutate(fires_train, lon=(case_when(grepl("'", fires_train$lon, fixed=TRUE) ~ sub("'", ":", lon, fixed=TRUE), TRUE ~ lon)))
fires_train <- mutate(fires_train, lat=(case_when(grepl("Âº", fires_train$lat, fixed=TRUE) ~ sub("Âº", ":", lat, fixed=TRUE), TRUE ~ lat)))
fires_train <- mutate(fires_train, lon=(case_when(grepl("Âº", fires_train$lon, fixed=TRUE) ~ sub("Âº", ":", lon, fixed=TRUE), TRUE ~ lon)))
fires_train <- mutate(fires_train, lat=(case_when(grepl(".", fires_train$lat, fixed=TRUE) ~ sub(".", ":", lat, fixed=TRUE), TRUE ~ lat)))
fires_train <- mutate(fires_train, lon=(case_when(grepl(".", fires_train$lon, fixed=TRUE) ~ sub(".", ":", lon, fixed=TRUE), TRUE ~ lon)))
fires_train <- mutate(fires_train, lat=(case_when(startsWith(fires_train$lat, '00:') ~ sub("00:", "", lat, fixed=TRUE), TRUE ~ lat)))
fires_train <- mutate(fires_train, lon=(case_when(startsWith(fires_train$lon, '00:') ~ sub("00:", "", lon, fixed=TRUE), TRUE ~ lon)))
fires_train <- mutate(fires_train, lat=(case_when(startsWith(fires_train$lat, '-') ~ sub("-", "", lat, fixed=TRUE), TRUE ~ lat)))
fires_train <- mutate(fires_train, lon=(case_when(startsWith(fires_train$lon, '-') ~ sub("-", "", lon, fixed=TRUE), TRUE ~ lon)))
fires_train <- mutate(fires_train, lat=(case_when(grepl("E-02", fires_train$lat, fixed=TRUE) ~ sub("E-02", "", lat, fixed=TRUE), TRUE ~ lat)))
fires_train <- mutate(fires_train, lon=(case_when(grepl("E-02", fires_train$lon, fixed=TRUE) ~ sub("E-02", "", lon, fixed=TRUE), TRUE ~ lon)))
fires_test <- mutate(fires_test, lat=(case_when(grepl("''", fires_test$lat, fixed=TRUE) ~ sub("''", "", lat, fixed=TRUE), TRUE ~ lat)))
fires_test <- mutate(fires_test, lon=(case_when(grepl("''", fires_test$lon, fixed=TRUE) ~ sub("''", "", lon, fixed=TRUE), TRUE ~ lon)))
fires_test <- mutate(fires_test, lat=(case_when(grepl("'", fires_test$lat, fixed=TRUE) ~ sub("'", ":", lat, fixed=TRUE), TRUE ~ lat)))
fires_test <- mutate(fires_test, lon=(case_when(grepl("'", fires_test$lon, fixed=TRUE) ~ sub("'", ":", lon, fixed=TRUE), TRUE ~ lon)))
fires_test <- mutate(fires_test, lat=(case_when(grepl("Âº", fires_test$lat, fixed=TRUE) ~ sub("Âº", ":", lat, fixed=TRUE), TRUE ~ lat)))
fires_test <- mutate(fires_test, lon=(case_when(grepl("Âº", fires_test$lon, fixed=TRUE) ~ sub("Âº", ":", lon, fixed=TRUE), TRUE ~ lon)))
fires_test <- mutate(fires_test, lat=(case_when(grepl(".", fires_test$lat, fixed=TRUE) ~ sub(".", ":", lat, fixed=TRUE), TRUE ~ lat)))
fires_test <- mutate(fires_test, lon=(case_when(grepl(".", fires_test$lon, fixed=TRUE) ~ sub(".", ":", lon, fixed=TRUE), TRUE ~ lon)))
fires_test <- mutate(fires_test, lat=(case_when(startsWith(fires_test$lat, '00:') ~ sub("00:", "", lat, fixed=TRUE), TRUE ~ lat)))
fires_test <- mutate(fires_test, lon=(case_when(startsWith(fires_test$lon, '00:') ~ sub("00:", "", lon, fixed=TRUE), TRUE ~ lon)))
fires_test <- mutate(fires_test, lat=(case_when(startsWith(fires_test$lat, '-') ~ sub("-", "", lat, fixed=TRUE), TRUE ~ lat)))
fires_test <- mutate(fires_test, lon=(case_when(startsWith(fires_test$lon, '-') ~ sub("-", "", lon, fixed=TRUE), TRUE ~ lon)))
fires_test <- mutate(fires_test, lat=(case_when(grepl("E-02", fires_test$lat, fixed=TRUE) ~ sub("E-02", "", lat, fixed=TRUE), TRUE ~ lat)))
fires_test <- mutate(fires_test, lon=(case_when(grepl("E-02", fires_test$lon, fixed=TRUE) ~ sub("E-02", "", lon, fixed=TRUE), TRUE ~ lon)))

for(i in 1:length(fires_train$region)) {
  lat <- fires_train$lat[i]
  lon <- fires_train$lon[i]
  
  # Formato não Data
  if(grepl(" ", lat) == FALSE) {
    lat_split <- str_split_fixed(lat, ":", 3)
    lon_split <- str_split_fixed(lon, ":", 3)
    
    fires_train$lat[i] <- dms2dd(as.numeric(lat_split[1]), as.numeric(lat_split[2]), as.numeric(lat_split[3]), "N")
    fires_train$lon[i] <- dms2dd(as.numeric(lon_split[1]), as.numeric(lon_split[2]), as.numeric(lon_split[3]), "W")
    
    if(fires_train$lat[i]<36.5 || fires_train$lon[i]<(-6.5)) {
      fires_train$lon[i] <- NA
      fires_train$lat[i] <- NA
    }
  }
  # Formato Data
  else {
    lat <- str_split(lat, " ")
    lat <- lat[[1]][2]
    
    lat_split <- str_split(lat, ":")
    lon_split <- str_split(lon, ":")
    
    fires_train$lat[i] <- dms2dd(as.numeric(lat_split[[1]][1]), as.numeric(lat_split[[1]][2]), as.numeric(lat_split[[1]][3]), "N")
    fires_train$lon[i] <- dms2dd(as.numeric(lon_split[[1]][1]), as.numeric(lon_split[[1]][2]), as.numeric(lon_split[[1]][]), "W")
    
    if(fires_train$lat[i]<36.5 || fires_train$lon[i]<(-6.5)) {
      fires_train$lon[i] <- NA
      fires_train$lat[i] <- NA
    }
  }
}
for(j in 1:length(fires_test$region)) {
  lat <- fires_test$lat[j]
  lon <- fires_test$lon[j]
  
  # Formato não Data
  if(grepl(" ", lat) == FALSE) {
    lat_split <- str_split_fixed(lat, ":", 3)
    lon_split <- str_split_fixed(lon, ":", 3)
    
    fires_test$lat[j] <- dms2dd(as.numeric(lat_split[1]), as.numeric(lat_split[2]), as.numeric(lat_split[3]), "N")
    fires_test$lon[j] <- dms2dd(as.numeric(lon_split[1]), as.numeric(lon_split[2]), as.numeric(lon_split[3]), "W")
    
    if(fires_test$lat[j]<(36.5) || fires_test$lon[j]<(-6.5)) {
      fires_test$lon[j] <- NA
      fires_test$lat[j] <- NA
    }
  }
  # Formato Data
  else {
    lat <- str_split(lat, " ")
    lat <- lat[[1]][2]
    
    lat_split <- str_split_fixed(lat, ":", 3)
    lon_split <- str_split_fixed(lon, ":", 3)
    
    fires_test$lat[j] <- dms2dd(as.numeric(lat_split[[1]][1]), as.numeric(lat_split[[1]][2]), as.numeric(lat_split[[1]][3]), "N")
    fires_test$lon[j] <- dms2dd(as.numeric(lon_split[[1]][1]), as.numeric(lon_split[[1]][2]), as.numeric(lon_split[[1]][]), "W")
    
    if(fires_test$lat[j]<(36.5) || fires_test$lon[i]<(-6.5)) {
      fires_test$lon[j] <- NA
      fires_test$lat[j] <- NA
    }
  } 
}

fires_train <- fires_train %>% na.rm()
##### Date Cleanup #####
# Converte caracteres para Data
fires_train <- fires_train %>% mutate(firstInterv_date=as.Date(fires_train$firstInterv_date))
fires_train <- fires_train %>% mutate(alert_date=as.Date(fires_train$alert_date))
fires_train <- fires_train %>% mutate(extinction_date=as.Date(fires_train$extinction_date))
fires_test <- fires_test %>% mutate(firstInterv_date=as.Date(fires_test$firstInterv_date))
fires_test <- fires_test %>% mutate(alert_date=as.Date(fires_test$alert_date))
fires_test <- fires_test %>% mutate(extinction_date=as.Date(fires_test$extinction_date))

# Cria colunas dos dias e dos meses
fires_train <- fires_train %>% mutate(alert_day=day(fires_train$alert_date))
fires_train <- fires_train %>% mutate(alert_month=month(fires_train$alert_date))
fires_test <- fires_test %>% mutate(alert_day=day(fires_test$alert_date))
fires_test <- fires_test %>% mutate(alert_month=month(fires_test$alert_date))

# Transforma os numeros dos meses em nome
fires_train$alert_month <- month.name[fires_train$alert_month]
fires_test$alert_month <- month.name[fires_test$alert_month]
##### Add New Data #####
# Cria as novas colunas 
fires_train$max_temperature <- ""
fires_train$precipitation <- ""
fires_train$min_temperature <- ""
fires_train$avg_temperature <- ""
fires_test$min_temperature <- ""
fires_test$max_temperature <- ""
fires_test$avg_temperature <- ""
fires_test$precipitation <- ""

for(i in 1:length(fires_train$region)) {
  nearby_stations = get_nearby_stations(fires_train$district[i], fires_train$lat[i], fires_train$lon[i])
  
  fires_train$max_temperature[i] <- maximum_getter(nearby_stations, fires_train$alert_date[i])
  fires_train$min_temperature[i] <- minimum_getter(nearby_stations, fires_train$alert_date[i])
  fires_train$avg_temperature[i] <- average_getter(nearby_stations, fires_train$alert_date[i])
  fires_train$precipitation[i] <- precipitation_getter(nearby_stations, fires_train$alert_date[i])
}
for(i in 1:length(fires_test$region)) {
  if(fires_test$lat[i] %>% any_na() == FALSE & fires_test$lon[i] %>% any_na() == FALSE) {
    nearby_stations = get_nearby_stations(fires_test$district[i], fires_test$lat[i], fires_test$lon[i])
    
    fires_test$max_temperature[i] <- maximum_getter(nearby_stations, fires_test$alert_date[i])
    fires_test$min_temperature[i] <- minimum_getter(nearby_stations, fires_test$alert_date[i])
    fires_test$avg_temperature[i] <- average_getter(nearby_stations, fires_test$alert_date[i])
    fires_test$precipitation[i] <- precipitation_getter(nearby_stations, fires_test$alert_date[i])
  }
}

for(i in 1:length(fires_train$region)) {
  fires_train$max_temperature[i] <- as.numeric(fires_train$max_temperature[i])/10
  fires_train$min_temperature[i] <- as.numeric(fires_train$min_temperature[i])/10
  fires_train$avg_temperature[i] <- as.numeric(fires_train$avg_temperature[i])/10
  fires_train$precipitation[i] <- as.numeric(fires_train$precipitation[i])/10
}
for(i in 1:length(fires_test$region)) {
  fires_test$max_temperature[i] <- as.numeric(fires_test$max_temperature[i])/10
  fires_test$min_temperature[i] <- as.numeric(fires_test$min_temperature[i])/10
  fires_test$avg_temperature[i] <- as.numeric(fires_test$avg_temperature[i])/10
  fires_test$precipitation[i] <- as.numeric(fires_test$precipitation[i])/10
}

# Remove linhas com NA's
fires_train <- fires_train %>% na.rm()
##### Other Operations #####
ids <- fires_test$id

fires_train <- subset(fires_train, select=-c(id, extinction_date, firstInterv_date))
fires_test <- subset(fires_test, select=-c(id, extinction_date, firstInterv_date))

summary(fires_train)
summary(fires_test)

fwrite(fires_train, "fires_train_alt.csv")
fwrite(fires_test, "fires_test_alt.csv")
########## Data Exploratory Analysis ##########
##### Graphics #####
train <- read.csv("fires_train_alt.csv", header=TRUE)
test <- read.csv("fires_test_alt.csv", header=TRUE)

train %>% group_by(region) %>% summarize(FiresRegion=n()) %>% arrange(desc(FiresRegion))
ggplot(train, aes(x=region)) + geom_bar() + ggtitle("Number of Fires by Region") + xlab("Region") + ylab("Number of Fires")

train %>% group_by(district) %>% summarize(FiresDistrict=n()) %>% arrange(desc(FiresDistrict))
ggplot(train, aes(x=district)) + geom_bar() + ggtitle("Number of Fires by District") + xlab("District") + ylab("Number of Fires")

train %>% group_by(alert_month) %>% summarize(FiresMonth=n()) %>% arrange(desc(FiresMonth))    
ggplot(train, aes(x=alert_month)) + geom_bar() + ggtitle("Months with more Fires") + xlab("Months") + ylab("Number of Fires")

train %>% group_by(origin) %>% summarize(Origin=n()) %>% arrange(desc(Origin))
ggplot(train, aes(x=origin)) + geom_bar() + ggtitle("Origin of Fires") + xlab("Origin") + ylab("Number of Fires")

train %>% group_by(origin, district) %>% summarize(NumberFires=n()) %>% arrange(desc(NumberFires))
ggplot(train, aes(x=origin)) + geom_bar() + facet_wrap(~district) + ggtitle("Relation between District and Origin") + xlab("Origin") + ylab("Number of Fires")

train %>% group_by(intentional_cause) %>% summarize(Intentional=n()) %>% arrange(desc(Intentional))
ggplot(train, aes(x=intentional_cause)) + geom_bar() + ggtitle("Intentional Fires")

train_bottom <- train[with(train, order(total_area)),]
train_bottom <- train[1:10,]
train_top <- train %>% arrange(desc(total_area)) %>% slice(1:10) 

train_top %>% group_by(origin, total_area) %>% summarize(total_area) %>% arrange(desc(total_area)) 
ggplot(train_top, aes(x=origin, y=total_area)) + geom_point() + ggtitle("Relation between the Top 10 Total Area Burn and Origin") + xlab("Origin") + ylab("Total Area")

train_bottom %>% group_by(origin, total_area) %>% summarize(total_area) %>% arrange(desc(total_area))
ggplot(train_bottom, aes(x=origin, y=total_area)) + geom_point() + ggtitle("Relation between the Bottom 10 Total Area Burn and Origin") + xlab("Origin") + ylab("Total Area")

train %>% group_by(max_temperature, district) %>% summarize(NumberFires=n()) %>% arrange(desc(NumberFires)) 
ggplot(train, aes(x=as.numeric(max_temperature))) + geom_histogram(binwidth=1) + facet_wrap(~district) + ggtitle("Relation between District and the Maximum Temperature") + xlab("Maximum Temperature") + ylab("District")

train %>% group_by(max_temperature, alert_month) %>% summarize(NumberFires=n()) %>% arrange(desc(NumberFires))
ggplot(train, aes(x=as.numeric(max_temperature))) + geom_histogram(binwidth=1) + facet_wrap(~alert_month) + ggtitle("Relation between Month and the Maximum Temperature") + xlab("Maximum Temperature") + ylab("Month")

train_top %>% group_by(max_temperature, total_area) %>% summarize(total_area) %>% arrange(desc(total_area)) 
ggplot(train_top, aes(max_temperature, y=total_area)) + geom_point() + ggtitle("Relation between the Top 10 Total Area Burn and the Maximum Temperature") + xlab("Maximum Temperature") + ylab("Total Area")

train_bottom %>% group_by(max_temperature, total_area) %>% summarize(total_area) %>% arrange(desc(total_area))
ggplot(train_bottom, aes(x=max_temperature, y=total_area)) + geom_point() + ggtitle("Relation between the Bottom 10 Total Area Burn and the Maximum Temperature") + xlab("Maximum Temperature") + ylab("Total Area")

# Map
maps::map('worldHires','Portugal', ylim=c(37, 43), xlim=c(-10, -6), col="gray90", fill=TRUE, projection="gilbert", orientation=c(90,0,0))
title("Origin Fires")
colors <- c("Red", "Blue", "Orange", "Green", "Yellow")
labels <- c("Agric Burn", "Agriculture", "False Alarm", "Fire", "Firepit")
legend("r", labels, fill=colors)

# Guarda as coordenadas de cada origem de fogo, com uma area total maior que 0
lat_agric_burn <- filter(train, origin=="agric_burn", total_area>0) %>% select(lat) 
lon_agric_burn <- filter(train, origin=="agric_burn", total_area>0) %>% select(lon) 
lat_agriculture <- filter(train, origin=="agriculture", total_area>0) %>% select(lat) 
lon_agriculture <- filter(train, origin=="agriculture", total_area>0) %>% select(lon) 
lat_false_alarm <- filter(train, origin=="false_alarm", total_area>0) %>% select(lat) 
lon_false_alarm <- filter(train, origin=="false_alarm", total_area>0) %>% select(lon) 
lat_fire <- filter(train, origin=="fire", total_area>0) %>% select(lat) 
lon_fire <- filter(train, origin=="fire", total_area>0) %>% select(lon)
lat_firepit <- filter(train, origin=="firepit", total_area>0) %>% select(lat) 
lon_firepit <- filter(train, origin=="firepit", total_area>0) %>% select(lon)

for(i in 1:length(ids)) {
  coordinates_agric_burn <- mapproject(as.vector(lon_agric_burn[[1]][i]), as.vector(lat_agric_burn[[1]][i]), proj="gilbert", orientation=c(90, 0, 0))
  coordinates_agriculture <- mapproject(as.vector(lon_agriculture[[1]][i]), as.vector(lat_agriculture[[1]][i]), proj="gilbert", orientation=c(90, 0, 0))
  coordinates_false_alarm <- mapproject(as.vector(lon_false_alarm[[1]][i]), as.vector(lat_false_alarm[[1]][i]), proj="gilbert", orientation=c(90, 0, 0))
  coordinates_fire <- mapproject(as.vector(lon_fire[[1]][i]), as.vector(lat_fire[[1]][i]), proj="gilbert", orientation=c(90, 0, 0))
  coordinates_firepit <- mapproject(as.vector(lon_firepit[[1]][i]), as.vector(lat_firepit[[1]][i]), proj="gilbert", orientation=c(90, 0, 0))
  
  # Mete os pontos no mapa
  points(coordinates_agric_burn, pch=20, cex=0.75, col="red") 
  points(coordinates_agriculture, pch=20, cex=0.75, col="blue") 
  points(coordinates_false_alarm, pch=20, cex=0.75, col="orange") 
  points(coordinates_fire, pch=20, cex=0.75, col="green") 
  points(coordinates_firepit, pch=20, cex=0.75, col="yellow") 
}
########## Predictive Modelling ##########
train <- subset(train, select=-c(alert_day, alert_month))
test <- subset(test, select=-c(alert_day, alert_month))

fwrite(train, "fires_train_alt_final.csv")
fwrite(test, "fires_test_alt_final.csv")

train <- read.csv("fires_train_alt_final.csv", header=TRUE)
test <- read.csv("fires_test_alt_final.csv", header=TRUE)

treeTrain <- rpart(intentional_cause ~ ., train)
rpart.plot(treeTrain)
##### Divide Train in Test (30%) and Train (70%) #####
inTrain <- createDataPartition(y=train$intentional_cause, p=.7, list=FALSE)

train <- train %>% slice(inTrain)
test <- train %>% slice(-inTrain)

test_intentional_cause <- test$intentional_cause

test <- test %>% select(-intentional_cause)
##### Random Forest #####
train$intentional_cause <- factor(train$intentional_cause)

rf_model <- randomForest(intentional_cause ~ ., train, mtree=1000, importance=TRUE) 
rf_preds <- predict(rf_model, test)

rf_model$importance
varImpPlot(rf_model, main = "Feature Relevance Scores")

summary(rf_preds)

#confusionMatrix(factor(test_intentional_cause), factor(rf_preds))
  
kaggle_fires <- data.frame(matrix(ncol=0, nrow=4416))
kaggle_fires$id <- ids
kaggle_fires$intentional_cause <- rf_preds

for(i in 1:length(kaggle_fires$intentional_cause)) {
  if(kaggle_fires$intentional_cause[i] %>% any_na() == TRUE) {
    kaggle_fires$intentional_cause[i] <- 1
  }  
}

fwrite(kaggle_fires, "kaggle_fires_rf.csv")
##### Naive Bayes #####
train$region <- factor(train$region)
train$district <- factor(train$district)
train$municipality <- factor(train$municipality)
train$origin <- factor(train$origin)
train$alert_date <- factor(train$alert_date)
train$alert_hour <- factor(train$alert_hour)
train$village_area <- factor(train$village_area)
train$village_veget_area <- factor(train$village_veget_area)
train$vegetation_area <- factor(train$vegetation_area)
train$farming_area <- factor(train$farming_area)
train$total_area <- factor(train$total_area)
train$intentional_cause <- factor(train$intentional_cause)
train$max_temperature <- factor(train$max_temperature)
train$min_temperature <- factor(train$min_temperature)
train$avg_temperature <- factor(train$avg_temperature)
train$precipitation <- factor(train$precipitation)
test$region <- factor(test$region)
test$district <- factor(test$district)
test$municipality <- factor(test$municipality)
test$origin <- factor(test$origin)
test$alert_date <- factor(test$alert_date)
test$alert_hour <- factor(test$alert_hour)
test$village_area <- factor(test$village_area)
test$village_veget_area <- factor(test$village_veget_area))
test$vegetation_area <- factor(test$vegetation_area)
test$farming_area <- factor(test$farming_area))
test$total_area <- factor(test$total_area)
test$max_temperature <- factor(test$max_temperature)
test$min_temperature <- factor(test$min_temperature)
test$avg_temperature <- factor(test$avg_temperature)
test$precipitation <- factor(test$precipitation)

levels(test$municipality) <- levels(droplevels((train$municipality)))
levels(test$alert_date) <- levels(droplevels((train$alert_date)))
levels(test$alert_hour) <- levels(droplevels((train$alert_hour)))
levels(test$village_area) <- levels(droplevels((train$village_area)))
levels(test$village_veget_area) <- levels(droplevels((train$village_veget_area)))
levels(test$vegetation_area) <- levels(droplevels((train$vegetation_area)))
levels(test$farming_area) <- levels(droplevels((train$farming_area)))
levels(test$total_area) <- levels(droplevels((train$total_area)))
levels(test$max_temperature) <- levels(droplevels((train$max_temperature)))
levels(test$min_temperature) <- levels(droplevels((train$min_temperature)))
levels(test$avg_temperature) <- levels(droplevels((train$avg_temperature)))
levels(test$precipitation) <- levels(droplevels((train$precipitation)))

nb_model <- naive_bayes(intentional_cause ~., train, laplace=1)
nb_preds <- predict(nb_model, test, type="class")

summary(nb_preds)

#confusionMatrix(factor(test_intentional_cause), factor(nb_preds))

kaggle_fires <- data.frame(matrix(ncol=0, nrow=4416))
kaggle_fires$id <- ids
kaggle_fires$intentional_cause <- nb_preds

fwrite(kaggle_fires, 'kaggle_fires_nb.csv')
##### Bagging #####
train$region <- factor(train$region)
train$district <- factor(train$district)
train$municipality <- factor(train$municipality)
train$origin <- factor(train$origin)
train$alert_date <- factor(train$alert_date)
train$alert_hour <- factor(train$alert_hour)
train$village_area <- factor(train$village_area)
train$village_veget_area <- factor(train$village_veget_area)
train$vegetation_area <- factor(train$vegetation_area)
train$farming_area <- factor(train$farming_area)
train$total_area <- factor(train$total_area)
train$intentional_cause <- factor(train$intentional_cause)
train$max_temperature <- factor(train$max_temperature)
train$min_temperature <- factor(train$min_temperature)
train$avg_temperature <- factor(train$avg_temperature)
train$precipitation <- factor(train$precipitation)
test$region <- factor(test$region)
test$district <- factor(test$district)
test$municipality <- factor(test$municipality)
test$origin <- factor(test$origin)
test$alert_date <- factor(test$alert_date)
test$alert_hour <- factor(test$alert_hour)
test$village_area <- factor(test$village_area)
test$village_veget_area <- factor(test$village_veget_area))
test$vegetation_area <- factor(test$vegetation_area)
test$farming_area <- factor(test$farming_area))
test$total_area <- factor(test$total_area)
test$max_temperature <- factor(test$max_temperature)
test$min_temperature <- factor(test$min_temperature)
test$avg_temperature <- factor(test$avg_temperature)
test$precipitation <- factor(test$precipitation)

levels(test$municipality) <- levels(droplevels((train$municipality)))
levels(test$alert_date) <- levels(droplevels((train$alert_date)))
levels(test$alert_hour) <- levels(droplevels((train$alert_hour)))
levels(test$village_area) <- levels(droplevels((train$village_area)))
levels(test$village_veget_area) <- levels(droplevels((train$village_veget_area)))
levels(test$vegetation_area) <- levels(droplevels((train$vegetation_area)))
levels(test$farming_area) <- levels(droplevels((train$farming_area)))
levels(test$total_area) <- levels(droplevels((train$total_area)))
levels(test$max_temperature) <- levels(droplevels((train$max_temperature)))
levels(test$min_temperature) <- levels(droplevels((train$min_temperature)))
levels(test$avg_temperature) <- levels(droplevels((train$avg_temperature)))
levels(test$precipitation) <- levels(droplevels((train$precipitation)))

bg_model <- bagging(intentional_cause ~ ., train, mfinal=100)
bg_preds <- predict(bg_model, test, type="class")

summary(factor(bg_preds$class))

#confusionMatrix(factor(test_intentional_cause), factor(bg_preds$class))

kaggle_fires <- data.frame(matrix(ncol=0, nrow=4416))
kaggle_fires$id <- ids
kaggle_fires$intentional_cause <- bg_preds$class

fwrite(kaggle_fires, 'kaggle_fires_bg.csv')
##### AdaBoost #####
train$region <- factor(train$region)
train$district <- factor(train$district)
train$municipality <- factor(train$municipality)
train$origin <- factor(train$origin)
train$alert_date <- factor(train$alert_date)
train$alert_hour <- factor(train$alert_hour)
train$village_area <- factor(train$village_area)
train$village_veget_area <- factor(train$village_veget_area)
train$vegetation_area <- factor(train$vegetation_area)
train$farming_area <- factor(train$farming_area)
train$total_area <- factor(train$total_area)
train$intentional_cause <- factor(train$intentional_cause)
train$max_temperature <- factor(train$max_temperature)
train$min_temperature <- factor(train$min_temperature)
train$avg_temperature <- factor(train$avg_temperature)
train$precipitation <- factor(train$precipitation)
test$region <- factor(test$region)
test$district <- factor(test$district)
test$municipality <- factor(test$municipality)
test$origin <- factor(test$origin)
test$alert_date <- factor(test$alert_date)
test$alert_hour <- factor(test$alert_hour)
test$village_area <- factor(test$village_area)
test$village_veget_area <- factor(test$village_veget_area)
test$vegetation_area <- factor(test$vegetation_area)
test$farming_area <- factor(test$farming_area)
test$total_area <- factor(test$total_area)
test$max_temperature <- factor(test$max_temperature)
test$min_temperature <- factor(test$min_temperature)
test$avg_temperature <- factor(test$avg_temperature)
test$precipitation <- factor(test$precipitation)

levels(test$municipality) <- levels(droplevels((train$municipality)))
levels(test$alert_date) <- levels(droplevels((train$alert_date)))
levels(test$alert_hour) <- levels(droplevels((train$alert_hour)))
levels(test$village_area) <- levels(droplevels((train$village_area)))
levels(test$village_veget_area) <- levels(droplevels((train$village_veget_area)))
levels(test$vegetation_area) <- levels(droplevels((train$vegetation_area)))
levels(test$farming_area) <- levels(droplevels((train$farming_area)))
levels(test$total_area) <- levels(droplevels((train$total_area)))
levels(test$max_temperature) <- levels(droplevels((train$max_temperature)))
levels(test$min_temperature) <- levels(droplevels((train$min_temperature)))
levels(test$avg_temperature) <- levels(droplevels((train$avg_temperature)))
levels(test$precipitation) <- levels(droplevels((train$precipitation)))

b_model <- boosting(intentional_cause ~ ., train, boost=TRUE, mfinal=100)
b_preds <- predict(b_model, test, type="class")

summary(factor(b_preds$class))

#confusionMatrix(factor(test_intentional_cause), factor(b_preds$class))

kaggle_fires <- data.frame(matrix(ncol=0, nrow=4416))
kaggle_fires$id <- ids
kaggle_fires$intentional_cause <- b_preds$class

fwrite(kaggle_fires, "kaggle_fires_b.csv")