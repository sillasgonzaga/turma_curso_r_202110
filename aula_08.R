library(tidyverse)
library(lubridate)

datas_brasil <- c("01/12/2019 13:51:15",
                  "20/11/2018 00:00:00",
                  "30011990 080000",
                  "17-03-2000 203000")

datas_brasil
class(datas_brasil)

# pacote para representação de tempo: anytime
datas_brasil <- dmy_hms(datas_brasil)
class(datas_brasil)

# Adicionar uma quantidade N de dias/meses/anos/etc em uma data:

datas_brasil
?ddays()

datas_brasil

datas_brasil + ddays(10)
datas_brasil - ddays(10)

dmy("01/12/2019 13:51:15")
dmy_hms("01/12/2019 13:51:15")


dmy_hms("01/12/2019")
dmy("01/12/2019")

datas_brasil + ddays(27)

datas_brasil + dhours(4)

datas_brasil
datas_brasil - dweeks(3)

# Calcular a diferença de tempo entre duas datas:
data1 <- dmy_hms("01/09/1993 20:00:00")
data2 <- dmy_hms("01-11-2021 17:00:00")
data3 <- dmy_hms("01-11-2021 16:23:00")

data1
data2

data2 - data1
data2 - data3


class(data2 - data1)
# convertendo para numerico
as.numeric(data2 - data1)
as.numeric(data2 - data3)

?difftime

as.numeric(difftime(data2, data1, units = "days"))
difftime(data2, data3, units = "days") %>% as.numeric()


datas_brasil

difftime(datas_brasil, data1, units = "days") %>% as.numeric()



as.numeric(difftime(datas_brasil, data1, units = "days"))/12
as.numeric(difftime(datas_brasil, data1, units = "days"))/365

# Arredondar datas:
?floor_date

wday(datas_brasil)


floor_date(datas_brasil, unit = "week")




?runif

round(runif(365, 0, 100))
sample(0:100, size = 365, replace = TRUE)
?sample
set.seed(123)
sample(0:5, size = 4, replace = FALSE)


set.seed(124)
df_likes <- tibble(
  data = seq.Date(today(),
                  by = "1 day",
                  length.out = 365),
  likes = sample(0:100, size = 365, replace = TRUE)
)

df_likes %>% 
  mutate(data_semana = floor_date(data, "week")) %>% 
  group_by(data_semana) %>% 
  summarise(total_likes = sum(likes))



df_likes_mes <- df_likes %>% 
  mutate(data_mes = floor_date(data, "month")) %>% 
  group_by(data_mes) %>% 
  summarise(total_likes = sum(likes))

ceiling_date(datas_brasil, "month") - ddays(1)
floor_date(datas_brasil, "month")

df_likes_mes
class(df_likes_mes)

df_likes_mes %>% write_csv("likes por mes.csv")

df_likes_mes_2 <- readr::read_csv("likes por mes.csv")
df_likes_mes_2
class(df_likes_mes_2)


df_likes_mes %>% write_rds("likes por mes.rds")

df_likes_mes_3 <- read_rds("likes por mes.rds")
df_likes_mes_3
class(df_likes_mes_3)
class(df_likes_mes)


datas_brasil %>% write_rds("vetor datas.rds")
datas_brasil %>% write_csv("vetor datas.csv")

read_rds("vetor datas.rds")

#library(writexl)
df_likes_mes %>% writexl::write_xlsx("likes por mes.xlsx")

#### api

url <- "https://api.github.com/users/sillasgonzaga"

library(jsonlite)

fromJSON(url)
class(fromJSON(url))

?fromJSON

fromJSON(url, simplifyDataFrame = TRUE) %>% 
  as_tibble()

url <- "https://dadosabertos.camara.leg.br/api/v2/proposicoes?idDeputadoAutor=204534"

proposicoes <- fromJSON(url)
proposicoes$dados %>% 
  as_tibble()

#install.packages("")


#### web scraping

wikipedia <- "https://pt.wikipedia.org/wiki/Campeonato_Brasileiro_de_Futebol_de_2018_-_S%C3%A9rie_A#Classifica%C3%A7%C3%A3o"

library(rvest)

wiki <- read_html(wikipedia)

wiki <- html_table(wiki)

df_classificacao <- wiki[[6]]
df_classificacao

#### capitulo 11

a <- c(1, 2, "c", 4)
class(a)

c(1, 2, df_likes_mes)


numero_inteiro <- 2
numero_decimal <- 3.14
string <- "ibpad"
data <- today()
df_likes_mes


minha_lista <- list(numero_inteiro,
                    numero_decimal,
                    string, 
                    data,
                    df_likes_mes,
                    vetor_num = c(5, 8, 0))


class(minha_lista)
minha_lista
class(minha_lista[[1]])



minha_lista$vetor_num
minha_lista[[6]]


minha_lista$vetor_num * 2

minha_lista * 2

lista_vetores <- list(
  v1 = c(-3, -2, -1),
  v2 = c(0, 2, 4),
  v3 = c(1, 3, 5)
)

lista_vetores * 2 

class(lista_vetores)

minha_lista
class(minha_lista)


map(minha_lista, class)

lista_vetores
mean(lista_vetores)

map(lista_vetores, mean)


multiplica_por_2 <- function(x){
  x * 2
}
# sintaxe: map(LISTA, sum(y))
map(lista_vetores, multiplica_por_2)



# funcao anonima:
# SINTAXE: map(LISTA, function(x) ...)


map(lista_vetores, function(z) z * 3)


# funcao anonima com ~:
# SINTAXE: map(LISTA, ~ .x)
map(lista_vetores, ~ .x * 3)


lista_vetores <- list(
  v1 = c(-3, -2, -1),
  v2 = c(0, 2),
  v3 = c(1, 3, 5),
  v4 = c(0, -9, 3.41, 9, 50)
)

lista_vetores

length(lista_vetores)

map(lista_vetores, length)
class(map(lista_vetores, length))


map_dbl(lista_vetores, length)
class(map_dbl(lista_vetores, length))


map_dbl(lista_vetores, sum)


map(lista_vetores, function(x) mean(x) > 1)
map_lgl(lista_vetores, function(x) mean(x) > 1)


### projeto


df <- read_csv("dados/kaggle-cap11/DAYTON_hourly.csv")
df

nome_regiao <- str_sub(colnames(df)[2], 1, -4)

df %>% 
  # SINTAXE de rename:
  # rename(DATAFRAME, NOVO NOME DA COL = ANTIGO NOME DA COL)
  # rename(DATAFRAME, NOVO NOME DA COL = POSICAO DA COL)
  rename(consumo_mw = 2) %>% 
  mutate(
    mes = lubridate::month(Datetime),
    regiao = nome_regiao
  ) %>% 
  group_by(mes, regiao) %>% 
  summarise(consumo_medio = mean(consumo_mw))


calcular_consumo_medio <- function(arquivo){
  
  df <- read_csv(arquivo)

  nome_regiao <- str_sub(colnames(df)[2], 1, -4)
  
  df %>% 
    rename(consumo_mw = 2) %>% 
    mutate(
      mes = lubridate::month(Datetime),
      regiao = nome_regiao
    ) %>% 
    group_by(mes, regiao) %>% 
    summarise(consumo_medio = mean(consumo_mw))
  
}

calcular_consumo_medio("dados/kaggle-cap11/AEP_hourly.csv")
calcular_consumo_medio("dados/kaggle-cap11/DUQ_hourly.csv")


read_csv("dados/kaggle-cap11/pjm_hourly_est.csv")


# dir()
arquivos_projeto <- list.files("dados/kaggle-cap11/", 
                               full.names = TRUE,
                               pattern = "hourly\\.csv$")
arquivos_projeto


arquivos_projeto[3] %>% calcular_consumo_medio()


str_detect("arquivo_csv.parquet", "csv$")



df_consumo <- map(arquivos_projeto, calcular_consumo_medio)
df_consumo
class(df_consumo)
map(df_consumo, class)

bind_rows(df_consumo)


df_consumo <- map_df(arquivos_projeto, calcular_consumo_medio)
df_consumo



























