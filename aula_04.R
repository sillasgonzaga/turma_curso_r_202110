library(tidyverse)

# Leia o arquivo listings com a função read_csv() e salve no objeto df_anuncios
df <- readr::read_csv("dados/listings.csv.gz")

df

#read.csv()

# Inspecione os dados: funções summary() e glimpse()

summary(df)
glimpse(df)

# A partir do output de glimpse(), explique as diferentes classes de objetos no R.
glimpse(df)

# Observe o problema nas variáveis de preço


# Retorne a url (scrape_url) do anúncio mais caro do airbnb
df %>% 
  select(listing_url, price, neighbourhood_cleansed) %>% 
  mutate(price = as.numeric(price)) %>% 
  arrange(price)

as.numeric("200")
as.numeric("$ 200")
as.numeric(" 200")


df$price
as.numeric(str_sub(df$price, 2))[6]
df$price[6]
as.numeric("1699.00")
parse_number(df$price)

parse_number("Meu número é 11 95810 1474")

df <- df %>% 
  mutate(price_clean = parse_number(price))

glimpse(df)
class(df$price)

df %>% 
  select(listing_url, price, price_clean, neighbourhood_cleansed) %>% 
  arrange(desc(price_clean))

df %>% select(neighbourhood_cleansed)


# funcao(x)
# é equivalente a
# x %>% f()


# select(df, ...) == df %>% select(...)

# Retorne o nome do host (host_name) que tem a maior quantidade de anúncios
df %>% 
  select(host_name)


df %>% 
  group_by(host_url, host_name) %>%
  summarise(qtd_anuncios = n()) %>% 
  arrange(desc(qtd_anuncios))


df %>% 
  count(host_url, host_name, sort = TRUE, name = "qtd_anuncios")


# Retorne a quantidade de hosts por ano em que entrou no airbnb
library(lubridate)

df %>% 
  select(host_id, host_name, host_since) %>% 
  mutate(host_ano = year(host_since)) %>% 
  count(host_ano)

df %>% 
  distinct(host_id, host_name, host_since) %>% 
  filter(host_id == 91654021)

df %>% 
  distinct(host_id, host_name, host_since) %>% 
  mutate(host_ano = year(host_since)) %>% 
  count(host_ano)


df %>% 
  select(name, description, neighbourhood_cleansed) %>% 
  filter(str_detect(description, "praia") | str_detect(description, "beach")) 


# Imóveis que mencionam a palavra praia são em média mais caros?

df %>% 
  select(description, price_clean) %>% 
  mutate(description = tolower(description)) %>% 
  mutate(tem_praia = str_detect(description, "praia") |
           str_detect(description, "beach")) %>% 
  group_by(tem_praia) %>% 
  summarise(preco_medio = mean(price_clean),
            preco_mediano = median(price_clean),
            qtd_anuncios = n())


  tolower(c("Sillas", "Beach", "BEACH", "beach"))

# Use mutate() para modificar o dataframe criando uma coluna booleana 
  # chamada esgotado informando se o imovel esta indisponivel para os 
  # proximos 30 dias (coluna availability_30)

  
  df %>% 
    select(availability_30)
  
  df <- df %>% 
    mutate(esgotado = availability_30 == 0)
  
glimpse(df)


# Quais os 5 bairros que possuem mais de 100 anúncios com a maior taxa 
# de anúncios esgotados nos próximos 30 dias? Dica: crie duas colunas com 
#summarise, uma usando n() e outra com mean(esgotado) e depois use filter(),
# arrange() e head()


df %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(
    qtd_anuncios = n(),
    taxa_esgotado = mean(esgotado)
  ) %>% 
  filter(qtd_anuncios > 100) %>% 
  arrange(desc(taxa_esgotado))

# Retorne a quantidade de anúncios e reviews (number_of_reviews) por bairro, 
# calcule uma taxa de quantidade de reviews por quantidade de anuncios.


df %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(
    qtd_anuncios = n(),
    taxa_esgotado = mean(esgotado),
    qtd_reviews = sum(number_of_reviews)
  ) %>% 
  mutate(taxa = qtd_reviews / qtd_anuncios) %>% 
  arrange(desc(qtd_anuncios))

# Quais são os diferentes tipos de anúncio (quarto, apt, etc.) que existem?
# (Coluna room_type)

df %>% 
  distinct(room_type)

# A quantidade de quartos tem relação com o preço dos apartamentos inteiros?
df %>% 
  filter(room_type == "Entire home/apt") %>% 
  select(bedrooms, price_clean) %>% 
  group_by(bedrooms) %>% 
  summarise(valor_mediano = median(price_clean),
            qtd_anuncios = n())





# DESAFIO

# Vocês querem ficar em Ipanema, Copacabana ou Leblon.
# Vocês preferem que o host esteja no mesmo bairro.
# Querem um apartamento inteiro só para vocês que seja “instant bookable”


df %>% 
  select(listing_url, neighbourhood_cleansed, host_neighbourhood,
         availability_30, minimum_nights, 
         instant_bookable,
         price_clean, room_type, number_of_reviews,
         review_scores_rating) %>% 
  filter(
    neighbourhood_cleansed %in% c("Copacabana", "Ipanema", "Leblon"),
    host_neighbourhood == neighbourhood_cleansed,
    room_type == "Entire home/apt",
    instant_bookable,
    availability_30 >= 5,
    minimum_nights <= 5,
    review_scores_rating >= 4.93
    ) %>% 
  mutate(valor_total_viagem = price_clean * 5 * 2) %>% 
  select(listing_url, neighbourhood_cleansed,
         valor_total_viagem, review_scores_rating) %>% 
  filter(valor_total_viagem <= quantile(valor_total_viagem, .25))
  


vec_num <- c(1, 5, -1, 10, 20, 50, 100, -9, 15)
summary(vec_num)
quantile(vec_num, 0.25)










