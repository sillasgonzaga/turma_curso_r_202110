library(tidyverse)

a <- 'texto 1'
b <- 'texto 2'
c <- 'texto 3'

a
b
c

c(a, b, c)

str_c(a, b, c, sep = ", ")
?str_c

nomes <- c("Camila", "Glaudemias", "Amanda")
sobrenomes <- c("Lupetti", "Grangeiros", "Barcelos")

nomes
sobrenomes

str_c(nomes, sobrenomes, sep = " ")

cnae.texto <- c('10 Fabricação de produtos alimentícios', 
                '11 Fabricação de bebidas',
                '12 Fabricação de produtos do fumo',
                '13 Fabricação de produtos têxteis',
                '14 Confecção de artigos do vestuário e acessórios',
                '15 Preparação de couros e fabricação de artefatos de couro, artigos para viagem e calçados',
                '16 Fabricação de produtos de madeira',
                '17 Fabricação de celulose, papel e produtos de papel')

cnae.texto

str_sub(cnae.texto, 1, 3)
str_sub(cnae.texto, 4, -1)


telefones <- c('11-9931-9572', '8591-5772', '8562-1923')

str_replace(telefones, "-", "")
str_replace_all(telefones, "-", "")


nomes
str_replace_all(nomes, "a", "@")


telefones2 <- c('11-99931-9572', '79-48591-5772', '21-98562-1923')


telefones2 <- str_remove_all(telefones2, "-")
telefones2

str_sub(telefones2, 1, 2)
str_sub(telefones2, 3, 7)
str_sub(telefones2, 8, -1)

str_c(
  "(",
  str_sub(telefones2, 1, 2), # ddd
  ") ",
  # primeira parte do telefone (5 primeiros digitos)
  str_sub(telefones2, 3, 7),
  " ",
  str_sub(telefones2, 8, -1)
)

?str_sub

cnpj <- c('19.702.231/9999-98',
          '19.498.482/9999-05', 
          '19.499.583/9999-50', 
          '19.500.999/9999-46',
          '19.501.139/9999-90')

readr::parse_number(cnpj)


cnpj %>% 
  str_remove_all("-") %>% 
  str_remove_all("/") %>% 
  str_remove_all(".")


nomes
str_glue("Meu nome é {nomes}, prazer")

?str_glue()

library(nycflights13)
flights

nrow(flights)

str_glue("No ano de 2013 foram realizados {nrow(flights)} vôos em NY")

# 7.1.3 Buscar correspondências em um string
telefones2
str_detect(telefones2, "7")

telefones2 <- c(telefones2, "51611547894")
telefones2
str_detect(telefones2, "11")

?str_starts
str_starts(telefones2, "11")

nomes
str_detect(nomes, "d")


str_detect(nomes, "a")

str_count(str_to_lower(nomes), "a")
nomes %>% str_to_lower() %>% str_count("a")


nomes <- c(nomes, "Alfredo")
nomes


str_detect(str_to_lower(nomes), "a")
str_detect(nomes, "a|A")
str_detect(nomes, "u")
str_detect(nomes, "l")

# 1a forma: | por fora
str_detect(nomes, "u") | str_detect(nomes, "l")
# 2a forma: | no str_detect
str_detect(nomes, "u|l")

# 7.1.4 Complementar uma string


cpfs <- c(1234, 01833827570, 45614814570, 4, 4000001111)
cpfs
str_length(cpfs)

as.character(cpfs)

?str_pad

str_pad(cpfs, 11, side = "left", pad = "0")
str_pad(cpfs, 11, side = "left", pad = "@")

# 7.1.5 Remover espaços em branco desnecessários

x <- c("      inicio",
       "final      ",
       "      ambos      ",
       "    no       meio        ")
x

str_squish(x)

# 7.2 regex

cnpj %>% 
  str_remove_all("-") %>% 
  str_remove_all("/") %>% 
  str_remove_all("\\.")

str_remove_all(cnpj, "-|/|\\.")



textos <- c("Fulano", "fulano", "abcdeFgF", "01584", 
            "abc456", "123def", "OI", "meuemail@gmail.com",
            "www.google.com", "Meu nome é Fulano", "regex\\d",
            "daskdneikwww.g1.com.br",
            "c@s@")


str_detect(textos, "F")
str_starts(textos, "F")
str_detect(textos, "^F")

str_subset(textos, "^F")
str_subset(textos, "F")


str_subset(textos, "\\d")
str_subset(textos, "\\\\d")

str_subset(textos, "[0-9]")


str_subset(textos, "^www\\.")

str_subset(textos, "@") %>% 
  str_subset("\\.com$")


# exercicios do literaturabr
library(literaturaBR)
install.packages("remotes")
remotes::install_github("sillasgonzaga/literaturaBR")

df_livros <- literaturaBR::load_all_books()

df_livros <- as_tibble(df_livros)
df_livros

class(df_livros)

df_livros %>% 
  distinct(book_name)


#Quebre cada linha da coluna text em varias, tendo uma palavra por linha,
# usando separate_rows(),
df_livros %>% 
  separate_rows(text, sep = " ")

#e filtre as linhas da nova coluna que contem apenas letras.
df_livro_sep <- df_livros %>% 
  separate_rows(text, sep = " ") %>% 
  filter(str_detect(text, "[:alpha:]"))

# Salve em um novo dataframe chamado df_livros_sep.
textos

str_subset(textos, "[A-Z]|[a-z]")
str_subset(textos, "[:alpha:]")

df_livro_sep %>% 
  group_by(book_name) %>% 
  summarise(
    n_total = n(),
    n_termos_distintos = n_distinct(text),
    taxa = n_termos_distintos/n_total
  )


df_livro_sep %>% 
  group_by(book_name) %>% 
  summarise(
    n_total = n(),
    n_termos_distintos = n_distinct(text),
    qtd_palavras_com_a = sum(str_detect(text, "a|A")),
    qtd_ocorrencias_amor = sum(str_detect(text, "amor|Amor")),
    taxa = n_termos_distintos/n_total,
    pct_ocorrencia_amor = qtd_ocorrencias_amor/n_total
  ) %>% 
  arrange(desc(pct_ocorrencia_amor))

# Calcule a proporção de palavras que contem a letra a por livro.

df_livro_sep %>% 
  group_by(book_name)



df_livro_sep %>% 
  group_by(book_name) %>% 
  summarise( n_total = n(), 
             n_termos_distintos = n_distinct(text),
             qtd_ocorrencias_amor = sum(str_detect(text,'amor|Amor')),
             taxa = n_termos_distintos/n_total, #taxa
             pct_ocorrencia_amor = qtd_ocorrencias_amor/n_total ) %>%
  arrange(desc(pct_ocorrencia_amor)) 


x <- c("Juiz", 
       "Meu pai é juiz",
       "eu sou Juiz",
       "a ação foi analisado por um juiz",
       "meus tios são juizes",
       "a ação foi ajuizada")

str_subset(x, "juiz|Juiz")

str_subset(x, "\\bjuiz\\b")


#### datas

# AAAA-MM-DD
# no brasil: DD/MM/AAAA
x <- c("2014-07-15", "2018/03/20", "2019-12-31", "20170511", "151028", "palavra")

class(x)
class(as.Date(x))

library(lubridate)

lubridate::as_date(x)

?as_date

datas_brasil <- c("01/12/2019", "20/11/2018", "30011990", "17-03-2000")

dmy(datas_brasil)

dmy_hms("01-01-1993 22:15:51")
class(dmy_hms("01-01-1993 22:15:51"))


seq.Date(from = as.Date("2021-02-15"),
         to = as.Date("2021-04-01"),
         by = "3 days")

seq.Date(from = as.Date("2021-10-27"),
         by = "1 week",
         length.out = 6)

datas_brasil <- dmy_hms(c("01/12/2019 13:51:15",
                          "20/11/2018 00:00:00",
                          "30011990 080000",
                          "17-03-2000 203000"))

datas_brasil
class(datas_brasil)

year(datas_brasil)
month(datas_brasil)
day(datas_brasil)
hour(datas_brasil)
minute(datas_brasil)
second(datas_brasil)


?wday
wday(datas_brasil, label = TRUE, abbr = FALSE)
week(datas_brasil)
yday(datas_brasil)
quarter(datas_brasil)
semester(datas_brasil)









