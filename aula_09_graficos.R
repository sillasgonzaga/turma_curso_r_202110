library(tidyverse)
library(readxl)
library(janitor)
library(countrycode) # install.packages("contrycode")
library(wpp2019)

# baixe o arquivo do endereco:
download.file("https://s3.amazonaws.com/happiness-report/2018/WHR2018Chapter2OnlineData.xls",
              destfile = "felicidade.xls",
              mode = "wb")

df_feliz <- read_excel("felicidade.xls") %>% 
  janitor::clean_names() %>% 
  rename(life_expec = healthy_life_expectancy_at_birth)

glimpse(df_feliz)

unique(df_feliz$country)

View(countrycode::codelist)

df_continentes <- codelist %>% 
  select(continent, country.name.en, cow.name)

df_continentes %>% filter(is.na(continent))


df_continentes <- codelist %>% 
  select(continent, country.name.en, country_code = iso3n) %>% 
  filter(!is.na(continent))

df_continentes


df_feliz$country[df_feliz$country == "Bosnia and Herzegovina"] <- "Bosnia & Herzegovina"
df_feliz$country[df_feliz$country == "Czech Republic"] <- "Czechia"
df_feliz$country[df_feliz$country == "Hong Kong S.A.R. of China"] <- "Hong Kong SAR China"
df_feliz$country[df_feliz$country == "Taiwan Province of China"] <- "Taiwan"


df_feliz_completo <- left_join(df_feliz,
                               df_continentes,
                               by = c("country" = "country.name.en"))

df_feliz_completo %>% 
  distinct(country, continent) %>% 
  filter(is.na(continent))

View(df_continentes)

glimpse(df_feliz_completo)

df_feliz_completo$year %>% unique() %>% sort()

data(pop)
head(pop)

df_pop_long <- pop %>% 
  as_tibble() %>% 
  pivot_longer(cols = `1950`:`2020`,
               names_to = "ano",
               values_to = "population") %>% 
  select(-name) %>% 
  mutate(ano = as.numeric(ano))


df_feliz_completo <- left_join(df_feliz_completo,
                               df_pop_long,
                               by = c("country_code"  = "country_code",
                                      "year" = "ano"))

glimpse(df_feliz_completo)

max(df_feliz_completo$year)

df_2015 <- df_feliz_completo %>% filter(year == 2015)

df_2015 %>% filter(country == "Brazil")


# nosso primeiro grafico

df_populacao_ano <- df_feliz_completo %>% 
  group_by(year) %>% 
  summarise(populacao_mundo = median(population, na.rm = TRUE),
            qtd_paises = n()) %>% 
  filter(!is.na(populacao_mundo))

sum(c(100, 500, 600, NA, 1000), na.rm = TRUE)

df_populacao_ano

ggplot(data = df_populacao_ano, aes(x = year, y = populacao_mundo)) +
  geom_line()

ggplot(data = df_populacao_ano, aes(x = year, y = populacao_mundo)) +
  geom_point()


ggplot(data = df_populacao_ano, aes(x = year, y = populacao_mundo)) +
  geom_col()


glimpse(df_feliz_completo)
# 
# df_feliz_completo %>% 
#   ggplot(aes(x = life_expec, y = log_gdp_per_capita)) +
#   geom_point()



df_2015 %>% 
  ggplot(aes(y = life_expec, x = log_gdp_per_capita, color = continent)) +
  geom_point()

# Y = f(X)
df_2015

glimpse(df_2015)

df_2015

df_2015 %>% 
  filter(!is.na(continent)) %>% 
  ggplot(aes(x = perceptions_of_corruption, y = life_ladder, color = continent)) +
  geom_point()

df_2015 %>% 
  ggplot() +
  geom_point(aes(x = perceptions_of_corruption, y = life_ladder, color = continent))

df_2015 %>% 
  filter(life_ladder < 4, perceptions_of_corruption < 0.25)

# analisando o impacto da posição do aes()
df_2015 %>% 
  filter(!is.na(continent)) %>% 
  ggplot(aes(x = log_gdp_per_capita, y = life_expec, color = continent)) +
  geom_point() +
  geom_smooth(method = "lm")

df_2015 %>% 
  filter(!is.na(continent)) %>% 
  ggplot(aes(x = log_gdp_per_capita, y = life_expec)) +
  geom_point(aes(color = continent)) +
  geom_smooth(method = "lm")

df_2015 %>% 
  filter(!is.na(continent)) %>% 
  ggplot(aes(x = log_gdp_per_capita, y = life_expec)) +
  geom_point(color = "red",
             alpha = 0.4)


df_aleatorio <- tibble(
  varx = rnorm(100000),
  vary = rnorm(100000)
)

df_aleatorio %>% 
  ggplot(aes(x  = varx, y = vary)) +
  geom_point(alpha = 0.1)


df_2015 %>% 
  filter(!is.na(continent)) %>% 
  ggplot(aes(x = log_gdp_per_capita, y = life_expec)) +
  geom_point(color = "red",
             alpha = 1, 
             size = 5,
             shape = 7)

df_2015 %>% 
  filter(!is.na(continent)) %>%
  ggplot(aes(x = log_gdp_per_capita, y = life_expec, size = population)) + 
  geom_point() 


df_2015 %>% 
  filter(!is.na(continent)) %>% 
  ggplot(aes(x = log_gdp_per_capita, y = life_expec, size = population)) +
  geom_point()

df_2015 %>% 
  group_by(continent) %>% 
  summarise(life_expec_media = mean(life_expec)) %>% 
  na.omit() %>% 
  ggplot(aes(x = continent, y = life_expec_media)) +
  geom_col()

options(scipen = 999)

df_2015 %>% 
  slice_max(order_by = population, n = 15) %>% 
  ggplot(aes(y = country, x = population, fill = continent)) +
  geom_col()

# colocando as barras em ordem
?fct_reorder

df_2015 %>% 
  slice_max(order_by = population, n = 15) %>% 
  ggplot(aes(y = fct_reorder(country, population),
             x = population, 
             fill = continent)) +
  geom_col()


df_feliz_completo %>% 
  count(year)

df_feliz_completo %>% 
  filter(year %in% c(2011, 2017)) %>% 
  mutate(year = as.character(year)) %>% 
  group_by(continent, year) %>% 
  summarise(pib_per_capita_medio = mean(log_gdp_per_capita, na.rm = TRUE)) %>% 
  na.omit() %>% 
  ggplot(aes(x = continent, y = pib_per_capita_medio, fill = year)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c("black", "yellow"))


### grafico linhas
library(rbcb)

#remotes::install_github("wilsonfreitas/rbcb")
lista_datasets <- rbcb::get_series(code = c(ipca = 433, selic = 4390))

df_st <- left_join(lista_datasets$ipca,
                   lista_datasets$selic,
                   by = "date")

library(lubridate)
today() - dyears(10)

df_st %>% 
  filter(date >= today() - dyears(10)) %>% 
  ggplot(aes(x = date, y = ipca)) +
  geom_line() +
  geom_point() +
  geom_smooth()


df_st_long <- df_st %>% 
  na.omit() %>% 
  pivot_longer(
    cols = -date,
    names_to = "indicador",
    values_to = "taxa"
  )

df_st_long %>%
  filter(date >= today() - dyears(5)) %>% 
  ggplot(aes(x = date, y = taxa, color = indicador)) +
  geom_line() +
  geom_point(aes(shape = indicador))

### histogramas
df_2015$life_expec %>% summary()

df_2015 %>% 
  ggplot(aes(x = life_expec)) +
  geom_histogram(color = "black",
                 binwidth = 5,
                 boundary = 0,
                 fill = "red")


### boxplot
df_2015 %>% 
  ggplot(aes(x = continent,  y = life_expec)) +
  geom_boxplot()

# iqr = q3 - q1

df_2015 %>% 
  filter(continent == "Americas") %>% 
  select(country, life_expec) %>% 
  arrange(life_expec)


## textos

df_2015 %>% 
  group_by(continent) %>% 
  summarise(life_expec_media = mean(life_expec)) %>% 
  na.omit() %>% 
  ggplot(aes(x = continent, y = life_expec_media)) +
  geom_col() +
  geom_text(aes(label = round(life_expec_media, 0)),
            vjust = 1.2)


df_2015 %>% 
  group_by(continent) %>% 
  summarise(life_expec_media = mean(life_expec)) %>% 
  na.omit() %>% 
  ggplot(aes(y = continent, x = life_expec_media)) +
  geom_col() +
  geom_text(aes(label = round(life_expec_media, 0)),
            hjust = 1.3,
            color = "red")

df_2015 %>% 
  group_by(continent) %>% 
  summarise(life_expec_media = mean(life_expec)) %>% 
  na.omit() %>% 
  ggplot(aes(x = continent, y = life_expec_media)) +
  geom_col() +
  geom_label(aes(label = round(life_expec_media, 0)),
            vjust = 1.2)


df_exemplo <- df_2015 %>% 
  filter(country %in% c("Brazil", "Argentina"))

df_exemplo


df_2015 %>% 
  ggplot(aes(x = log_gdp_per_capita, y = life_expec)) +
  geom_point() +
  geom_text(data = df_exemplo, aes(label = country))


library(ggrepel)

ggplot(data = df_2015, aes(x = log_gdp_per_capita, y = life_expec)) +
  geom_point() +
  geom_text_repel(data = df_exemplo, aes(label = country))


df_2015 %>% 
  ggplot(aes(x = continent,  y = life_expec)) +
  geom_boxplot() +
  geom_text(x = "Americas", y = 50.5, label = "Aqui é o Haiti")

# 
df_st %>% 
  filter(date >= today() - dyears(10)) %>% 
  ggplot(aes(x = date, y = ipca)) +
  geom_rect(xmin = as.Date("2016-08-31"),
            xmax = as.Date("2018-12-31"),
            ymin = -Inf,
            ymax = Inf) +
  geom_line() +
  geom_point()
  

df_st %>% 
  filter(date >= today() - dyears(10)) %>% 
  ggplot(aes(x = date, y = ipca)) +
  geom_rect(xmin = as.Date("2018-12-31"),
            xmax = Inf,
            ymin = -Inf,
            ymax = Inf,
            alpha = 0.01) +
  geom_line() +
  geom_point()


df_2015 %>% 
  filter(continent == "Americas") %>% 
  ggplot(aes(y = reorder(country, life_expec),
             x = life_expec)) +
  geom_point()

df_2015 %>% 
  count(continent)



df_feliz_completo %>% 
  filter(continent == "Americas", 
         year %in% c(2011, 2017)) %>% 
  mutate(year = as.character(year)) %>% 
  ggplot(aes(y = reorder(country, life_expec),
             x = life_expec)) +
  geom_line(aes(group = country)) +
  geom_point(aes(color = year)) 
  
### alterando as escalas dos graficos
df_2015 %>% 
  ggplot(aes(x = exp(log_gdp_per_capita),
             y = life_expec,
             color = life_ladder)) +
  geom_point()

df_2015 %>% 
  ggplot(aes(x = log(exp(log_gdp_per_capita)))) +
  geom_histogram()


### SINTAXE DAS FUNÇÕES DE ESCALA:
# scale_AESTHETIC_TIPO DO AESTHETIC
scale_

library(scales)

df_2015 %>% 
  #filter(continent == "Europe") %>% 
  ggplot(aes(x = exp(log_gdp_per_capita),
             y = life_expec,
             color = life_ladder)) +
  geom_point() +
  scale_y_continuous(name = "Expectativa de vida",
                     #breaks = c(45, 50, 55, 60, 65, 70, 75))
                     breaks = scales::breaks_width(5),
                     minor_breaks = NULL) +
  scale_x_continuous(breaks = scales::breaks_width(10000))
                     


df_2015 %>% 
  #filter(continent == "Europe") %>% 
  ggplot(aes(x = exp(log_gdp_per_capita),
             y = life_expec,
             color = life_ladder)) +
  geom_point() +
  scale_y_continuous(name = "Expectativa de vida",
                     #breaks = c(45, 50, 55, 60, 65, 70, 75))
                     breaks = scales::breaks_width(5),
                     minor_breaks = NULL) +
  scale_x_log10()





df_2015 %>% 
  #filter(continent == "Europe") %>% 
  ggplot(aes(x = log_gdp_per_capita,
             y = life_expec,
             color = life_ladder)) +
  geom_point() +
  scale_color_distiller(
    palette = "YlGn",
    direction = 1
  )


?scale_color_distiller

# escala de cores para variavel categorica
df_2015 %>% 
  #filter(continent == "Europe") %>% 
  ggplot(aes(x = log_gdp_per_capita,
             y = life_expec,
             color = continent)) +
  geom_point() +
  scale_color_brewer(
    palette = "Dark2"
  )

# alterando escala de eixo de datas

df_st_long %>%
  filter(date >= today() - dyears(10)) %>% 
  ggplot(aes(x = date, y = taxa, color = indicador)) +
  geom_line() +
  geom_point(aes(shape = indicador)) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  )

df_st_long %>%
  filter(date >= today() - dyears(2)) %>% 
  ggplot(aes(x = date, y = taxa, color = indicador)) +
  geom_line() +
  geom_point(aes(shape = indicador)) +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%Y-%m",
    minor_breaks = NULL
  )



#### facets

df_2015 %>% 
  filter(!is.na(continent)) %>% 
  #filter(continent == "Europe") %>% 
  ggplot(aes(x = log_gdp_per_capita,
             y = life_expec)) +
  geom_point() +
  facet_wrap(vars(continent), scales = "fixed", nrow = 1)

df_feliz_completo %>% 
  filter(year %in% c(2016, 2017)) %>% 
  filter(!is.na(continent)) %>% 
  #filter(continent == "Europe") %>% 
  ggplot(aes(x = log_gdp_per_capita,
             y = life_expec)) +
  geom_point() +
  facet_grid(rows = vars(year), cols = vars(continent))

# mudando o tema

df_2015 %>% 
  filter(!is.na(continent)) %>% 
  #filter(continent == "Europe") %>% 
  ggplot(aes(x = log_gdp_per_capita,
             y = life_expec)) +
  geom_point() +
  facet_wrap(vars(continent), scales = "fixed", nrow = 1) +
  theme_bw()



df_st_long %>%
  filter(date >= today() - dyears(10)) %>% 
  ggplot(aes(x = date, y = taxa, color = indicador)) +
  geom_line() +
  geom_point(aes(shape = indicador)) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  theme_bw()

library(ggthemes)


df_st_long %>%
  filter(date >= today() - dyears(10)) %>% 
  ggplot(aes(x = date, y = taxa, color = indicador)) +
  geom_line() +
  geom_point(aes(shape = indicador)) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  theme_bw()

?labs



df_st_long %>%
  filter(date >= today() - dyears(10)) %>% 
  ggplot(aes(x = date, y = taxa, color = indicador)) +
  geom_line() +
  geom_point(aes(shape = indicador)) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  theme_bw() +
  labs(y = "Taxa",
       x = NULL,
       title = "Indicadores macroeconômicos no Brasil",
       subtitle = "Desde 2021 a inflação tem aumentado bastante",
       color = NULL,
       shape = NULL,
       caption = "Tema usado: bw")


df_2015 %>% 
  ggplot(aes(x = log_gdp_per_capita,
             y = life_expec,
             color = continent,
             text = country)) +
  geom_point()

library(plotly)
ggplotly(tooltip = "text")



df_2015 %>% 
  ggplot(aes(x = log_gdp_per_capita,
             y = life_expec,
             color = continent,
             text = str_c(
               "País: ", country, "\n", 
               "População :", population
             )
             )) +
  geom_point()

ggplotly(tooltip = "text")






