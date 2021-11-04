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






