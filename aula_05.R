library(tidyverse)

# tidyr

table1
table2
table3

relig_income
?relig_income

# religion | faixa_renda | qtd_pessoas

rowSums(relig_income[1, -1])

relig_income[1, -1]/rowSums(relig_income[1, -1])

relig_income_prop <- relig_income[, -1]/rowSums(relig_income[, -1])

which.max(relig_income_prop[, 1])

relig_income$religion[which.max(relig_income_prop[, 1])]

relig_income

?pivot_longer

# pivot_longer

relig_income_long <- relig_income %>% 
  pivot_longer(
    #cols = `<$10k`:`Don't know/refused`,
    cols = -religion,
    names_to = "faixa_renda",
    values_to = "qtd_pessoas"
  )

relig_income_long %>% 
  #group_by(religion) %>%
  mutate(total_por_religiao = sum(qtd_pessoas))



relig_income_long %>% 
  group_by(religion) %>% 
  mutate(total_por_religiao =  sum(qtd_pessoas)) %>% 
  mutate(proporcao_na_faixa_renda = qtd_pessoas/total_por_religiao) %>% 
  filter(faixa_renda == "<$10k") %>% 
  arrange((proporcao_na_faixa_renda))

relig_income_long %>% 
  group_by(religion) %>% 
  mutate(proporcao_na_faixa_renda = qtd_pessoas/sum(qtd_pessoas)) %>% 
  filter(faixa_renda == "<$10k") %>% 
  arrange((proporcao_na_faixa_renda))


relig_income_long %>% 
  group_by(religion) %>% 
  mutate(total_por_religiao =  sum(qtd_pessoas),
         proporcao_na_faixa_renda = qtd_pessoas/total_por_religiao) %>%
  filter(faixa_renda == "<$10k") %>% 
  arrange((proporcao_na_faixa_renda)) %>% 
  mutate(porcentual_na_faixa_renda = round(100 * proporcao_na_faixa_renda, 1))



?us_rent_income


us_rent_income %>% 
  filter(variable == "income") %>% 
  arrange(desc(estimate))


#GEOID | NAME | variable | estimate
# em
# GEOID | NAME | income | rent

us_rent_income %>% 
  select(-moe) %>% 
  pivot_wider(names_from = variable,
              values_from = estimate) %>% 
  mutate(peso = rent/income) %>% 
  arrange(desc(peso))


us_rent_income %>% 
  #select(-moe) %>% 
  pivot_wider(names_from = variable,
              values_from = c(estimate, moe)) 


### separate
table3
table1

?separate

table3 %>% 
  separate(col = rate,
           into = c("casos", "populacao"),
           sep = "/",
           remove = FALSE,
           convert = TRUE)

df_exemplo <- data.frame(
  telefone = c(11958471231, 51984500023, 79123456890)
)

df_exemplo

df_exemplo %>% 
  separate(col = telefone,
           into = c("ddd", "telefone"),
           sep = 2)


df_exemplo_2 <- data.frame(
  telefone = c("5511958471231", "5551984500023", "5579123456890")
)

df_exemplo_2 %>% 
  separate(col = telefone,
           into = c("ddi", "ddd", "telefone"),
           sep = c(2, 4))

df_exemplo_3 <- tibble(
  data = c("01-09-1993", "20-04-2015", "12-2021")
)

df_exemplo_3

df_exemplo_3 %>% 
  separate(col = data,
           into = c("dia", "mes", "ano"),
           sep = "-",
           fill = "left")

table5
?unite

table5 %>% 
  unite(col = ano,
        century, year,
        sep = "")



df_exemplo_3 %>% 
  separate(col = data,
           into = c("dia", "mes", "ano"))

exemplo <- tibble(grupo = c("a", "a", "b","b"),
                  y = c("1, 2", "3;4", "1,2,3", "4"))

exemplo

?separate_rows


exemplo %>% 
  separate_rows(y,
                sep = ",|;") %>% 
  mutate(y = str_remove_all(y, " "))


# Transforme a table1 para a table2 usando pivot_longer()
table1
table2

table1
table2

table1 %>% 
  pivot_longer(cols = c(cases, population),
               names_to = "type2",
               values_to = "count2")

# Transforme a table2 para a table1 usando pivot_wider()
table2
table1

table2 %>% 
  pivot_wider(names_from = type,
              values_from = count)

# Transforme a table5 para a table1 e para a table2
table5
table1
table2

table5 %>% 
  unite(col = year2,
        century, year,
        sep = "") %>% 
  separate(col = rate,
           into = c("cases2", "population2"))



table5 %>% 
  unite(col = year2,
        century, year,
        sep = "") %>% 
  separate(col = rate,
           into = c("cases2", "population2")) %>% 
  pivot_longer(cols = c(cases2, population2),
               names_to = "type2",
               values_to = "count2")


#### capitulo 06


dados2016 <- data.frame(ano = c(2016, 2016, 2016), 
                        valor = c(938, 113, 1748), 
                        produto = c('A', 'B', 'C'))

dados2017 <- data.frame(valor = c(8400, 837, 10983), 
                        produto = c('H', 'Z', 'X'),
                        ano = c(2017, 2017, 2017))


dados2016
dados2017

bind_rows(dados2016, dados2017)


dados2018 <- data.frame(valor = c(8400, 837, 10983), 
                        produto = c('H', 'Z', 'X'),
                        ano = c(2018, 2018, 2018),
                        empresa = c("E1", "E2", "E3"))

dados2018

bind_rows(dados2016, dados2017, dados2018)

### funcoes join
band_members
band_instruments
band_instruments2

left_join(band_members, band_instruments, by = "name")
left_join(band_members, band_instruments2, by = c("name" = "artist"))


left_join(band_instruments, band_members, by = "name")



inner_join(band_members, band_instruments, by = "name")


full_join(band_members, band_instruments, by = "name")


library(nycflights13)
flights
airports



