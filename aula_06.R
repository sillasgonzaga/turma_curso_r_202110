library(tidyverse)
library(nycflights13)

airports
flights

?flights

glimpse(flights)


airports %>% 
  filter(faa == "JFK")

flights %>% 
  count(origin, dest, sort = TRUE)

df_nomes_aeroportos <- airports %>% select(faa, name)

df_nomes_aeroportos

df_count_voos <- flights %>% 
  count(origin, dest, sort = TRUE)

df_count_voos
df_nomes_aeroportos

left_join(df_count_voos, df_nomes_aeroportos, by = c("origin" = "faa")) %>% 
  left_join(df_nomes_aeroportos, by = c("dest" = "faa")) %>% 
  select(origem = name.x, destino = name.y, qtd = n)


df_vendas <- tibble(
  id_vendedor = c(1, 2, 4),
  vendas = c(500, 250, 100)
)

df_vendas


df_vendedores <- tibble(
  id_vendedor = c(1, 2, 3, 4, 5),
  nome = c("Sillas", "Glaudemias", "Camila", "Amanda", "Brenna")
)
df_vendas
df_vendedores

inner_join(df_vendedores, df_vendas, by = "id_vendedor")

left_join(df_vendedores, df_vendas, by = "id_vendedor") %>% 
  mutate(vendas = replace_na(vendas, 0))

# questao 2
participantes <- data.frame(
  Nome = c('Carlos', 'Maurício', 'Ana Maria', 'Rebeca', 'Patrícia'),
  Estado = c('Brasília', 'Minas Gerais', 'Goiás', 'São Paulo', 'Ceará'),
  Idade = c(23, 24, 22, 29, 28)
)

aprovados <- data.frame(
  Nome = c('Carlos', 'Patrícia'),
  Pontuacao = c(61, 62)
)

eliminados <- data.frame(
  Nome = c('Maurício', 'Ana Maria', 'Rebeca'),
  Pontuacao = c(49, 48, 48)
)

aprovados <- aprovados %>% 
  mutate(Resultado = "Aprovado")

aprovados

eliminados <- eliminados %>% 
  mutate(Resultado = "Reprovado")

eliminados

df_resultado <- bind_rows(aprovados, eliminados)


left_join(participantes, df_resultado)


#### exercicio do kaggle
library(janitor)
#install.packages("janitor")

hero_powers <- readr::read_csv("dados/archive/super_hero_powers.csv")
hero_powers

#read_csv("dados/archive/super_hero_powers.csv")
hero_info <- readr::read_csv("dados/archive/heroes_information.csv",
                             na = c("", "NA", "-", "-99"))
hero_info

glimpse(hero_info)

hero_info %>% select(Gender)
hero_info %>% select(`Eye color`)


?janitor::clean_names()

hero_info <- clean_names(hero_info)
hero_powers <- clean_names(hero_powers)


glimpse(hero_info)


# No caso de hero_info, remova a primeira coluna.
hero_info <- hero_info %>% select(-x1)
hero_info

glimpse(hero_powers)

# questao 5
hero_info %>% distinct(publisher) %>% print(n = 500)

hero_info %>% count(publisher, sort = TRUE)

# ifelse()

hero_info %>% 
  select(publisher) %>% 
  mutate(publisher2 = ifelse(
    publisher == "Marvel Comics",
    "Marvel",
    ifelse(
      publisher == "DC Comics",
      "DC",
      "Outros"
    )
  ))

?case_when


# SINTAXE DO case_when()
# case_when(
# TESTE LOGICO 1 ~ "OUTPUT1",
# TESTE2 LOGICO 2 ~ "OUTPUT"2,
#...
# TRUE ~ "OUTPUT N"
#)

hero_info <- hero_info %>% 
  #select(publisher) %>% 
  mutate(publisher2 = case_when(
    publisher == "Marvel Comics" ~ "Marvel",
    publisher == "DC Comics" ~ "DC",
    TRUE ~ "Outros"
  ))


glimpse(hero_info)


# questao 6
hero_info %>% 
  select(publisher2, race) %>% 
  na.omit() %>% 
  count(publisher2, race) %>% 
  arrange(race) %>% 
  pivot_wider(names_from = publisher2,
              values_from = n,
              values_fill = 0) %>% 
  filter(Marvel > 0, DC == 0, Outros == 0)


racas_por_editora <- hero_info %>% 
  count(publisher2, race)

racas_por_editora

racas_geral <- hero_info %>% 
  count(race, name = "n_geral")
racas_geral


left_join(racas_por_editora, racas_geral) %>% 
  filter(n == n_geral)

# questao 7
hero_info %>% 
  select(eye_color, gender)

summary(hero_info)

hero_info
mean(is.na(hero_info$race))
mean(is.na(hero_info[["race"]]))

colnames(hero_info)



for(c in colnames(hero_info)){
  print(c)
  print(mean(is.na(hero_info[[c]])))
  print('-----')
}

map(hero_info, function(x) mean(is.na(x)))


library(naniar)
?naniar::pct_miss(hero_info)

pct_complete_var(hero_info)

miss_summary(hero_info)
prop_miss_case(hero_info)

hero_info %>% 
  select(eye_color, gender) %>% 
  na.omit() %>% 
  count(gender, eye_color, name = "qtd_personagens") %>% 
  group_by(gender) %>% 
  dplyr::slice_max(order_by  = qtd_personagens,
                   n = 3)

?dplyr::slice_max()

?slice_max

# questao 9
glimpse(hero_powers)

hero_powers %>% 
  pivot_longer(cols = -hero_names,
               names_to = "nome_poder",
               values_to = "tem_poder") %>% 
  group_by(nome_poder) %>% 
  summarise(pct_tem_poder = mean(tem_poder))


# questao 10
hero_info
hero_powers

hero <- inner_join(hero_info, hero_powers, by = c("name" = "hero_names"))

glimpse(hero)



# questao 11
hero %>% 
  select(publisher2, telepathy) %>% 
  group_by(publisher2) %>% 
  summarise(pct_telepatas = mean(telepathy))


# questao 12
hero %>% 
  select(name, publisher, flight, weight) %>% 
  filter(flight) %>% 
  arrange(desc(weight))

# questao 13
hero %>% write_csv("dados/herois_completo.csv")















