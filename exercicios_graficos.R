library(tidyverse)

herois <- read_csv("dados/herois_completo.csv") %>% 
  filter(weight > 0, height > 0)

# Crie um histograma da variável altura.
herois %>% 
  ggplot(aes(x = height)) +
  geom_histogram()
ggsave("meu_histograma2.png")


herois %>% 
  ggplot(aes(x = weight)) +
  geom_histogram()

herois <- herois %>% 
  mutate(peso_kg = weight * 0.45,
         altura_m = height / 100)

herois %>% 
  ggplot(aes(x = altura_m)) +
  geom_histogram()

herois %>% 
  filter(altura_m <= 2.2, altura_m >= 1.4) %>% 
  ggplot(aes(x = altura_m)) +
  geom_histogram(binwidth = 0.05,
                 boundary = 0,
                 color = "black") +
  scale_x_continuous(breaks = scales::breaks_width(0.2))


# Analise a distribuição da variável peso em função da editora dos heróis.
herois %>% 
  ggplot(aes(x = publisher2, y = peso_kg)) +
  geom_boxplot()


herois %>% 
  filter(peso_kg <= 200) %>% 
  ggplot(aes(x = publisher2, y = peso_kg)) +
  geom_boxplot()

# Crie um gŕafico de barras mostrando a quantidade de heróis por editora
herois %>% 
  count(publisher2) %>% 
  ggplot(aes(x = fct_reorder(publisher2, n, .desc = TRUE),
             y = n)) +
  geom_col() +
  geom_text(aes(label  = n), 
            vjust = c(1.3, 1.3, -0.3))

# Crie um gráfico de barras mostrando a quantidade de herois bons, maus ou neutros 
# (variável alignment) por editora
herois %>% 
  count(publisher2, alignment) %>% 
  na.omit() %>% 
  ggplot(aes(x = publisher2, fill = alignment, y = n)) +
  geom_col(position = position_dodge())


herois %>% 
  count(publisher2, alignment) %>% 
  na.omit() %>% 
  group_by(publisher2) %>% 
  mutate(proporcao = n /sum(n)) %>% 
  ggplot(aes(x = publisher2, y = proporcao, fill = alignment)) +
  geom_col(position = position_dodge())

herois %>% 
  count(publisher2, alignment) %>% 
  na.omit() %>% 
  ggplot(aes(x = publisher2, y = n, fill = alignment)) +
  geom_col(position = position_fill()) +
  scale_y_continuous(breaks = scales::breaks_width(0.10))

hero_agg <- herois %>% 
  pivot_longer(cols = agility:omniscient,
               names_to = "nome_poder",
               values_to = "tem_poder") %>% 
  group_by(publisher2, name) %>% 
  summarise(qtd_poderes = sum(tem_poder))

hero_agg <- hero_agg %>% 
  mutate(name = ifelse(name == "Captain Marvel" & publisher2 == "DC",
                       "Captain Marvel (DC)",
                       name))

hero_agg$name[hero_agg$publisher2 == "Marvel" & hero_agg$name == "Captain Marvel"] <- "Captian Marvel (Marvel)"

hero_agg %>% 
  group_by(publisher2) %>% 
  slice_max(order_by = qtd_poderes, n = 10, with_ties = FALSE) %>% 
  ggplot(aes(x = qtd_poderes, 
             y = fct_reorder(name, qtd_poderes))) +
  geom_col() +
  facet_wrap(vars(publisher2), scales = "free_y")


# Faça um gráfico de densidade da distribuição da quantidade de poderes dos herois por editora
hero_agg %>% 
  ggplot(aes(x = qtd_poderes, fill = publisher2)) +
  geom_density(alpha = 0.5)

hero_agg %>% 
  ggplot(aes(x = publisher2, y = qtd_poderes)) +
  geom_boxplot()








