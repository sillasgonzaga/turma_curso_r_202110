library(tidyverse)


df <- readr::read_csv("https://raw.githubusercontent.com/sillasgonzaga/curso_series_temporais/master/data/Bike-Sharing-Dataset/day.csv")

glimpse(df)


df_transf <- df %>% 
  # remover colunas irrelevantes
  select(-c(instant, workingday)) %>% 
  # renomear algumas colunas
  rename(
    estacao = season,
    total = cnt,
    year = yr, 
    month = mnth
  ) %>% 
  # mudar weekday, que começa a contar do zero
  mutate(weekday = weekday + 1) %>% 
  # transformar a variavel de feriado para texto
  mutate(holiday = as.character(holiday)) %>% 
  # mudar os valores de algumas variaveis
  mutate(
    # substituir o codigo do ano  pelo ano real
    year = lubridate::year(dteday),
    # adicionar um leading zero no mês
    month = str_pad(month, width = 2, side = "left", pad = "0"),
    # converter weathersit para variavel do tipo factor
    weathersit = factor(weathersit,
                        levels = 1:4,
                        labels = c("muito bom", "bom", "ruim", "muito ruim")),
    # converter dia da semana para variavel do tipo factor
    weekday = factor(weekday, 
                     levels = 1:7,
                     labels = c("Dom", "Seg", "Ter", "Qua", "Qui", "Sex", "Sab")),
    # fazer o mesmo para estacao
    estacao = factor(estacao, 
                     levels = 1:4,
                     labels = c("Primavera", "Verao", "Outono", "Inverno")),
    # converter colunas numericas para escala normal (não-normalizada)
    temp = temp * 41,
    atemp = atemp * 50,
    hum = hum * 100,
    windspeed = windspeed * 67
  )

glimpse(df_transf)


mes <- 1:12
mes
sort(mes)
rev(sort(mes))

mes_chr <- as.character(mes)
mes_chr
sort(mes_chr)

mes_chr <- str_pad(mes, width = 2, pad = "0")
mes_chr
sort(mes_chr)


df_transf %>% 
  ggplot(aes(x = dteday, y = total)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%m/%y") +
  theme(axis.text.x = element_text(angle = 90))


df_transf %>% 
  ggplot(aes(x = dteday, y = total)) +
  geom_line() +
  geom_smooth() +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%m/%y")

df_mudancas_estacao <- df_transf %>% 
  select(dteday, estacao) %>% 
  mutate(estacao_dia_anterior = lag(estacao)) %>% 
  filter(estacao != estacao_dia_anterior)

df_mudancas_estacao



df_transf %>% 
  ggplot(aes(x = dteday, y = total)) +
  geom_line() +
  geom_smooth() +
  geom_vline(data = df_mudancas_estacao,
             aes(xintercept = dteday,
                 color = estacao),
             linetype = "dashed") +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%m/%y")


# dia da semana vs bikes alugadas
df_transf %>% 
  ggplot(aes(x = weekday, y = total)) +
  geom_boxplot()

df_transf %>% 
  ggplot(aes(x = weekday, y = casual)) +
  geom_boxplot()


df_transf %>% 
  ggplot(aes(x = weekday, y = registered)) +
  geom_boxplot()

df_transf %>% 
  ggplot(aes(x = month, y = total, fill = estacao)) +
  geom_boxplot()



df_transf %>% 
  ggplot(aes(x = estacao, y = total, fill = month)) +
  geom_boxplot()


df_transf %>% 
  ggplot(aes(x = temp, y = total)) +
  geom_point()

cor(df_transf$temp, df_transf$total)

df_transf %>% 
  select(temp, total) %>% 
  cor()

df_transf %>% 
  ggplot(aes(x = temp, y = total)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_vline(xintercept = 25, linetype = "dashed") +
  scale_y_continuous(breaks = scales::breaks_width(2000))


x1 <- -100:100
y1 <- x1^2


tibble(x1, y1) %>% 
  ggplot(aes(x = x1, y = y1)) +
  geom_point()

cor(x1, y1) %>% round(4)






