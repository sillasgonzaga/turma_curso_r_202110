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
                     labels = c("Inverno", "Primavera", "Verao", "Outono")),
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
               date_labels = "%m/%y") +
  scale_y_continuous(breaks = scales::breaks_width(1000))


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


# correlação ads variaveis numericas
df_transf %>% 
  select_if(is.numeric) %>% 
  select(-c(year, casual, registered, atemp)) %>% 
  cor()

df_transf %>% 
  ggplot(aes(x = hum, y = total)) +
  geom_point()

# criando um modelo de regressão

# SINTAXE: lm(VARIAVEL RESPOSTA Y ~ VARIAVEL PREDITORA X1, data = DATA FRAME)
# SINTAXE: lm(VARIAVEL RESPOSTA Y ~ VARIAVEL PREDITORA X1 + X2 + X3 ...., data = DATA FRAME)
# SINTAXE: lm(VARIAVEL RESPOSTA Y ~ ., data = DATA FRAME)

# simples
mod1 <- lm(total ~ temp, data = df_transf)

mod1

# BIKES = 1215 + 162 * TEMP

df_transf %>% 
  ggplot(aes(x = temp, y = total)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(breaks = scales::breaks_width(1000))

mod2 <- lm(total ~ temp + estacao, data = df_transf)
mod2


model.matrix(total ~ temp + estacao, data = df_transf) %>% 
  head()


summary(mod1)
summary(mod2)

df_aleatorio <- as_tibble(matrix(rnorm(731 * 100), nrow = 731))

df_mod_aleatorio <- df_transf %>% 
  select(total, temp) %>% 
  bind_cols(df_aleatorio)

mod3 <- lm(total ~ ., data = df_mod_aleatorio)

summary(mod3)

library(broom)
glance(mod3)
glance(mod1)

summary(mod1)
summary(mod2)

df_mod_3 <- df_transf %>% 
  select(-c(dteday, casual, registered, atemp))

mod3 <- lm(total ~ ., data = df_mod_3)

options(scipen = 999)
summary(mod3)


# BIKES = 1215 + 162 * TEMP
1215 + 162 * 30.1 # 6091

df_transf %>% filter(round(temp) == 30)


# y = 4098
# y_hat = 6091

4098 - 6091

mean(residuals(mod3))

library(forecast)
checkresiduals(mod3)


df_transf %>% 
  View( )

df_mod_4 <- df_transf %>% 
  mutate(teve_furacao = ifelse(dteday == as.Date("2012-10-29"), "1", "0")) %>% 
  select(-c(dteday, casual, registered, atemp))

df_mod_4

mod4 <- lm(total ~ ., data = df_mod_4)

summary(mod4)
glance(mod3)



# importar arquivos referentes ao ponto de entrada
ponto_entrada <- readr::read_csv("dados/hr/in_time.csv")

# mudar formato do dataset para o formato tidy (long)
ponto_entrada <- ponto_entrada %>% 
  rename(employee = X1) %>% 
  mutate_at(vars(-employee), as.character) %>% 
  gather(data, horario_entrada, -employee) %>% 
  arrange(employee, data) 

ponto_entrada <- ponto_entrada %>% 
  mutate(horario_entrada = as.POSIXct(horario_entrada))

# fazer o mesmo com o ponto de saída
ponto_saida <- readr::read_csv("dados/hr/out_time.csv") %>% 
  rename(employee = X1) %>% 
  mutate_at(vars(-employee), as.character) %>% 
  gather(data, horario_saida, -employee) %>% 
  arrange(employee, data) %>% 
  mutate(horario_saida = as.POSIXct(horario_saida))

# juntar os dois datasets para ter o ponto do dia
ponto <- left_join(ponto_entrada, ponto_saida,
                   by = c("employee", "data"))

ponto <- ponto %>% 
  mutate(tempo_trabalho = difftime(horario_saida,
                                   horario_entrada,
                                   units = "hours")) %>% 
  mutate(tempo_trabalho = as.numeric(tempo_trabalho)) %>% 
  filter(!is.na(tempo_trabalho))

# calcular o tempo medio no trabalho por dia dos funcionarios
ponto_agg_ano <- ponto %>% 
  group_by(employee) %>% 
  summarise(
    tempo_medio_trabalho = mean(tempo_trabalho)
  )


emp_pesquisa <- readr::read_csv("dados/hr/employee_survey_data.csv")
gestor_pesquisa <- readr::read_csv("dados/hr/manager_survey_data.csv")
geral <- readr::read_csv("dados/hr/general_data.csv")

# criar um dataset unico
rh <- left_join(geral, emp_pesquisa, by = "EmployeeID") %>% 
  left_join(gestor_pesquisa, by = "EmployeeID") %>% 
  left_join(ponto_agg_ano, by = c("EmployeeID" = "employee")) %>% 
  na.omit() %>% 
  mutate_if(is.character, as.factor)

# salvar os dados
write_rds(rh, "dados/exemplo_classificacao_rh.rds")

rh <- read_rds("https://github.com/sillasgonzaga/livro_intro_r/blob/master/dados/exemplo_classificacao_rh.rds?raw=true")

rh

glimpse(rh)


rh %>% 
  ggplot(aes(x = tempo_medio_trabalho, y = Attrition)) +
  geom_boxplot()

rh

n <- nrow(rh)
n


ind_treino <- sample(1:n, size = n*0.7)
length(ind_treino)

rh_treino <- rh[ind_treino, ]
rh_treino

rh_teste <- rh[-ind_treino, ]

rh_teste

library(rpart)
mod_arvore <- rpart(Attrition ~ . - EmployeeID, data = rh_treino)

mod_arvore

rpart.plot::prp(mod_arvore,
                type = 4,
                extra = 6,
                fallen.leaves = FALSE, 
                varlen = 0,
                faclen = 0, box.palette = "auto")





rh_treino %>% count(Attrition)
2530/(2530 + 480)


# inicializar pdf em branco
pdf("minha_primeira_arvore.pdf")
# preencher pdf com o grafico da arvore
rpart.plot::prp(mod_arvore, type = 4, extra = 6, fallen.leaves = FALSE, varlen = 0,
                faclen = 0, box.palette = "auto")
# destravar a sessao para o pdf ser salvo
dev.off()


ycast <- predict(mod_arvore, newdata = rh_teste, type = "class")

ycast
yreal <- rh_teste$Attrition

caret::confusionMatrix(data = ycast, 
                       reference = yreal, 
                       positive = "Yes",
                       mode = "everything")




