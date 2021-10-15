library(tidyverse)

df <- readr::read_csv2("dados/Ano-2021.csv")


# sintaxe de usar funções no R:

# f(x, y, z)

sqrt(70)
round(sqrt(70), 1)

# f(g(h(x))) : x -> h() -> g() -> f()

# x %>% h() %>% g() %>% f()
# atalho: ctrl shift M
70 %>% sqrt() # é equivalent a sqrt(70)
70 %>% sqrt() %>% round(1)

length(unique(df$sgPartido))
# é equivalente a:
df$sgPartido %>% unique() %>% length()
df$sgPartido |> unique() |> length()

# library(magrittr)
df

# FUNÇÕES DO DPLYR: funcao(DATAFRAME, ...) ou DATAFRAME %>% funcao(...)

df %>% select(txNomeParlamentar, cpf, sgPartido, vlrDocumento)
df

df %>% select(-ideCadastro)
# remover mais de uma coluna
df %>% select(-c(txNomeParlamentar, cpf, nuLegislatura))

df %>% select(starts_with("vlr"))
df %>% select(-starts_with("vlr"))

df %>% select_if(is.numeric)
# df %>% select(across(.fns = is.numeric))
nomes <- c("Sillas", "DAVI", "fuLANO")
tolower(nomes)

df %>% select_all(tolower)

# mutate(): modificar ou criar colunas no seu dataframe
glimpse(df)

df$vlrDocumento / 2

# sintaxe de mutate:
# mutate(DATAFRAME, <NOME DO OUTPUT> = <OUTPUT>)
df %>%
  mutate(vlrDocumento = as.numeric(vlrDocumento)) %>%
  select(vlrDocumento)
  
word("Meu nome é Sillas")

df %>% 
  mutate(primeiro_nome_parlamentar = word(txNomeParlamentar)) %>% 
  select(txNomeParlamentar, primeiro_nome_parlamentar)

df_limpo <- df %>% 
  mutate(vlrDocumento = as.numeric(vlrDocumento))

# FILTER: DATAFRAME %>%  filter(TESTE LOGICO)

df_limpo %>% filter(vlrDocumento > 50000) %>% 
  select(txNomeParlamentar, sgPartido, vlrDocumento)


df_limpo %>% 
  filter(!is.na(cpf))


df_limpo %>% 
  filter(sgPartido == "NOVO", vlrDocumento > 3000)


df_limpo %>% 
  filter(sgUF == "SP" | sgUF == "RJ" | sgUF == "MG")

6 %in% c(4, 5, 6)

df_limpo %>% filter(sgUF %in% c("SP", "RJ", "MG"))

glimpse(df_limpo)

df_limpo %>% filter(numMes == 9 | sgPartido == "PT")

# group by
df_limpo %>% 
  filter(!is.na(cpf)) %>% 
  select(txtFornecedor, txNomeParlamentar, vlrDocumento, sgPartido) %>% 
  group_by(txtFornecedor) %>% 
  summarise(valor_total = sum(vlrDocumento)) %>% 
  filter(txtFornecedor == "COMPANHIA ENERGÉTICA DO CEARÁ")
  

df_limpo %>% 
  filter(!is.na(cpf)) %>% 
  #select(txtFornecedor, txNomeParlamentar, vlrDocumento, sgPartido) %>% 
  group_by(txtCNPJCPF, txtFornecedor) %>% 
  summarise(valor_total = sum(vlrDocumento)) %>% 
  arrange(desc(valor_total))

df_limpo %>% 
  filter(!is.na(cpf)) %>% 
  #select(txtFornecedor, txNomeParlamentar, vlrDocumento, sgPartido) %>% 
  group_by(txtCNPJCPF, txtFornecedor, sgPartido) %>% 
  summarise(valor_total = sum(vlrDocumento)) %>% 
  arrange(desc(valor_total))

df_limpo %>% 
  filter(!is.na(cpf), txtFornecedor %in% c("TAM", "GOL")) %>% 
  group_by(sgPartido) %>% 
  summarise(valor_total = sum(vlrDocumento),
            valor_medio = mean(vlrDocumento),
            qtd_deputados = n_distinct(cpf),
            qtd_despesas = n()) %>% 
  mutate(valor_por_dep = valor_total / qtd_deputados) %>% 
  arrange(valor_por_dep) %>% 
  print(n = 1000)


# Quais foram os deputados com mais despesas na cota parlamentar
# (considerando a coluna vlrDocumento)?

df_limpo %>% 
  filter(!is.na(cpf)) %>% 
  group_by(cpf, txNomeParlamentar, sgPartido, sgUF) %>% 
  summarise(valor_total = sum(vlrDocumento)) %>% 
  arrange(desc(valor_total)) %>% 
  write_csv2("despesas por deputado.csv")

# Quais foram as 5 empresas mais contratadas (considerando a coluna textCNPJCPF)?


#write_*(dataframe, nome do arquivo.csv)
df_limpo %>% 
  group_by(txtCNPJCPF, txtFornecedor) %>% 
  summarise(soma_vlr_document = sum(vlrDocumento)) %>% 
  arrange(desc(soma_vlr_document)) %>% 
  head(5)


df_limpo %>% 
  filter(!is.na(cpf)) %>% 
  group_by(cpf, txNomeParlamentar, sgPartido, sgUF) %>% 
  summarise(valor_medio = mean(vlrDocumento))



df_limpo %>% 
  filter(!is.na(cpf)) %>%
  group_by(cpf, txNomeParlamentar, sgPartido, sgUF) %>% 
  summarise(valor_total = sum(vlrDocumento)) %>% 
  arrange(desc(valor_total))

# Quais categorias de gastos registraram mais despesas nas lideranças
# (considerando a coluna txtDescricao)?

df_limpo %>% 
  filter(is.na(cpf)) %>% 
  group_by(txtDescricao) %>% 
  summarise(valor_total_despesas = sum(vlrDocumento),
            qtd_despesas = n())


# Quantas linhas da coluna com o PDF da nota fiscal estão com NA
# (considerando a coluna urlDocumento)?
df_limpo %>% 
  select(urlDocumento) %>% 
  summarise(qtd_sem_pdf = sum(is.na(urlDocumento)),
            pct_sem_pdf = mean(is.na(urlDocumento)))


sequencia <- 1:20
sum(sequencia > 14)

vetor <- c(1, NA, 3, NA, NA, NA, 4)
vetor
vetor %>% is.na() %>% mean()


# Qual foi o mês com menos gastos (considerando a coluna numMes)?

df_limpo %>% 
  group_by(numMes) %>% 
  summarise(valor_total_despesas = sum(vlrDocumento),
            qtd_despesas = n()) %>% 
  arrange(valor_total_despesas)






