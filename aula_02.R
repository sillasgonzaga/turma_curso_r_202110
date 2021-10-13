library(tidyverse)



df <- readr::read_csv2("dados/Ano-2021.csv")

#readr::read_csv2()

glimpse(df)
df

x = 1

x

x + 5

nome <- "Sillas"
nome

nome + 5

ano <- "2021"

ano + "5"


class(ano)

class(x)

class(df)

logico1 <- TRUE
logico2 <- FALSE


class(logico1)


nome1 <- "Sillas"
nome2 <- "Camila"
nome3 <- "Glaudemias"

nomes <- c("Sillas", "Camila", "Glaudemias", "Wanessa")

class(nomes)

altura <- c(1.78, 1.70, 1.85, 1.64)
class(altura)

usa_oculos <- c(FALSE, TRUE, TRUE, FALSE)
class(usa_oculos)
  
df_turma <- data.frame(nomes, altura, usa_oculos)
df_turma
glimpse(df_turma)


ano

as.numeric(ano) + 10

class(as.numeric(ano))

altura

x
x  + 10

as.character(x) + 10

# operações com vetores
altura * 100
altura_cm <- altura * 100

altura -  5

misturado <- c("texto", 2021, FALSE)

misturado
class(misturado)


misturado2 <- c("palavra", 1993)
class(misturado2)

as.character(1993)

as.numeric("palavra")


as.numeric("578")

misturado3 <- c(100, FALSE)
misturado3
class(misturado3)

7 + TRUE
as.logical(100)
as.logical(1)
as.logical(0)


misturado2

as.numeric(misturado2)


idade <- c(28, 35, NA, 25)
df_turma <- data.frame(nomes, altura, usa_oculos, idade)
df_turma


is.na(idade)

# testes logicos de comparação
# > | >= 
# < | <=
# ==
# !=
# %in%
2000 < 100
x
x == 1
x == 10
x != 10
"Elias" %in% nomes
"Wanessa" %in% nomes
"SILLAS" %in% nomes
"SILLAS" == "Sillas"

aluno <- c("Glaudemias")

if (aluno %in% nomes){
  # se TRUE:
  print("Boas-vindas")
} else{
  # se FALSE:
  print("Matricule-se na próxima turma")
}


# |
# &

numero <- 500

if(numero > 10000){
  resultado <- "VALOR ALTO"
} else if (numero <= 10000 & numero > 1000){
  resultado <- "VALOR MEDIO"
} else {
  resultado <- "VALOR BAIXO"
}

10 > 8 & 4 > 1 # TRUE & TRUE == TRUE
10 > 8 & 4 > 5 # TRUE & FALSE == FALSE
4 > 5 & 10 > 8 # FALSE & TRUE == FALSE


10 > 8 | 4 > 5 # TRUE | FALSE == TRUE

#### for loop

# SINTAXE:
# for (i in VETOR_QUALQUER)

altura

for (i in c(-10, 2, 0, 4, 5, 10)){
  print("Valor da iteração: ")
  print(i)
  print(i * 15)
  print('---------- Proximo i -------')
}


altura * 100

altura

# aplicando funções a alturas
sqrt(altura)

mean(altura)

sum(altura)

f_para_c <- function(x){
  
  if (is.numeric(x)){
    output <- (x - 32) / 1.8
    return(output)
  } else{
    stop("O input não é numérico")
  }
  
  
  # ou:
  # return((x - 32) / 1.8)
  
  # ou:
  # (x - 32) / 1.8
  
}


f_para_c(67)

f_para_c("67")


c_para_f <- function(x, digitos = 5){
  resultado <- x * 1.8 + 32
  round(resultado, digitos)
}


c_para_f(19.1111)

NULL

converter_temperatura <- function(c = NULL, f = NULL){
  #print(c)
  #print(f)
  
  if (!is.null(c)){
    print("Convertendo para F:")
    c_para_f(c)
  } else {
    print("Convertendo para C:")
    f_para_c(f)
  }
  
}

converter_temperatura(c = 5)
converter_temperatura(f = 10)
converter_temperatura(f = 10, c  = 8)

!(5 > 1)


### mexendo com vetores

nomes
length(nomes)
# usando indice numerico
nomes[1]
nomes[3]
nomes[c(2, 4)]


# usando vetores booleanos
altura > 1.70
altura[altura > 1.70]

nomes[altura <= 1.70]

# sintaxe de [] em dataframes:
# DATAFRAME[linha, coluna]

df[915, 20]
df[915, ]
df[, 20]

df$sgPartido

unique(df$sgPartido)

1:15

df[1:15, 30]
df[1:15, 25:30]






