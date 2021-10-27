a <- 'texto 1'
b <- 'texto 2'
c <- 'texto 3'

a
b
c

c(a, b, c)

str_c(a, b, c, sep = ", ")
?str_c

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