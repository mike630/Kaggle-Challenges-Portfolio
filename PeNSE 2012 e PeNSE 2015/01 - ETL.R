# ETL

# Irei fazer o processo de ETL por programação em R, pois o Integration Service e o MSSQL Server da minha 
# máquina não estão funcionando corretamente. Utilizarei biblioteca no R para tratar os dados em SQL para não
# fugir muito do escopo do que se pede no business case.

# Selecionando diretório
setwd('C:\\Users\\mcn63\\Downloads\\Albert Einstein')

#------------------------------------------------------------------------------------------------------------

# Carregando os arquivos em cada variável

# Carregando PENSE_AMOSTRA1_ALUNO.csvdo PeNSE 2015
a <- read.csv('PENSE_AMOSTRA1_ALUNO.csv', sep = ';', na.strings = c(' ', ''))
# Não havia arquivo de extensão .txt conforme pedido no case, então estou carregando o disponibilizado
# pelo site.

# Carregando PENSE_AMOSTRA2_ALUNO.csv do PeNSE 2015
b <- read.csv('PENSE_AMOSTRA2_ALUNO.csv', sep = ';', na.strings = c(' ', ''))
# Não havia arquivo de extensão .txt conforme pedido no case, então estou carregando o disponibilizado
# pelo site.

# Carregando arquivos PENSE_AMOSTRA#_ESCOLA.csv disponibilizados nos microdados do site para trabalhar
# com a variável Dependência Administrativa da escola - V0007 ( Público ou Privada). Há esta variável
# no arquivo de alunos do PeNSE 2012, mas não no arquivo alunos do PeNSE 2015.

escola1 <- read.csv('PENSE_AMOSTRA1_ESCOLA.csv', sep = ';', na.strings = c(' ', ''))

escola2 <- read.csv('PENSE_AMOSTRA2_ESCOLA.csv', sep = ';', na.strings = c(' ', ''))

# Carregando estudantes.csv do PeNSE 2012
c <- read.csv('estudantes.csv', sep = ';', na.strings = c(' ', ''))
# Tive que converter o arquivo .xlsx em .csv para uma melhor performance em carregar estes dados em memória.
# O arquivo de extensão .txt não vem com o header e nem delimitado de forma correta.

#------------------------------------------------------------------------------------------------------------

# Manipulação de dados

# Padronizando PeNSE 2012 e PeNSE 2015

c$PUBPRIV <- ifelse(c$PUBPRIV == 1, 2, 1)
# No arquivo PeNSE 2015, a variável V0007 atribui 1 para 'Pública' e 2 para 'Privada', enquanto o PeNSE 2012 é o 
# inverso. Por isso, estou convertendo para padronização.

c$B01003 <- ifelse(c$B01003 == 1, 11, ifelse(c$B01003 ==2, 12, ifelse(c$B01003 == 3, 13, ifelse(c$B01003 == 4, 14,
                    ifelse(c$B01003 == 5, 15, ifelse(c$B01003 == 6, 16, ifelse(c$B01003 == 7, 17, ifelse(c$B01003 == 8, 18,
                    ifelse(c$B01003 == 9, 19, c$B01003)))))))))

c$B08002 <- ifelse(c$B08002 == 2, 9, ifelse(c$B08002 == 3, 10, ifelse(c$B08002 == 4, 11, ifelse(c$B08002 == 5, 12,
                    ifelse(c$B08002 == 6, 13, ifelse(c$B08002 == 7, 14, ifelse(c$B08002 == 8, 15, ifelse(c$B08002 == 9, 16, 1))))))))

a$VB08002 <- ifelse(a$VB08002 == 17, 16, ifelse(a$VB08002 == 18, 16, a$VB08002))

b$VB08002 <- ifelse(b$VB08002 == 17, 16, ifelse(b$VB08002 == 18, 16, b$VB08002))

c$B08003 <- ifelse(c$B08003 == 1, 0, ifelse(c$B08003 == 2, 1, ifelse(c$B08003 == 3, 2, ifelse(c$B08003 == 4, 3,
                    ifelse(c$B08003 == 5, 4, ifelse(c$B08003 == 6, 5, ifelse(c$B08003 == 7, 6, c$B08003)))))))

c$B08005 <- ifelse(c$B08005 == 1, 0, ifelse(c$B08005 == 2, 1, ifelse(c$B08005 == 3, 2, ifelse(c$B08005 == 4, 3, c$B08005))))

#------------------------------------------------------------------------------------------------------------

# Select e join em SQL

install.packages('sqldf')
library(sqldf)

a <- sqldf('SELECT a.ANOPESQ,a.REGEOGR, a.VB01001, a.VB01002, a.VB01003, a.VB08001, a.VB08002, a.VB08003A,
                   a.VB08005, a.VB08008, a.VB08009 , escola1.V0007
            FROM a 
            JOIN escola1
            ON a.aluno = escola1.aluno')

b <- sqldf('SELECT b.ANOPESQ,b.REGEOGR, b.VB01001, b.VB01002, b.VB01003, b.VB08001, b.VB08002, b.VB08003A,
                   b.VB08005, b.VB08008, b.VB08009 , escola2.V0007
            FROM b 
            JOIN escola2
            ON b.aluno = escola2.aluno')

# Colocando variável do ano de pesquisa (ANOPESQ) no PeNSE 2012
c$ANOPESQ <- 2012

c <- sqldf('SELECT ANOPESQ, REGIAO, B01001, B01002, B01003, B08001, B08002, B08003, B08005, B08008,
           B08009, PUBPRIV
           FROM c')

# Juntando as três variáveis acima em apeans uma
colnames(c) <- colnames(a)

final  <- rbind(a,b,c)

#------------------------------------------------------------------------------------------------------------

# Renomeando os dados em SQL para melhor visualização dos insights

regiao <- sqldf('SELECT CASE WHEN REGEOGR = 1 THEN "Norte"
                             WHEN REGEOGR = 2 THEN "Nordeste"
                             WHEN REGEOGR = 3 THEN "Sudeste"
                             WHEN REGEOGR = 4 THEN "Sul"
                             WHEN REGEOGR = 5 THEN "Centro-Oeste" END AS REGIAO
                FROM final')

VB01001 <- sqldf('SELECT CASE WHEN VB01001 = 1 THEN "Masculino"
                              WHEN VB01001 = 2 THEN "Feminino" END AS SEXO
                  FROM final')

VB01002 <- sqldf('SELECT CASE WHEN VB01002 = 1 THEN "Branca"
                              WHEN VB01002 = 2 THEN "Preta"
                              WHEN VB01002 = 3 THEN "Amarela"
                              WHEN VB01002 = 4 THEN "Parda"
                              WHEN VB01002 = 5 THEN "Indígena"
                              ELSE "NÃO INFORMADO" END AS RACA
                  FROM final')

VB01003 <- sqldf('SELECT CASE WHEN VB01003 = 11 THEN "11 anos ou menos"
                              WHEN VB01003 = 12 THEN "12 anos"
                              WHEN VB01003 = 13 THEN "13 anos"
                              WHEN VB01003 = 14 THEN "14 anos"
                              WHEN VB01003 = 15 THEN "15 anos"
                              WHEN VB01003 = 16 THEN "16 anos"
                              WHEN VB01003 = 17 THEN "17 anos"
                              WHEN VB01003 = 18 THEN "18 anos"
                              WHEN VB01003 = 19 THEN "19 anos ou mais" END AS IDADE
                 FROM final')

VB08001 <- sqldf('SELECT CASE WHEN VB08001 = -1 THEN "Pulou questionário"
                              WHEN VB08001 = 1 THEN "Sim"
                              WHEN VB08001 = 2 THEN "Não"
                              ELSE "Não informado" END AS B08001
                  FROM final')

VB08002 <- sqldf('SELECT CASE WHEN VB08002 = -1 THEN "Pulou questionário"
                              WHEN VB08002 = 9 THEN "9 anos ou menos"
                              WHEN VB08002 = 10 THEN "10 anos"
                              WHEN VB08002 = 11 THEN "11 anos"
                              WHEN VB08002 = 12 THEN "12 anos"
                              WHEN VB08002 = 13 THEN "13 anos"
                              WHEN VB08002 = 14 THEN "14 anos"
                              WHEN VB08002 = 15 THEN "15 anos"
                              WHEN VB08002 = 16 THEN "16 anos ou mais"
                              ELSE "Não informado" END AS B08002
                  FROM final')

VB08003A <- sqldf('SELECT CASE WHEN VB08003A = -1 THEN "Pulou questionário"
                               WHEN VB08003A = 0 THEN "Nunca teve relação sexual"
                               WHEN VB08003A = 1 THEN "1 pessoa"
                               WHEN VB08003A = 2 THEN "2 pessoas"
                               WHEN VB08003A = 3 THEN "3 pessoas"
                               WHEN VB08003A = 4 THEN "4 pessoas"
                               WHEN VB08003A = 5 THEN "5 pessoas"
                               WHEN VB08003A = 6 THEN "6 pessoas ou mais"
                               ELSE "Não informado" END AS B08003
                  FROM final')

VB08005 <- sqldf('SELECT CASE WHEN VB08005 = -1 THEN "Pulou questionário"
                               WHEN VB08005 = 0 THEN "Nunca teve relação sexual"
                               WHEN VB08005 = 1 THEN "Sim"
                               WHEN VB08005 = 2 THEN "Não"
                               WHEN VB08005 = 3 THEN "Não sabe"
                               ELSE "Não informado" END AS B08005
                  FROM final')


VB08008 <- sqldf('SELECT CASE WHEN VB08008 = -1 THEN "Pulou questionário"
                               WHEN VB08008 = 1 THEN "Sim"
                               WHEN VB08008 = 2 THEN "Não"
                               WHEN VB08008 = 3 THEN "Não sabe"
                               ELSE "Não informado" END AS B08008
                  FROM final')

VB08009 <- sqldf('SELECT CASE WHEN VB08009 = -1 THEN "Pulou questionário"
                               WHEN VB08009 = 1 THEN "Sim"
                               WHEN VB08009 = 2 THEN "Não"
                               WHEN VB08009 = 3 THEN "Não sabe"
                               ELSE "Não informado" END AS B08009
                  FROM final')


V0007 <- sqldf('SELECT CASE WHEN V0007 = 1 THEN "Pública"
                            WHEN V0007 = 2 THEN "Privada" 
                            END AS PUBPRIV
               FROM final')

final <- cbind(final$ANOPESQ, regiao, V0007, VB01001,VB01002, VB01003, VB08001, VB08002, VB08003A, VB08005,
               VB08008,VB08009 )
#------------------------------------------------------------------------------------------------------------

# Salvando arquivo final
write.csv(final,'final.csv',row.names = T, fileEncoding = 'UTF-8')
