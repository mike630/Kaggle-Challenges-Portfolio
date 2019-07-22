setwd('C:/Projetos/Kaggle/House Prices - Advanced Regression Techniques')

install.packages('ggrepel')
install.packages('psych')

library(corrplot)
library(knitr)
library(randomForest)
library(caret)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(psych)
library(xgboost)

train <- read.csv(file.choose(), na.strings = c('NA', ''))
View(train)

test <- read.csv(file.choose(), na.strings = c('NA', ''))
View(test)

colnames(train[!colnames(train) %in% colnames(test)])

test$SalePrice <- NA
all <- rbind(train,test)

dim(all)
str(all)

kable(sapply(all, function(x) sum(is.na(x))))

kable(str(all))

#---------------------------------------------------------------------------------

# Correlação

# Verificando a correlação das variáveis numéricas

numericVars <- which(sapply(all, is.numeric))#index vector numeric variables
numericVars <- names(numericVars)
View(numericVars)
class(numericVars)
length(numericVars)

correlacao <- cor(all[,numericVars], use = 'pairwise.complete.obs')

# Melhor correlações para a variável-alvo

best_cor <- sort(correlacao[,'SalePrice'], decreasing = TRUE)
View(best_cor)
# PS: Lembrando que as correlações fortes são 1 e -1.

# Correlação com todas as variáveis numéricas

corrplot(correlacao, tl.col="black", tl.pos = "lt")
# Preciso incluir apenas uma variável independente que tenha uma alta correlação 
# com outra variável independente se elas tiverem logicamente dependentes entre elas.
# Caso contrário, vale a pena verificar se as duas variáveis devem ser incluídas no modelo.

#  De acordo com o gráfico, YearBuilt x GarageYrBlt, GrLiveArea x TotRmsAbvGrd, 
# HalfBath x TotRmsAbvGrd, YearRemodAdd x GarageYrBlt,GarageArea x GarageCars, 
# TotalBsmtSF x X1stFlrSF


#  Já as altas correlações entre as variáveis independentes com a variável-alvo (SalePrice),
# são ótimas variáveis preditoras para o nosso modelo.


# Analisando as maiores corelaÇões com a variável-alvo

#sort on decreasing correlations with SalePrice - Target Variable
cor_decresc <- as.matrix(sort(correlacao[,'SalePrice'], decreasing = TRUE))
# Visualizando as correlações

#--------------------------------------------------------------------------------------
# Dealing with missing values

all$MiscFeature <- as.character(all$MiscFeature)
all$MiscFeature <- ifelse(is.na(all$MiscFeature),'None', all$MiscFeature)
all$MiscFeature <- as.factor(all$MiscFeature)

all$Fence <- as.character(all$Fence)
all$Fence <- ifelse(is.na(all$Fence),'No Fence', all$Fence)
all$Fence <- as.factor(all$Fence)

all$PoolQC <- as.character(all$PoolQC)
all$PoolQC <- ifelse(is.na(all$PoolQC),'No Pool', all$PoolQC)
all$PoolQC <- as.factor(all$PoolQC)

all$GarageCond <- as.character(all$GarageCond)
all$GarageCond <- ifelse(is.na(all$GarageCond),'No Garage', all$GarageCond)
all$GarageCond <- as.factor(all$GarageCond)

all$GarageQual <- as.character(all$GarageQual)
all$GarageQual <- ifelse(is.na(all$GarageQual),'No Garage', all$GarageQual)
all$GarageQual <- as.factor(all$GarageQual)

all$GarageFinish <- as.character(all$GarageFinish)
all$GarageFinish <- ifelse(is.na(all$GarageFinish),'No Garage', all$GarageFinish)
all$GarageFinish <- as.factor(all$GarageFinish)

all$GarageType <- as.character(all$GarageType)
all$GarageType <- ifelse(is.na(all$GarageType),'No Garage', all$GarageType)
all$GarageType <- as.factor(all$GarageType)

all$FireplaceQu <- as.character(all$FireplaceQu)
all$FireplaceQu <- ifelse(is.na(all$FireplaceQu),'No Fireplace', all$FireplaceQu)
all$FireplaceQu <- as.factor(all$FireplaceQu)

all$BsmtFinType2 <- as.character(all$BsmtFinType2)
all$BsmtFinType2 <- ifelse(is.na(all$BsmtFinType2),'No Basement', all$BsmtFinType2)
all$BsmtFinType2 <- as.factor(all$BsmtFinType2)

all$BsmtFinType1 <- as.character(all$BsmtFinType1)
all$BsmtFinType1 <- ifelse(is.na(all$BsmtFinType1),'No Basement', all$BsmtFinType1)
all$BsmtFinType1 <- as.factor(all$BsmtFinType1)

all$BsmtExposure <- as.character(all$BsmtExposure)
all$BsmtExposure <- ifelse(is.na(all$BsmtExposure),'No Basement', all$BsmtExposure)
all$BsmtExposure <- as.factor(all$BsmtExposure)

all$BsmtCond <- as.character(all$BsmtCond)
all$BsmtCond <- ifelse(is.na(all$BsmtCond),'No Basement', all$BsmtCond)
all$BsmtCond <- as.factor(all$BsmtCond)

all$BsmtQual <- as.character(all$BsmtQual)
all$BsmtQual <- ifelse(is.na(all$BsmtQual),'No Basement', all$BsmtQual)
all$BsmtQual <- as.factor(all$BsmtQual)                          

all$Alley <- as.character(all$Alley)
all$Alley <- ifelse(is.na(all$Alley),'No alley access', all$Alley)
all$Alley <- as.factor(all$Alley)

all$GarageYrBlt <- as.character(all$GarageYrBlt)
all$GarageYrBlt <- ifelse(is.na(all$GarageYrBlt),'No Garage', all$GarageYrBlt)
all$GarageYrBlt <- as.character(all$GarageYrBlt)

all$GarageCars <- as.character(all$GarageCars)
all$GarageCars <- ifelse(is.na(all$GarageCars), 0 , all$GarageCars)
all$GarageCars <- as.integer(all$GarageCars)

all$GarageArea <- as.character(all$GarageArea)
all$GarageArea <- ifelse(is.na(all$GarageArea), 0 , all$GarageArea)
all$GarageArea <- as.integer(all$GarageArea)

#----------------------------------------------------------------------------------
# Check Power BI Data Visualizations
all$MSZoning <- as.character(all$MSZoning)
all$MSZoning <- ifelse(is.na(all$MSZoning) &
                         all$Street == 'Grvl' &
                         all$Neighborhood == 'IDOTRR', "C (all)", all$MSZoning)

all$MSZoning <- ifelse(is.na(all$MSZoning) &
                         all$Street == 'Pave' &
                         all$Neighborhood == 'IDOTRR', 'RM', all$MSZoning)

all$MSZoning <- ifelse(is.na(all$MSZoning) &
                         all$Street == 'Pave' &
                         all$Neighborhood == 'Mitchel', 'RL', all$MSZoning)
all$MSZoning <- as.factor(all$MSZoning)

#----------------------------------------------------------------------------------------
# Transformando variáveis quantitativas em qualitativos

all$MSSubClass <- as.factor(all$MSSubClass)
all$OverallQual <- as.factor(all$OverallQual)
all$OverallCond  <- as.factor(all$OverallCond)

#---------------------------------------------------------------------------------------

# Modelagem e Previsao para imputar os dados para o MasVnrType no train

kable(sapply(all, function(x) sum(is.na(x))))

# Amostra Holdout
trainClean <- all[!is.na(all$MasVnrType) & !is.na(all$Exterior1st) & !is.na(all$Exterior2nd),]
testClean <- all[is.na(all$MasVnrType) & !is.na(all$Exterior1st) & !is.na(all$Exterior2nd),]

amostra = sample(2,dim(trainClean)[1], replace = T, prob = c(0.7,0.3))

trainClean1 <- trainClean[amostra==1,]
trainClean2 <- trainClean[amostra==2,]

# Modelo 1
set.seed(2017)
modelo = randomForest(MasVnrType~MSSubClass+LotArea+LotShape+LotConfig+Neighborhood+
                        OverallQual+OverallCond+YearBuilt+
                        YearRemodAdd+Exterior1st+Exterior2nd+ExterQual+BsmtQual +BsmtExposure+
                        BsmtFinType1+X1stFlrSF+X2ndFlrSF, 
                        data = trainClean1, ntree= 350)

plot(modelo)
summary(modelo$importance)
varImpPlot(modelo)
View(importance(modelo))

# Modelo 2
set.seed(12345)
modelo2 = train(MasVnrType~MSSubClass+LotArea+LotShape+LotConfig+Neighborhood+
                  OverallQual+OverallCond+YearBuilt+
                  YearRemodAdd+Exterior1st+Exterior2nd+ExterQual+BsmtQual +BsmtExposure+
                  BsmtFinType1+X1stFlrSF+X2ndFlrSF, 
                data = trainClean1, method= 'rf', 
                trControl=trainControl(method="cv", number=7))

plot(modelo2)
modelo2$results
class(modelo2)
a <- varImp(modelo2,scale = T)
View(a$importance)

previsao = predict(modelo, trainClean2)
previsao2 = predict(modelo2, trainClean2)

previsao <- as.factor(previsao)
previsao2 = as.factor(previsao2)
trainClean2$MasVnrType <- as.factor(trainClean2$MasVnrType)

confusao = confusionMatrix(reference = trainClean2$MasVnrType, data = previsao)
confusao

confusao2 = confusionMatrix(reference = trainClean2$MasVnrType, data = previsao2)
confusao2

# Previsao final será com o modelo1 

modelo3 = randomForest(MasVnrType~MSSubClass+LotArea+LotShape+LotConfig+Neighborhood+
                        OverallQual+OverallCond+YearBuilt+
                        YearRemodAdd+Exterior1st+Exterior2nd+ExterQual+BsmtQual +BsmtExposure+
                        BsmtFinType1+X1stFlrSF+X2ndFlrSF, 
                      data = trainClean, ntree= 350)

# Imputando a previsao nos missing values do dataset  train
imput = predict(modelo3, testClean)
table(imput)
imput <- as.character(imput)

View(all[is.na(all$MasVnrType) | is.na(all$MasVnrArea), c('MasVnrType','MasVnrArea')])

all$MasVnrType <- as.character(all$MasVnrType)
all$MasVnrType[is.na(all$MasVnrType)] <- imput
all$MasVnrType <- as.factor(all$MasVnrType)

# Pegando a média de MasVnrArea para imputar
all$MasVnrArea <- ifelse(all$MasVnrType != 'None' & is.na(all$MasVnrArea), 
                         mean(all$MasVnrArea[!is.na(all$MasVnrArea)]),
                         ifelse(all$MasVnrType == 'None' & is.na(all$MasVnrArea),
                                0,all$MasVnrArea))

#----------------------------------------------------------------------------------------

# Modelagem e Previsao para imputar os dados para o LotFrontage no train

ggplot(all[!is.na(all$SalePrice),], aes(x = LotFrontage, y= LotArea)) +
  geom_point(col = 'green') + geom_smooth(method = 'lm', se = F, col = 'blue' ) + ylim(0,75000)
# Como podemos perceber pela análise do Power BI e pela correlação acima quanto maior a 
# LotArea, maior tendência do LotFrontage ser maior também.
# Para imputar os dados, devo verificar a possibilidade de prever o valor de cada NA de LotFrontage
# com regressão.

# Amostra Holdout
trainClean <- all[!is.na(all$LotFrontage) & !is.na(all$TotalBsmtSF),]
testClean <- all[is.na(all$LotFrontage) & !is.na(all$TotalBsmtSF),]

amostra = sample(2,dim(trainClean)[1], replace = T, prob = c(0.7,0.3))

trainClean1 <- trainClean[amostra==1,]
trainClean2 <- trainClean[amostra==2,]

# Modelo 1
set.seed(2017)
modelo4 = lm(LotFrontage~LotArea+Neighborhood+X1stFlrSF+TotRmsAbvGrd+GarageArea,
            data = trainClean1)

modelo4
summary(modelo4)

# Modelo 2
set.seed(12345)
modelo5 = train(LotFrontage~LotArea+Neighborhood+X1stFlrSF+TotRmsAbvGrd+
                GarageArea,data = trainClean1, 
                method= 'lm',trControl=trainControl(method="cv", number=7))

modelo5

modelo5$results
summary(modelo5)
sort(summary(modelo5)$coefficients[,4], decreasing  = F)
class(modelo5)

# Podemos verificar pela R-squared e Adj R-squared, além do RMSE, MAE que o
# modelo não é tão bom para prever os missing values da variável LotFrontage
# e por isso, não usaremos mais esta variável

all$LotFrontage <- NULL

#----------------------------------------------------------------------------------------

kable(all[(is.na(all$BsmtFullBath)|is.na(all$BsmtHalfBath)|is.na(all$BsmtFinSF1)|is.na(all$BsmtFinSF2)|
       is.na(all$BsmtUnfSF)|is.na(all$TotalBsmtSF)), c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 
                                                       'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF')])

str(all[,c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 
           'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF')])

all$BsmtFullBath <- ifelse(is.na(all$BsmtFullBath),0,all$BsmtFullBath)
all$BsmtHalfBath <- ifelse(is.na(all$BsmtHalfBath),0,all$BsmtHalfBath)
all$BsmtFinSF1 <- ifelse(is.na(all$BsmtFinSF1),0,all$BsmtFinSF1)
all$BsmtFinSF2 <- ifelse(is.na(all$BsmtFinSF2),0,all$BsmtFinSF2)
all$BsmtUnfSF <- ifelse(is.na(all$BsmtUnfSF),0,all$BsmtUnfSF)
all$TotalBsmtSF <- ifelse(is.na(all$TotalBsmtSF),0,all$TotalBsmtSF)

# Como só há 1 NA para a variável Exterior1st e Exterior2nd numa mesma observação, irei sortear a 
#classe com a maior frequência para fazer a imputação.
all$Exterior1st[is.na(all$Exterior1st)] <- names(sort(table(all$Exterior1st),decreasing=T))[1]
table(all$Exterior1st)

all$Exterior2nd[is.na(all$Exterior2nd)] <- names(sort(table(all$Exterior2nd),decreasing=T))[1]
table(all$Exterior2nd)

# Como Há apenas 1 observação com classe diferente, eu irei desconsiderar a variável Utilities
table(all$Utilities)
View(all[is.na(all$Utilities),])
all$Utilities <- NULL

# Como só há 1 NA para a variável Electrical, irei sortear a 
#classe com a maior frequência para fazer a imputação.
table(all$Electrical)
summary(all$Electrical[is.na(all$Electrical)])


# Esta é uma forma mais linear de fazer uma consulta
table(all$Electrical) %>% sort(decreasing = T) %>% names()
# Este é a forma de uma função dentro de outra para trazer a consulta
names(sort(decreasing = T, table(all$Electrical)))[1] -> all$Electrical[is.na(all$Electrical)]

# Como só há 1 NA para a variável KitchenQual, irei sortear a 
#classe com a maior frequência para fazer a imputação.
summary(all$KitchenQual)
table(all$KitchenQual)
names(sort(decreasing = T, table(all$KitchenQual)))[1] -> all$KitchenQual[is.na(all$KitchenQual)]

View(table(all[all$KitchenQual=='Gd' | all$KitchenQual=='TA',c('KitchenAbvGr', 'KitchenQual', 'SalePrice')]))

# Como só há 1 NA para a variável SaleType, irei sortear a 
#classe com a maior frequência para fazer a imputação.
summary(all$SaleType)
table(all$SaleType)
names(sort(decreasing = T, table(all$SaleType)))[1] -> all$SaleType[is.na(all$SaleType)]

# Como só há 2 NA para a variável Functional, irei sortear a 
#classe com a maior frequência para fazer a imputação.
summary(all$Functional)
table(all$Functional)
names(sort(decreasing = T, table(all$Functional)))[1] -> all$Functional[is.na(all$Functional)]

# Missing Values Zerados
kable(sapply(all, function(x) sum(is.na(x))))


#-------------------------------------------------------------------------------------------
#Verificando outliers

ggplot(data=all[!is.na(all$SalePrice),], aes(x=factor(OverallQual), y = SalePrice))+
  geom_boxplot(col='blue')

ggplot(data=all[!is.na(all$SalePrice),], aes(x=factor(OverallCond), y = SalePrice))+
  geom_boxplot(col='blue')

ggplot(data=all[!is.na(all$SalePrice),], aes(x=factor(ExterCond), y = SalePrice))+
  geom_boxplot(col='blue')

ggplot(data=all[!is.na(all$SalePrice),], aes(x=SalePrice, y= GrLivArea))+
  geom_point (col='purple') + labs(x='Sale Price', y = 'Living area square feet')+
  geom_smooth(method = "lm", se=FALSE, color="black")+
  scale_x_continuous(breaks = seq(0,755000, 100000)) +
  scale_y_continuous(breaks = seq(0, 6000, 500)) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)]>4500, 
                                     rownames(all), '')))

ggplot(data=all[!is.na(all$SalePrice),], aes(x=SalePrice, y= GarageArea))+
  geom_point (col='purple') + labs(x='Sale Price', y = 'Garage Area')+
  geom_smooth(method = "lm", se=FALSE, color="black")+
  scale_x_continuous(breaks = seq(0,755000, 100000)) +
  scale_y_continuous(breaks = seq(0, 1500, 250))+
  geom_text_repel(aes(label = ifelse(all$GarageArea[!is.na(all$SalePrice)]>1250, 
                                     rownames(all), '')))

ggplot(data=all[!is.na(all$SalePrice),], aes(x=SalePrice, y= TotalBsmtSF))+
  geom_point (col='purple') + labs(x='Sale Price', y = 'Total Basement Square Feet')+
  geom_smooth(method = "lm", se=FALSE, color="black")+
  scale_x_continuous(breaks = seq(0,755000, 100000)) +
  scale_y_continuous(breaks = seq(0, 6200, 500)) +
  geom_text_repel(aes(label = ifelse(all$TotalBsmtSF[!is.na(all$SalePrice)]>6000, 
                                     rownames(all), '')))

ggplot(data=all[!is.na(all$SalePrice),], aes(x=SalePrice, y= X1stFlrSF))+
  geom_point (col='purple') + labs(x='Sale Price', y = '1st Floor Square Feet')+
  geom_smooth(method = "lm", se=FALSE, color="black")+
  scale_x_continuous(breaks = seq(0,755000, 100000)) +
  scale_y_continuous(breaks = seq(0, 5100, 400)) +
  geom_text_repel(aes(label = ifelse(all$X1stFlrSF[!is.na(all$SalePrice)]>4400, 
                                     rownames(all), '')))

ggplot(data=all[!is.na(all$SalePrice),], aes(x=SalePrice, y= MasVnrArea))+
  geom_point (col='purple') + labs(x='Sale Price', y = 'Masonry veneer area')+
  geom_smooth(method = "lm", se=FALSE, color="black")+
  scale_x_continuous(breaks = seq(0,755000, 100000))+
  geom_text_repel(aes(label = ifelse(all$MasVnrArea[!is.na(all$SalePrice)]>1500, 
                                     rownames(all), '')))

ggplot(data=all[!is.na(all$SalePrice),], aes(x=SalePrice, y= BsmtFinSF1))+
  geom_point (col='purple') + labs(x='Sale Price', y = '2nd Floor Square Feet')+
  geom_smooth(method = "lm", se=FALSE, color="black")+
  scale_x_continuous(breaks = seq(0,755000, 100000))+
  geom_text_repel(aes(label = ifelse(all$BsmtFinSF1[!is.na(all$SalePrice)]>4000, 
                                     rownames(all), '')))

# Como podemos observar nos gráficos acima, a observaçao de número 1299 é a observação
# que mais enviesa nossas análises e deve ser retirada para fazermos a modelagem.
# Irei retirar mais alguns outliers

all <- all[-c(1299,582,1191,524,298),]

#-----------------------------------------------------------------------------------

# Veriricando as variáveis categóricas mais importantes

classVars <- which(sapply(all, is.factor))#index vector numeric variables
names(classVars)

all %>% select(classVars, SalePrice) -> col1

# Amostra Holdout
trainClean <- col1[!is.na(col1$SalePrice),]
testClean <- col1[is.na(col1$SalePrice),]

amostra = sample(2,dim(trainClean)[1], replace = T, prob = c(0.7,0.3))

trainClean1 <- trainClean[amostra==1,]
trainClean2 <- trainClean[amostra==2,]

# Modelo 1
set.seed(2017)

modelo6 = randomForest(SalePrice ~ ., data = trainClean1, trControl=trainControl(method="cv", number=5))

plot(modelo6)
summary(modelo6)
varImpPlot(modelo6)

importanceVar <- as.data.frame(importance(modelo6))

#---------------------------------------------------------------------------------

# Correlação Novamente

# Verificando a correlação das variáveis numéricas novamente

numericVars2 <- which(sapply(all, is.numeric))#index vector numeric variables
numericVars2 <- names(numericVars2)
View(numericVars2)
class(numericVars2)
length(numericVars2)

correlacao2 <- cor(all[,numericVars2], use = 'pairwise.complete.obs')

# Melhor correlações para a variável-alvo

best_cor2 <- sort(correlacao2[,'SalePrice'], decreasing = TRUE)
View(best_cor2)
# PS: Lembrando que as correlações fortes são 1 e -1.

# Correlação com todas as variáveis numéricas

corrplot(correlacao2, tl.col="black", tl.pos = "lt")
# Preciso incluir apenas uma variável independente que tenha uma alta correlação 
# com outra variável independente se elas tiverem lógica dependentes entre elas.
# Caso contrário, vale a pena verificar se as duas variáveis devem ser incluídas no modelo.

#  De acordo com o gráfico, BsmtFinSF1 x BsmtFullBath , TotalBsmtSF x X1stFlrSF
#  X2ndFlrSF x GrLivArea, X2ndFlrSF x HalfBath, X2ndFlrSF x TotRmsAbvGrd
# GrLivArea x FullBath, TotRmsAbvGrd x GrLivArea, BsmtFullBath x BsmtFinSF1
# TotRmsAbvGrd x BedroomAbvGr, GarageArea x GarageCars

#  Já as altas correlações entre as variáveis independentes com a variável-alvo (SalePrice),
# são ótimas variáveis preditoras para o nosso modelo.


# Analisando as maiores corelaÇões com a variável-alvo

#sort on decreasing correlations with SalePrice - Target Variable
cor_decresc2 <- as.matrix(sort(correlacao2[,'SalePrice'], decreasing = TRUE))

# ---------------------------------------------------------------------------------

# Deletando variáveis que tenha forte correlação com outra variável independente,
# se elas tiverem realmente a possibilidade de estarem demonstrando o mesmo tipo de dados.

# De acordo com o gráfico corrplot, eu irei excluir as seguintes variáveis:

all$GarageArea <- NULL
all$TotRmsAbvGrd <- NULL
all$YearRemodAdd <- NULL
all$TotalBsmtSF <- NULL


# Com relação as variáveis categóricas, irei excluir Exterior2nd

all$Exterior2nd <- NULL

# Também devo verificar as variáveis categóricas, se elas tem correlação entre si.

# OverallQual x OverallCond
ggplot(all, aes(x = as.factor(OverallCond))) + geom_bar(stat = 'count')

ggplot(all, aes(x = as.factor(OverallQual))) + geom_bar(stat = 'count')

a <- as.numeric(all$OverallQual)
b <- as.numeric(all$OverallCond)
cor(a,b)
# Não há correlação

# GarageQual x GarageCond
ggplot(all, aes(x = as.factor(GarageQual))) + geom_bar(stat = 'count')

ggplot(all, aes(x = as.factor(GarageCond))) + geom_bar(stat = 'count')

a <- as.numeric(all$GarageQual)
b <- as.numeric(all$GarageCond)
cor(a,b)
# Correlação de 54%, irei mantê-la

# BsmtQual x BsmtCond
ggplot(all, aes(x = as.factor(BsmtQual))) + geom_bar(stat = 'count')

ggplot(all, aes(x = as.factor(BsmtCond))) + geom_bar(stat = 'count')

a <- as.numeric(all$BsmtQual)
b <- as.numeric(all$BsmtCond)
cor(a,b)
# Não há correlação

# ExterQual x ExterCond

ggplot(all, aes(x = as.factor(ExterQual))) + geom_bar(stat = 'count')

ggplot(all, aes(x = as.factor(ExterCond))) + geom_bar(stat = 'count')

a <- as.numeric(all$ExterQual)
b <- as.numeric(all$ExterCond)
cor(a,b)
# Não há correlação

# Deletarei variáveis que tenha baixa variabilidade do modelo

ggplot(all, aes(x = as.factor(BsmtCond))) + geom_bar(stat = 'count')
table(all$BsmtCond)
all$BsmtCond <- NULL

ggplot(all, aes(x = as.factor(GarageCond))) + geom_bar(stat = 'count')
table(all$GarageCond)
all$GarageCond <- NULL

ggplot(all, aes(x = as.factor(GarageQual))) + geom_bar(stat = 'count')
table(all$GarageQual)
all$GarageQual <- NULL

ggplot(all, aes(x = as.factor(ExterCond))) + geom_bar(stat = 'count')
table(all$ExterCond)
all$ExterCond <- NULL

ggplot(all, aes(x = as.factor(Street))) + geom_bar(stat = 'count') 
table(all$Street)
all$Street <- NULL

ggplot(all, aes(x = as.factor(Alley))) + geom_bar(stat = 'count') 
table(all$Alley)
all$Alley <- NULL

ggplot(all, aes(x = as.factor(LandContour))) + geom_bar(stat = 'count') 
table(all$LandContour)
all$LandContour <- NULL

ggplot(all, aes(x = as.factor(LandSlope))) + geom_bar(stat = 'count') 
table(all$LandSlope)
all$LandSlope <- NULL

ggplot(all, aes(x = as.factor(Condition1))) + geom_bar(stat = 'count') 
table(all$Condition1)
all$Condition1 <- NULL

ggplot(all, aes(x = as.factor(Condition2))) + geom_bar(stat = 'count') 
table(all$Condition2)
all$Condition2 <- NULL

ggplot(all, aes(x = as.factor(BldgType))) + geom_bar(stat = 'count') 
table(all$BldgType)
all$BldgType <- NULL

ggplot(all, aes(x = as.factor(RoofMatl))) + geom_bar(stat = 'count')
table(all$RoofMatl)
all$RoofMatl <- NULL

ggplot(all, aes(x = as.factor(Heating))) + geom_bar(stat = 'count') 
table(all$Heating)
all$Heating <- NULL

ggplot(all, aes(x = as.factor(CentralAir))) + geom_bar(stat = 'count') 
table(all$CentralAir)
all$CentralAir <- NULL

ggplot(all, aes(x = as.factor(Electrical))) + geom_bar(stat = 'count') 
table(all$Electrical)
all$Electrical <- NULL

ggplot(all, aes(x = as.factor(Functional))) + geom_bar(stat = 'count') 
table(all$Functional)
all$Functional <- NULL

ggplot(all, aes(x = as.factor(PavedDrive))) + geom_bar(stat = 'count') 
table(all$PavedDrive)
all$PavedDrive <- NULL

ggplot(all, aes(x = as.factor(PoolQC))) + geom_bar(stat = 'count') 
table(all$PoolQC)
all$PoolQC <- NULL

ggplot(all, aes(x = as.factor(MiscFeature))) + geom_bar(stat = 'count') 
table(all$MiscFeature)
all$MiscFeature <- NULL

ggplot(all, aes(x = as.factor(SaleType))) + geom_bar(stat = 'count')
table(all$SaleType)
all$SaleType <- NULL

ggplot(all, aes(x = as.factor(MSSubClass))) + geom_bar(stat = 'count')
ggplot(all, aes(x = as.factor(MSZoning))) + geom_bar(stat = 'count')
ggplot(all, aes(x = as.factor(LotShape))) + geom_bar(stat = 'count')
ggplot(all, aes(x = as.factor(LotConfig))) + geom_bar(stat = 'count')
ggplot(all, aes(x = as.factor(Neighborhood))) + geom_bar(stat = 'count')
ggplot(all, aes(x = as.factor(HouseStyle))) + geom_bar(stat = 'count')
ggplot(all, aes(x = as.factor(RoofStyle))) + geom_bar(stat = 'count')
ggplot(all, aes(x = as.factor(Exterior1st))) + geom_bar(stat = 'count') 
ggplot(all, aes(x = as.factor(MasVnrType))) + geom_bar(stat = 'count')
ggplot(all, aes(x = as.factor(ExterQual))) + geom_bar(stat = 'count')
ggplot(all, aes(x = as.factor(Foundation))) + geom_bar(stat = 'count')
ggplot(all, aes(x = as.factor(BsmtExposure))) + geom_bar(stat = 'count')
ggplot(all, aes(x = as.factor(BsmtFinType1))) + geom_bar(stat = 'count')
ggplot(all, aes(x = as.factor(BsmtFinType2))) + geom_bar(stat = 'count')
ggplot(all, aes(x = as.factor(HeatingQC))) + geom_bar(stat = 'count')
ggplot(all, aes(x = as.factor(SaleCondition ))) + geom_bar(stat = 'count')
ggplot(all, aes(x = as.factor(KitchenQual))) + geom_bar(stat = 'count')
ggplot(all, aes(x = as.factor(FireplaceQu))) + geom_bar(stat = 'count')
ggplot(all, aes(x = as.factor(GarageType))) + geom_bar(stat = 'count')
ggplot(all, aes(x = as.factor(GarageFinish))) + geom_bar(stat = 'count')
ggplot(all, aes(x = as.factor(Fence))) + geom_bar(stat = 'count')
#---------------------------------------------------------------------------------

#Visualizando gráfico de frequência de SalePrice

ggplot(data=all[!is.na(all$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000))
summary(all$SalePrice)

# Não é normalmente distribuído SalePrice
qqnorm(all$SalePrice)
qqline(all$SalePrice)
shapiro.test(all$SalePrice)

skew(all$SalePrice)
# The skew of 1.87 indicates a right skew that is too high, and the Q-Q plot shows that 
# sale prices are also not normally distributed. To fix this I am taking the log of SalePrice.

skew(log(all$SalePrice))
#As you can see,the skew is now quite low and the Q-Q plot is also looking much better.
qqnorm(all$SalePrice)
qqline(all$SalePrice)

all$SalePrice <- log(all$SalePrice)

# Normalizando variáveis numéricas com mais de 0.8 de skew e pre-processando todas
all$Id <- as.character(all$Id)

numericVars2 <- which(sapply(all, is.numeric))

numericVars2 <- numericVars2[-which(names(numericVars2)=='SalePrice')] 
# Removendo SalePrice que já está com log e não pode ser preprocessado

skewed <- as.data.frame(ifelse(skew(all[,numericVars2]) > 0.7 | skew(all[,numericVars2]) < -0.7, 
                 log(all[,numericVars2]+1),all[,numericVars2]))

colnames(skewed) <- names(numericVars2)

#-----------------------------------------------------------------------------------------
# Preprocessing and predicting it

# PreProcessing dataset Train and predicting it
preNumVars <- preProcess(skewed, method = c('center','scale'),na.remove = T)
preNumVars
trainClean_NumVars <- predict(preNumVars,skewed)
dim(trainClean_NumVars)


# Transformando variáveis categóricas em dummies para facilitar nosso modelo a prever
# de forma mais eficiente.
categoricalVars2 <- which(sapply(all,is.factor))

dummy <- as.data.frame(model.matrix(~.-1,all[,categoricalVars2]))
dim(dummy)

# Juntando variáveis numéricas com variáveis dummies
trainClean <- cbind(trainClean_NumVars,dummy, all$SalePrice)
colnames(trainClean)[which(colnames(trainClean)== 'all$SalePrice')] <- 'SalePrice'
dim(trainClean)



#------------------------------------------------------------------------------------------
# Modelagem e previsao final
 
my_control <- trainControl(method="cv", number=10)

amostra = sample(2,dim(trainClean)[1], replace = T, prob = c(0.7,0.3))

treino = trainClean[!is.na(trainClean$SalePrice) & amostra==1,] 

teste = trainClean[!is.na(trainClean$SalePrice) & amostra==2,]

# Modelo 7
set.seed(2017)
lm_mod = lm(SalePrice ~ .,data = trainClean[!is.na(trainClean$SalePrice),])

summary(lm_mod)

# Verificando com validações
treino_mod = lm(SalePrice ~ .,data = treino)
summary(lm_mod)
modelo7_prev = predict(treino_mod, teste[,-which(colnames(treino)=='SalePrice')])
RMSE(pred = modelo7_prev, obs = teste[,'SalePrice'])

# Prevendo as vendas
lm_prev = predict(lm_mod,trainClean[is.na(trainClean$SalePrice),
                                    -which(colnames(trainClean)=='SalePrice')])

# Lasso Modelo
set.seed(50000)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.00001,0.1,by = 0.0005))
# alpha = 1 (lasso), alpha = 0 (ridge) and a value between 0 and 1 (say 0.3) is elastic net regression.
lasso_mod <- train(SalePrice~., data = trainClean[!is.na(trainClean$SalePrice),], 
                   method='glmnet', trControl= my_control,tuneGrid=lassoGrid) 

lasso_mod
lasso_mod$bestTune
min(lasso_mod$results$RMSE)
min(lasso_mod$results$MAE)
max(lasso_mod$results$Rsquared)

#Importância
varImp(lasso_mod)

# Prevendo as vendas
lasso_prev = predict(lasso_mod, trainClean[is.na(trainClean$SalePrice),
                                           -which(colnames(trainClean)=='SalePrice')])

# Ridge Modelo
set.seed(50000)
ridgeGrid <- expand.grid(alpha = 0, lambda = seq(0.00001,0.1,by = 0.0005)) 
# alpha = 1 (lasso), alpha = 0 (ridge) and a value between 0 and 1 (say 0.3) is elastic net regression.
ridge_mod <- train(SalePrice~., data = trainClean[!is.na(trainClean$SalePrice),], 
                   method='glmnet',trControl= my_control,tuneGrid=ridgeGrid) 

ridge_mod
ridge_mod$bestTune
min(ridge_mod$results$RMSE)
min(ridge_mod$results$MAE)
max(ridge_mod$results$Rsquared)


# Elastic Net Modelo
set.seed(50000)
elasticnet_mod <- train(SalePrice~., data = trainClean[!is.na(trainClean$SalePrice),],
                        method='glmnet', trControl= my_control, tuneLength = 25) 

elasticnet_mod
elasticnet_mod$bestTune
min(elasticnet_mod$results$RMSE)
min(elasticnet_mod$results$MAE)
max(elasticnet_mod$results$Rsquared)

# eXtreme Gradient Boosting - XGBoost
xgb_params <- list(
  booster = 'gbtree',
  objective = 'reg:linear',
  colsample_bytree=0.9,
  eta=0.071,
  max_depth=2,
  min_child_weight=5,
  alpha=0.41,
  lambda=0.35,
  gamma=0.0001, # less overfit
  subsample=0.8)

dtrain <- xgb.DMatrix(as.matrix(trainClean[!is.na(trainClean$SalePrice),
                                           -which(colnames(trainClean)=='SalePrice')]),
                      label = as.matrix(trainClean$SalePrice[!is.na(trainClean$SalePrice)]))

dtest <- xgb.DMatrix(as.matrix(trainClean[is.na(trainClean$SalePrice),
                                          -which(colnames(trainClean)=='SalePrice')]))

set.seed(50000)
xgboost_mod <- xgb.cv(xgb_params, data = dtrain,nrounds = 1000, metrics = 'rmse', 
                      print_every_n = 50, nfold = 10)

xgboost_mod2 <- xgb.train(data = dtrain, params=xgb_params, nrounds = 1000)

# Importância
mat <- xgb.importance (feature_names = colnames(trainClean[!is.na(trainClean$SalePrice),
                                                           -which(colnames(trainClean)=='SalePrice')]),
                       model = xgboost_mod2)

xgb.ggplot.importance(importance_matrix = mat[1:20], rel_to_first = TRUE)


xgboost_prev = predict(xgboost_mod2, dtest)

#Correlacao
correlacao <- cbind(lm_prev,lasso_prev,xgboost_prev)
cor(correlacao)

# Previsao Finallasso_prev
previsao <- (((2*lasso_prev)+(3*lm_prev)+xgboost_prev)/6)

previsao <- as.data.frame(exp(previsao))#need to reverse the log to the real values

previsao$id <- rownames(trainClean[is.na(trainClean$SalePrice),])

colnames(previsao) <- c('SalePrice','Id')

previsao$SalePrice <- round(previsao$SalePrice)

dim(previsao)

kable(sapply(previsao, function(x) sum(is.na(x))))

write.csv(previsao, file = 'previsao.csv', row.names = F )

