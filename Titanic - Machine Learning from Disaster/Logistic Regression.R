
###################### Logistic Regression ###################################################
library(caret)
set.seed(1234)

train = read.csv(file.choose())

sapply(train, function(x) sum(is.na(x)))
train <- na.omit(train)
train <- train[,c(2,3,5,6,8,10,12)]

colnames(train)
View(train)
cor(train$Survived,train$Age, use='na.or.complete')


amostra = sample(2,714, replace = T, prob = c(0.7,0.3)) # 70% para treino, 30% para teste dos mil registros de credito
amostra

treino = train[amostra==1,] 

teste = train[amostra==2,]

#-------------------------------------------------------------------------------------------------

treino$Survived <- as.factor(treino$Survived)
str(train)

# Modelo com todas as variáveis
modelo = glm(Survived~., data = treino, family = 'binomial' )


# Modelo com as melhores variáveis
modelo2 = glm(Survived~Pclass+Sex+Age+Fare, data = treino, 
              family = 'binomial' )

plot(treino$Fare, treino$Survived, col='red', pch=20)
points(treino$Fare, modelo$fitted, pch=4)

previsao = predict(modelo2, newdata = teste, type = 'response')
previsao

previsao <- as.numeric(previsao)
previsao$results <- ifelse(previsao >= 0.5, 1, 0)

previsao$results <- as.factor(previsao$results)
teste$Survived <- as.factor(teste$Survived)
str(previsao$results)
str(teste$Survived)
confusao = confusionMatrix(reference = teste$Survived, data = previsao$results)
confusao

precision <- posPredValue(data = previsao$results, reference = teste$Survived)
precision

recall <- sensitivity(data = previsao$result, reference = teste$Survived)
recall

F1 <- ((1+(0.5*0.5))*(precision*recall/(((0.5*0.5)*precision)+recall)))
F1 # modelo com selecionadas variáveis é a melhor
