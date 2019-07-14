library(rpart)
library(caret)

train = read.csv(file.choose())

train <- na.omit(train)
train2 <- train[,c(2,3,5,6,8,10,12)]

colnames(train)
View(train)
cor()


amostra = sample(2,714, replace = T, prob = c(0.7,0.3)) # 70% para treino, 30% para teste dos mil registros de credito
amostra

train3 = train2[amostra==1,] 

teste = train2[amostra==2,]

#################### DECISION TREE ################################################

# Modelo com todas as variáveis
modelo = rpart(Survived~., data = train3, method='class')
plot(modelo$variable.importance)

# Modelo com as variáveis mais importantes
modelo = rpart(Survived~Sex+Pclass+Fare+Age, data = train3, method='class')

previsao = predict(modelo, teste)
previsao

previsao <- colnames(previsao[,1:2]) [max.col(previsao[,1:2], ties.method = 'first')]

previsao <- as.factor(previsao)
teste$Survived <- as.factor(teste$Survived)

confusao = confusionMatrix(reference = teste$Survived, data = previsao, positive = '0')
confusao

y <- teste$Survived
predictions <- previsao

precision <- posPredValue(predictions, y)
precision

recall <- sensitivity(predictions, y)
recall

F1 <- ((1+(0.5*0.5))*(precision*recall/(((0.5*0.5)*precision)+recall)))
F1 # modelo com todas as variáveis é a melhor

#-------- Arquivo real Test ----------------------------------------------
test_real = read.csv(file.choose())


teste_real2 = test_real[,c(2,4,5,6,7,9,11)]
View(teste_real2)

previsao2 = predict(modelo, teste_real2)
previsao2

previsao2 <- colnames(previsao2[,1:2]) [max.col(previsao2[,1:2], ties.method='first')]
previsao2

previsao2 = cbind(test_real$PassengerId,previsao2)
View(previsao)

colnames(previsao2) <- c('PassengerId','Survived')
View(previsao2)

setwd('')
write.csv(previsao2[,1:2], file = 'previsao.csv', row.names = F)
