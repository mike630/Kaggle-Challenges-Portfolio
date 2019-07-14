
###################### Random Forest ###################################################
library(randomForest)
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
modelo = randomForest(Survived~., data = treino, ntree=600, importance = T )
varImpPlot(modelo)

# Modelo com as melhores variáveis
modelo2 = randomForest(Survived~Pclass+Sex+Age+Fare, data = treino, ntree=600, importance = T )

previsao = predict(modelo2, newdata = teste)
previsao

previsao <- as.factor(previsao)
teste$Survived <- as.factor(teste$Survived)

confusao = confusionMatrix(reference = teste$Survived, data = previsao)
confusao

precision <- posPredValue(data = previsao, reference = teste$Survived)
precision

recall <- sensitivity(data = previsao, reference = teste$Survived)
recall

F1 <- ((1+(0.5*0.5))*(precision*recall/(((0.5*0.5)*precision)+recall)))
F1 # modelo com selecionadas variáveis é a melhor

#-------- Arquivo real Test ----------------------------------------------
test_real = read.csv(file.choose())

sapply(test_real, function(x) sum(is.na(x)))
dim(test_real)

test_real$Age = ifelse(is.na(test_real$Age),
                       yes = median(test_real$Age, na.rm=T),
                       test_real$Age)

sapply(test_real, function(x) sum(is.na(x)))

row.names(test_real[is.na(test_real$Fare) == T,])

test_real$Fare = ifelse(is.na(test_real$Fare),
                        yes = median(test_real$Fare, na.rm=T),
                        test_real$Fare)

sapply(test_real, function(x) sum(is.na(x)))


teste_real2 = test_real[,c(2,4,5,6,9)]

str(teste_real2)
str(train)
levels(teste_real2$Embarked) <- levels(train$Embarked)

train$Survived <- as.factor(train$Survived)
modelo2 = randomForest(Survived~Pclass+Sex+Age+Fare, data = train, ntree=600,
                       importance = T)
importance(modelo2)
varImpPlot(modelo2)

previsao = predict(object = modelo2, newdata = teste_real2)

previsao <- as.factor(previsao)
previsao <- cbind(test_real$PassengerId,previsao)
View(previsao)

colnames(previsao) <- c('PassengerId','Survived')
View(previsao)

previsao <- as.data.frame(previsao)
previsao$Survived <- ifelse(previsao$Survived == 1, 0, 1)

setwd('')
write.csv(previsao[,1:2], file = 'previsao.csv', row.names = F)
