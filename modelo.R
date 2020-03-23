# Credit Classification

# Importando os dados

credito = read.csv("./Credit/Credit.csv", sep = ',', header = T)

# EDA

summary(credito)

head(credito)
tail(credito)

str(credito)

sum(is.na(credito))

dim(credito[duplicated(credito), ])[1]

# Preparação dos dados

amostra = sample(2, length(credito), replace = T, prob = c(0.7, 0.3))

treino = credito[amostra == 1, ]

teste = credito[amostra == 2, ]

# Modelagem


# K - Nearest Neighbor

library(class)

matrix_treino = data.frame(data.matrix(treino))
# matrix_treino = data.frame(scale(matrix_treino, center = FALSE, scale = apply(matrix_treino, 2, sd, na.rm = TRUE)))

matrix_teste = data.frame(data.matrix(teste))
# matrix_teste = data.frame(data.frame(scale(matrix_teste, center = FALSE, scale = apply(matrix_teste, 2, sd, na.rm = TRUE))))

aux = c()

for (i in 1:50) {
  
  class_knn = knn(matrix_treino[, 1:20], matrix_teste[, 1:20], matrix_treino[, 21], k = i)
  
  prev = table(matrix_teste$class, class_knn)

  acertos = (prev[1] + prev[4]) / sum(prev)
  
  aux[length(aux) + 1] = acertos
  
}

k = as.numeric(which.max(aux))

class_knn = knn(matrix_treino[, 1:20], matrix_teste[, 1:20], matrix_treino[, 21], k = k)

table(matrix_teste$class, class_knn)


# Naive Bayes

library(e1071)

modelo_bayes = naiveBayes(class ~ . , treino)

class_bayes = predict(modelo_bayes, teste)

table(teste$class, class_bayes)


# Decision Trees

library(rpart)

modelo_trees = rpart(class ~ . , data = treino, method = 'class')

plot(modelo_trees)                                               
text(modelo_trees) 

class_trees = data.frame(predict(modelo_trees, teste))
class_trees = ifelse(class_trees$bad >= 0.5, 'bad', 'good')

table(teste$class, class_trees)


# Random Forest

library(randomForest)

modelo_forest = randomForest(class ~ . , data = treino, ntree = 100, importance = T)

varImpPlot(modelo_forest)

class_forest = predict(modelo_forest, teste)

table(teste$class, class_forest)

# Support Vector Machine

library (e1071)

library(FSelector)

random.forest.importance(class ~ . , credito) # Verifica quão importante é um atributo para uma classe

modelo_svm = svm(class ~ checking_status + duration + credit_history + credit_amount + savings_status, treino)

class_svm = predict(modelo_svm, teste)

table(teste$class, class_svm)


