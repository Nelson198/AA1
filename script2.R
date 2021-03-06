# Carregar a base de dados
db <- read.csv(file="database/heart.csv", header=T)

# Tratamento das variaveis qualitativas ordinais.
db$sex <- as.factor(db$sex)
db$cp <- as.factor(db$cp)
db$fbs <- as.factor(db$fbs)
db$exang <- as.factor(db$exang)
db$restecg <- as.factor(db$restecg)
db$slope <- as.factor(db$slope)
db$thal <- as.factor(db$thal)
db$target <- as.factor(db$target)

# Possivelmente n�o faz diferen�a
# Aumento da interpretabilidade das variaveis qualitativas ordinais.
levels(db$sex)[levels(db$sex) == 0] <- "Female"
levels(db$sex)[levels(db$sex) == 1] <- "Male"
levels(db$fbs)[levels(db$fbs) == 0] <- "Fasting Blood Sugar <= 120"
levels(db$fbs)[levels(db$fbs) == 1] <- "Fasting Blood Sugar > 120"
levels(db$thal)[levels(db$thal) == 0] <- "No Thalassemia"
levels(db$thal)[levels(db$thal) == 1] <- "Normal Thalassemia"
levels(db$thal)[levels(db$thal) == 2] <- "Fixed Defect Thalassemia"
levels(db$thal)[levels(db$thal) == 3] <- "Reversible Defect Thalassemia"
levels(db$target)[levels(db$target) == 0] <- "Healthy"
levels(db$target)[levels(db$target) == 1] <- "Heart Disease"
levels(db$exang)[levels(db$exang) == 1] <- "Exercise Induced Angina"
levels(db$exang)[levels(db$exang) == 0] <- "No Exercise Induced Angina"
levels(db$cp)[levels(db$cp) == 0] <- "Chest Pain Type 0"
levels(db$cp)[levels(db$cp) == 1] <- "Chest Pain Type 1"
levels(db$cp)[levels(db$cp) == 2] <- "Chest Pain Type 2"
levels(db$cp)[levels(db$cp) == 3] <- "Chest Pain Type 3"
levels(db$restecg)[levels(db$restecg) == 0] <- "Rest ECG 0"
levels(db$restecg)[levels(db$restecg) == 1] <- "Rest ECG 1"
levels(db$restecg)[levels(db$restecg) == 2] <- "Rest ECG 2"
levels(db$slope)[levels(db$slope) == 0] <- "Peak Excercise ST Slope 0"
levels(db$slope)[levels(db$slope) == 1] <- "Peak Excercise ST Slope 1"
levels(db$slope)[levels(db$slope) == 2] <- "Peak Excercise ST Slope 2"

# Analise exploratoria.
ncol(db)
nrow(db)
colnames(db)

names(db)[1] = "age"
str(db)
summary(db)

# Garantir que nao ha valores em falta.
sum(is.na(db))


########## MODELOS ###############

# todas as vars
model.1 = glm(target ~ age+sex+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal, family=binomial, data=db)
summary(model.1)

# sem fbs
model.2 = glm(target ~ age+sex+cp+trestbps+chol+restecg+thalach+exang+oldpeak+slope+ca+thal, data = db, family = binomial())
summary(model.2)

# sem age
model.3 = glm(target ~ sex+cp+trestbps+chol+restecg+thalach+exang+oldpeak+slope+ca+thal, data = db, family = binomial())
summary(model.3) 

# sem chol
model.4 = glm(target ~ sex+cp+trestbps+restecg+thalach+exang+oldpeak+slope+ca+thal, data = db, family = binomial())
summary(model.4)

# sem slope
model.5 = glm(target ~ sex+cp+trestbps+restecg+thalach+exang+oldpeak+ca+thal, data = db, family = binomial())
summary(model.5) # ja sao todas significativas, mas uma e a 10%

# sem restecg, para ficarem todas a 5%
model.6 = glm(target ~ sex+cp+trestbps+thalach+exang+oldpeak+ca+thal, data = db, family = binomial())
summary(model.6) 

# primeiro modelo tirando todas as nao significativas de uma vez
model.7 = glm(target ~ sex+cp+trestbps+thalach+exang+oldpeak+slope+ca+thal, family=binomial, data=db)
summary(model.7)

model.9 = glm(target ~ sex+age+cp+trestbps+thalach+exang+oldpeak+slope+ca+thal, data=db, family=binomial)
summary(model.9)


# Predicao
probs.1 = predict(model.1, type="response");pred.1 = rep(0, 303);pred.1[probs.1 > 0.5] = 1 ;table(pred.1, db$target)
probs.2 = predict(model.2, type="response");pred.2 = rep(0, 303);pred.2[probs.2 > 0.5] = 1 ;table(pred.2, db$target)
probs.3 = predict(model.3, type="response");pred.3 = rep(0, 303);pred.3[probs.3 > 0.5] = 1 ;table(pred.3, db$target)
probs.4 = predict(model.4, type="response");pred.4 = rep(0, 303);pred.4[probs.4 > 0.5] = 1 ;table(pred.4, db$target)
probs.5 = predict(model.5, type="response");pred.5 = rep(0, 303);pred.5[probs.5 > 0.5] = 1 ;table(pred.5, db$target)
probs.6 = predict(model.6, type="response");pred.6 = rep(0, 303);pred.6[probs.6 > 0.5] = 1 ;table(pred.6, db$target)
probs.7 = predict(model.7, type="response");pred.7 = rep(0, 303);pred.7[probs.7 > 0.5] = 1 ;table(pred.7, db$target)
probs.8 = predict(model.8, type="response");pred.8 = rep(0, 303);pred.8[probs.8 > 0.5] = 1 ;table(pred.8, db$target)
probs.9 = predict(model.9, type="response");pred.9 = rep(0, 303);

probs.7 = predict(model.7, type="response");pred.7 = rep(0, 303)
pred.7[probs.7 > 0.53] = 1
mean(pred.7 == db$target)


mean(pred.1 == db$target)
mean(pred.2 == db$target)
mean(pred.3 == db$target)
mean(pred.4 == db$target)
mean(pred.5 == db$target)
mean(pred.6 == db$target)
mean(pred.7 == db$target)
mean(pred.8 == db$target)
mean(pred.9 == db$target)

######### CROSS-VALIDATION #################

library(boot)

mycost = function(r, pi = 0) mean(abs(r-pi)) > 0.55

cv.err1 = c(); cv.err2 = c(); cv.err3 = c(); cv.err4 = c(); cv.err5 = c(); cv.err6 = c(); cv.err7 = c(); cv.err8 = c(); cv.err9 = c()
cv.err1 <- c(cv.err1, cv.glm(db, model.1, cost=mycost, K=303)$delta[1])
cv.err2 <- c(cv.err2, cv.glm(db, model.2, cost=mycost, K=303)$delta[1])
cv.err3 <- c(cv.err3, cv.glm(db, model.3, cost=mycost, K=303)$delta[1])
cv.err4 <- c(cv.err4, cv.glm(db, model.4, cost=mycost, K=303)$delta[1])
cv.err5 <- c(cv.err5, cv.glm(db, model.5, cost=mycost, K=303)$delta[1])
cv.err6 <- c(cv.err6, cv.glm(db, model.6, cost=mycost, K=303)$delta[1])
cv.err7 <- c(cv.err7, cv.glm(db, model.7, cost=mycost, K=303)$delta[1])
cv.err8 <- c(cv.err8, cv.glm(db, model.8, cost=mycost, K=303)$delta[1])
cv.err9 <- c(cv.err9, cv.glm(db, model.9, cost=mycost, K=303)$delta[1])

mean(cv.err1)
mean(cv.err2)
mean(cv.err3)
mean(cv.err4)
mean(cv.err5)
mean(cv.err6)
mean(cv.err7)
mean(cv.err8)
mean(cv.err9)

## O MELHOR MODELO PARECE SER O 2, mas nao e

### TEST

models.1 <- glm(target ~ age, data=db, family=binomial)
models.2 <- glm(target ~ sex, data=db, family=binomial)
models.3 <- glm(target ~ cp, data=db, family=binomial)
models.4 <- glm(target ~ trestbps, data=db, family=binomial)
models.5 <- glm(target ~ chol, data=db, family=binomial)
models.6 <- glm(target ~ fbs, data=db, family=binomial)
models.7 <- glm(target ~ restecg, data=db, family=binomial)
models.8 <- glm(target ~ thalach, data=db, family=binomial)
models.9 <- glm(target ~ exang, data=db, family=binomial)
models.10 <- glm(target ~ oldpeak, data=db, family=binomial)
models.11 <- glm(target ~ slope, data=db, family=binomial)
models.12 <- glm(target ~ ca, data=db, family=binomial)
models.13 <- glm(target ~ thal, data=db, family=binomial)

# sex, age, cp, thalach, exang, oldpeak, slope, ca, thal, todos com *** no p-valor

model.test <- glm(target ~ sex+ age+ cp+ trestbps+ thalach+ exang+ oldpeak+ slope+ ca+ thal, data=db, family=binomial)
summary(model.test)

model.test2 <- glm(target ~ sex+ cp+ trestbps+ thalach+ exang+ oldpeak+ slope+ ca+ thal, data=db, family=binomial)
summary(model.test2)

model.test3 <- glm(target ~ sex+ cp+ trestbps+ thalach+ exang+ oldpeak+ ca+ thal, data=db, family=binomial)
summary(model.test3)

#
probs.test = predict(model.test3, type="response");pred.test = rep(0, 303);pred.test[probs.test > 0.5] = 1 ;table(pred.test, db$target)
mean(pred.test == db$target)
cv.errtest = c()
for(i in 1:1) cv.errtest <- c(cv.errtest, cv.glm(db, model.test3, K=303)$delta[1])
mean(cv.errtest)

# 303 -> 0.1215411
# 101 -> 0.1217713
# 10  -> 0.1226231
# 3   -> 0.1253792 

########### MODEL.TEST3 E O MELHOR MODELO ################