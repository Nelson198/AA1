# Carregar a base de dados
db <- read.csv(file="database/heart.csv", header=T)

ncol(db)
nrow(db)
colnames(db)

# Garantir que nao existem valores em falta.
sum(is.na(db))

########## MODELOS ###############

# todas as variáveis
model.1 = glm(target ~ ., family=binomial, data=db)
summary(model.1)

# sem fbs
model.2 = glm(target ~ age+sex+cp+trestbps+chol+restecg+thalach+exang+oldpeak+slope+ca+thal, data = db, family = binomial)
summary(model.2)

# sem age
model.3 = glm(target ~ sex+cp+trestbps+chol+restecg+thalach+exang+oldpeak+slope+ca+thal, data = db, family = binomial)
summary(model.3) 

# sem chol
model.4 = glm(target ~ sex+cp+trestbps+restecg+thalach+exang+oldpeak+slope+ca+thal, data = db, family = binomial)
summary(model.4)

# sem slope
model.5 = glm(target ~ sex+cp+trestbps+restecg+thalach+exang+oldpeak+ca+thal, data = db, family = binomial)
summary(model.5) # já sao todas significativas, mas uma é a 10%

# sem restecg, para ficarem todas a 5%
model.6 = glm(target ~ sex+cp+trestbps+thalach+exang+oldpeak+ca+thal, data = db, family = binomial)
summary(model.6) 

# primeiro modelo tirando todas as vars não significativas
model.7 = glm(target ~ sex+cp+trestbps+thalach+exang+oldpeak+slope+ca+thal, family=binomial, data=db)
summary(model.7)


############ TESTES #################

model.test1 <- glm(target ~ age, data=db, family=binomial)
model.test2 <- glm(target ~ sex, data=db, family=binomial)
model.test3 <- glm(target ~ cp, data=db, family=binomial)
model.test4 <- glm(target ~ trestbps, data=db, family=binomial)
model.test5 <- glm(target ~ chol, data=db, family=binomial)
model.test6 <- glm(target ~ fbs, data=db, family=binomial)
model.test7 <- glm(target ~ restecg, data=db, family=binomial)
model.test8 <- glm(target ~ thalach, data=db, family=binomial)
model.test9 <- glm(target ~ exang, data=db, family=binomial)
model.test10 <- glm(target ~ oldpeak, data=db, family=binomial)
model.test11 <- glm(target ~ slope, data=db, family=binomial)
model.test12 <- glm(target ~ ca, data=db, family=binomial)
model.test13 <- glm(target ~ thal, data=db, family=binomial)

# Modelo construído a partir da análise anterior
model.8 = glm(target ~ sex+age+cp+trestbps+thalach+exang+oldpeak+slope+ca+thal, data=db, family=binomial)
summary(model.8)

## Qual o melhor modelo?
# Proporção de classificações corretas
probs.1 = predict(model.1, type="response");pred.1 = rep(0, 303);pred.1[probs.1 > 0.55] = 1 
probs.2 = predict(model.2, type="response");pred.2 = rep(0, 303);pred.2[probs.2 > 0.55] = 1 
probs.3 = predict(model.3, type="response");pred.3 = rep(0, 303);pred.3[probs.3 > 0.55] = 1 
probs.4 = predict(model.4, type="response");pred.4 = rep(0, 303);pred.4[probs.4 > 0.55] = 1 
probs.5 = predict(model.5, type="response");pred.5 = rep(0, 303);pred.5[probs.5 > 0.55] = 1 
probs.6 = predict(model.6, type="response");pred.6 = rep(0, 303);pred.6[probs.6 > 0.55] = 1 
probs.7 = predict(model.7, type="response");pred.7 = rep(0, 303);pred.7[probs.7 > 0.55] = 1
probs.8 = predict(model.8, type="response");pred.8 = rep(0, 303);pred.8[probs.8 > 0.55] = 1

mean(pred.1 == db$target)
mean(pred.2 == db$target)
mean(pred.3 == db$target)
mean(pred.4 == db$target)
mean(pred.5 == db$target)
mean(pred.6 == db$target)
mean(pred.7 == db$target)
mean(pred.8 == db$target)

# Cross-validation - Leave-one-out
library(boot)

mycost = function(r, pi = 0) mean(abs(r-pi)) > 0.55

cv.err1 = c(); cv.err2 = c(); cv.err3 = c(); cv.err4 = c()
cv.err5 = c(); cv.err6 = c(); cv.err7 = c(); cv.err8 = c()

s = nrow(db)

cv.err1 <- c(cv.err1, cv.glm(db, model.1, cost=mycost, K=s)$delta[1])
cv.err2 <- c(cv.err2, cv.glm(db, model.2, cost=mycost, K=s)$delta[1])
cv.err3 <- c(cv.err3, cv.glm(db, model.3, cost=mycost, K=s)$delta[1])
cv.err4 <- c(cv.err4, cv.glm(db, model.4, cost=mycost, K=s)$delta[1])
cv.err5 <- c(cv.err5, cv.glm(db, model.5, cost=mycost, K=s)$delta[1])
cv.err6 <- c(cv.err6, cv.glm(db, model.6, cost=mycost, K=s)$delta[1])
cv.err7 <- c(cv.err7, cv.glm(db, model.7, cost=mycost, K=s)$delta[1])
cv.err8 <- c(cv.err8, cv.glm(db, model.8, cost=mycost, K=s)$delta[1])
cv.err9 <- c(cv.err9, cv.glm(db, model.9, cost=mycost, K=s)$delta[1])

cv.err1
cv.err2
cv.err3
cv.err4
cv.err5
cv.err6
cv.err7
cv.err8

