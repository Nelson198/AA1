# Carregar a base de dados
db <- read.csv(file="database/heart.csv", header=T)

# Tratamento das variáveis qualitativas ordinais.
db$sex <- as.factor(db$sex)
db$cp <- as.factor(db$cp)
db$fbs <- as.factor(db$fbs)
db$exang <- as.factor(db$exang)
db$restecg <- as.factor(db$restecg)
db$slope <- as.factor(db$slope)
db$thal <- as.factor(db$thal)
db$target <- as.factor(db$target)

# Aumento da interpretabilidade das variáveis qualitativas ordinais.
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

# Análise exploratória.
ncol(db)
nrow(db)
colnames(db)

names(db)
str(db)
summary(db)

# Garantir que não há valores em falta.
sum(is.na(db))

# <----- Análise das variáveis ----->
boxplot(db)
boxplot(db$age, db$trestbps, db$chol, db$thalach, names = c("age", "trestbps", "chol", "thalach"))

## age
range(db$age)
summary(db$age)
sd(db$age)
var(db$age)
cor(db$age, db$target)

hist(db$age, labels=TRUE, main="Histogram of Age", xlab="Age Class", ylab="Frequency", col="blue")
boxplot(db$age, horizontal=TRUE, col="red", main="Boxplot of Age")

## sex
a = table(db$sex)
par(mfrow = c(1,2))
barplot(a,
        col = rainbow(2),
        legend = rownames(a),
        xlab = "Female and Male",
        ylab = "Count",
        main = "Barplot of SEX")

pct = round(a/sum(a)*100)
lbs = paste(c("Female", "Male"), " ", pct, "%", sep=" ")
pie(a, labels=lbs, main="Percentage of Male and Female")

b = table(db$sex, db$target)
barplot(b,
        col = c("red","blue"),
        legend = rownames(b),
        beside = TRUE,
        xlab = "Target",
        ylab = "Count",
        main = "Side by Side Barplot")

## cp (Chest Pain)
d = table(db$cp)
barplot(d,
        col = rainbow(5),
        main = "Barplot of Chest pain",
        xlab = "Chest Pain Category",
        ylab = "Count",
        legend = rownames(d))

## trestbps (Rest Blood Pressure)
summary(db$trestbps)
cor(db$trestbps,db$age)
boxplot(db$trestbps,
        col="pink",
        main="Descriptive Analysis of RBP",
        horizontal=TRUE)

hist(db$trestbps,col=rainbow(7),
     main="Histogram for RBP",
     xlab="Rest Blood Pressure Class",
     ylab="Frequency",
     labels=TRUE)

## chol (Cholestrol)
summary(db$chol)
boxplot(db$chol ~ db$sex, col=rainbow(2), 
        main="Descriptive Analysis of Chol with Male and Female",
        xlab="Female or Male",
        ylab="Cholestrol")

hist(db$chol,
     main="Histogram of Cholestrol",
     xlab="Cholestrol Class",
     ylab="Frequency",
     col=rainbow(7),labels=TRUE)

## fbs (Fasting Blood Sugar)
summary(db$fbs)
class(db$fbs)
head(db$fbs,10)
h = table(db$fbs)
par(mfrow = c(1,2))
barplot(h,
        main="Barplot for FBS",
        xlab="FBS",
        ylab="Count",
        col=rainbow(2),
        legend=rownames(h))

pct = round(h/sum(h)*100)
lbs = paste(c("fbs<120","fbs>120"), " ", pct, "%", sep=" ")
pie(h, lbs, main="Pie Chart of FBS", col=c("pink", "blue"))

## restecg (Resting ECG)
unique(db$restecg)
j = table(db$restecg)
barplot(j,
        main = "Barplot of Rest ECG",
        xlab = "ECG Category",
        ylab = "Count",
        col = rainbow(3),
        legend = rownames(j))

## thalach (Maximum Heart Rate)
class(db$thalach)
head(db$thalach)
summary(db$thalach)
boxplot(db$thalach ~ db$sex,
        main = "Boxplot of Thalac with Sex Category",
        xlab = "Sex: Female or Male",
        ylab = "Maximum Heart Rate Achieved",
        col = rainbow(2))
# Chi square test to see how age and Heart rate is corelating
chisq.test(db$age, db$thalach)
# Let us see how age is effecting Heart Rate
cor(db$age, db$thalach)

## exang (Exercise Induced Angina)
k = table(db$exang, db$target)
k
barplot(k,
        legend = rownames(k),
        col = rainbow(2),
        main = "Stacked Barplot of Exang and Target",
        xlab = "Target",
        ylab = "Count")

## oldpeak
hist(db$oldpeak,
     main = "Histogram of Oldpeak",
     xlab = "Oldpeak Class",
     ylab = "Frequency",
     col = rainbow(7),
     labels = TRUE)

plot(density(db$oldpeak),
     main = "Density plot of Oldpeak",
     xlab = "Oldpeak",
     ylab = "Density")

polygon(density(db$oldpeak), col="orange", border="green")

boxplot(data$oldpeak,
        main="Descriptive Analysis of Oldpeak",col="pink")

boxplot(data$oldpeak~data$sex,
        main="Descriptive Analysis of Oldpeak",col=rainbow(2),xlab="0: Feamle, 1:Male",
        ylab="Oldpeak")

## slope
l = table(db$slope)
l
barplot(l,
        main = "Barplot of Slope",
        xlab = "Slope",
        ylab = "Count",
        col = "pink")

m = table(db$slope, db$target)
barplot(m,
        main = "Stacked barplot of slope and target",
        col = rainbow(3),
        xlab = "Target",
        ylab = "Count",
        legend = rownames(m))

## ca
n = table(db$ca)
barplot(n,
        main ="Barplot of ca",
        xlab = "ca",
        col = rainbow(5),
        legend = rownames(n),
        ylab = "Count")
o = table(db$target, db$ca)
barplot(o,
        main = "Stacked Barplot of ca and Target",
        xlab = "ca",
        col = rainbow(2),
        legend = rownames(o),
        ylab = "Count")
# <----- Análise das variáveis ----->

# Criação de modelos para averiguar se existe uma relação entre cada um dos preditores e a variável de interesse.
model = glm(target ~ ., data = db, family = binomial())
summary(model)

newModel = glm(target ~ sex + cp + thalach + oldpeak + ca, db, family=binomial())
summary(newModel)

# Predição
pred = predict(newModel, db, type="response")

predNew = as.data.frame(pred)

categorise = function(x) {
  return(ifelse(x>0.5,1,0))
}

predNew = apply(predNew, 2, categorise)
head(predNew, 10)

# Avaliação do modelo

