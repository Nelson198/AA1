db <- read.csv(file="database/heart.csv", header=T)

# Análise exploratória.
ncol(db)
nrow(db)

names(db)
str(db)
summary(db)

# Nº de pessoas que possui ou não doença cardíaca.
table(db$target)
barplot(table(db$target), ylim=c(0, 200), xlab = "Doença cardíaca?", ylab = "Número de pessoas", main="Nº de pessoas com doença cardíaca", col="orange", border = "orange", space=5)
text(5.5, 142, 136)
text(11.5, 169, 165)

