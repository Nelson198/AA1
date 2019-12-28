db <- read.csv(file="database/crime.csv", header=T)

# Remover variáveis que dizem respeito às coordenadas dos locais dos crimes.
db <- db[, -(15:17), drop=FALSE]

# Análise exploratória.
names(db)
str(db)
summary(db)

# Nº de registos de crimes em cada mês desde 2015 até 2019.
table(db$MONTH, db$YEAR)

# Gráfico que evidencia a distribuição da criminalidade ao longo das horas desde 2015 até 2019.
barplot(table(db$HOUR), xlab = "Nº da hora", ylab = "Nº de incidentes reportados", main = "Evolução dos incidentes reportados pela hora do dia")

# Gráfico que evidencia a distribuição da criminalidade ao longo dos meses desde 2015 até 2019.
barplot(table(db$MONTH), xlab = "Mês", ylab = "Nº de incidentes reportados", main = "Evolução dos incidentes reportados ao longo dos meses")

# Gráfico que evidencia a distribuição da criminalidade ao longo dos anos (2015-2019).
barplot(table(db$YEAR), xlab = "Ano", ylab = "Nº de incidentes reportados", main = "Evolução dos incidentes reportados ao longo dos anos")

# Nº de incidentes criminosos associados a cada código de ofensa.
table(db$OFFENSE_CODE)

# Nº de incidentes criminosos associados à descrição especificada.
sum(db$OFFENSE_DESCRIPTION == "INVESTIGATE PERSON")
length(which(db$OFFENSE_DESCRIPTION == "INVESTIGATE PERSON"))

# Descrição do incidente mais reportado pela polícia de Boston durante a periodicidade completa da base de dados.
occurrencesMAX <- max(table(db$OFFENSE_DESCRIPTION))
names(which.max(table(db$OFFENSE_DESCRIPTION)))

# Descrição do incidente menos reportado pela polícia de Boston durante a periodicidade completa da base de dados.
occurrencesMIN <- min(table(db$OFFENSE_DESCRIPTION))
names(which.min(table(db$OFFENSE_DESCRIPTION)))