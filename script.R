db <- read.csv(file="database/crime.csv", header=T)

# Remover variáveis que dizem respeito às coordenadas dos locais dos crimes.
db <- db[, -(15:17), drop=FALSE]

# Análise exploratória.
names(db)
str(db)
summary(db)

# Nº de registos de crimes em cada mês desde 2015 até 2019.
table(db$MONTH, db$YEAR)