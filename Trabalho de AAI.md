# Trabalho de AAI

### O problema (a rever/completar)

Grande parte dos cidadãos queixam-se da intervenção da polícia nas suas cidades/países. Ou porque não é suficiente, ou porque é demasiado violenta. Como podemos melhorar a intervenção das autoridades?

Vamos estudar as intervenções policiais na cidade de Boston, Estados Unidos, com o intuito de perceber em que tipo de ocorrências estes mais intervêm e procurar entender se essas intervenções têm sido eficazes, ou se podem ser melhoradas.

### Perguntas

Qual a evolução, por mês e por ano, do tipo ocorrências a que a polícia respondeu?

Aumentar/Diminuíram os acidentes? Aumentaram/Diminuíram os crimes violentos?

Qual o mês do ano mais propício a crimes? E em que existem mais acidentes? E outras ocorrências?

O dia da semana influencia a quantidade de ocorrências?

Qual a distribuição de ocorrências por hora?

Qual o tipo de ocorrência mais registadas?

É possível fazer a predição da evolução de crimes/incidências para os anos seguintes?



```R
db <- read.csv("crimes.csv", header=T)
names(db)
str(db)
```



### O método

