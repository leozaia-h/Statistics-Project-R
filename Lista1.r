url <- "./ENEM_AL_EXCEL_AJUS_OK.csv"
dt <- read.csv(url, header = TRUE, sep = ",")

freq.abs <- table(dt$NOTA_ENEN)
freq.abs.acu <- cumsum(freq.abs)
par (mfrow = c (1,2))
boxplot(freq.abs, main="Boxplot - Frequência Absoluta", col = "yellow")
boxplot(freq.abs.acu, main="Boxplot - Frequência Absoluta Acumulada", col = "orange")
#IMAGEM Q1.1.png
par (mfrow = c (1,2))

freq.rel <- prop.table(filhos.tb)
freq.rel.acu <- cumsum(freq.rel)
boxplot(freq.rel, main="Boxplot - Frequência Relativa", col = "brown")
boxplot(freq.rel.acu, main="Boxplot - Frequência Relativa Acumulada", col = "light blue")
#IMAGEM Q1.2.png

histograma = hist(dt$NOTA_ENEN, ylab="Frequência das notas",  xlab="Notas",main="Histograma", col="orange")
#IMAGEM Q2.png

notas.cut <- cut(dt$NOTA_ENEN, breaks = quantile(dt$NOTA_ENEN))
notas.sexo <- table(dt$TP_SEXO, notas.cut)
barplot(notas_sexo, main = "Notas dividida por sexo", col=c("red", "blue"), beside=TRUE, legend=TRUE, args.legend = list(x = "topright", inset = c(0, -0.4)),cex.axis = 0.8, ylab = "Notas acumuladas") 
legend ("topright", c ("Homem","Mulher"), fill=c("blue", "red")
#IMAGEM Q3.png

media_MT <- mean (dt$NU_NOTA_MT)
media_LC <- mean (dt$NU_NOTA_LC)
media_CN <- mean (dt$NU_NOTA_CN)
media_CH <- mean (dt$NU_NOTA_CH)
media_REDACAO <- mean (dt$NU_NOTA_REDACAO)
lista.medias <- c (media_MT, media_CN, media_LC, media_CH, media_REDACAO)
barnames <- c ("Matemática", "Natureza", "Linguagens", "Humanas", "Redação")
barplot (lista.medias, col=c ("green", "blue", "red", "purple", "yellow"), ylab="Média", main="Média por área", xlab="Área", names.arg=barnames)
#IMAGEM Q4.png

par (mfrow = c (1,2))

qnt.treineiro <- table(dt$IN_TREINEIRO)
labs <- paste (c ("Não =", "Sim = "), round(qnt.treineiro / length(dt$IN_TREINEIRO) * 100, digits=2), "%")
pizza.treineiros <- pie(qnt.treineiro, labels=labs, col=c("blue", "red"), main="Proporção de treineiros", cex=1.1)

qnt.cor.raca <- table (dt$TP_COR_RACA)
labs <- round (qnt.cor.raca / length(dt$TP_COR_RACA) * 100, digits=2)
pizza.cor.raca <- pie (qnt.cor.raca, labels=labs, main="Proporção de raças", cex=1.1, col = rainbow(7), clockwise = TRUE)
legend ("bottomright", c ("Amarelo","Branca","Indigena","Não Declarado", "Pardo", "Preta"), cex = 0.8, fill = rainbow(7))
#IMAGEM Q5.png

red.cut <- cut(dt$NU_NOTA_REDACAO, breaks = quantile(dt$NU_NOTA_REDACAO), include.lowest=TRUE)
inst.sexo.red <- table(dt$TP_SEXO, red.cut)
barplot(inst.sexo.red, col=c("yellow","red",  "orange"), main= "Redação x Sexo", xlab = "Quantiles",ylab = "Frequência  Nota", beside = TRUE, legend = TRUE)
#IMAGEM Q6.png

plot(dt$NU_IDADE, dt$NU_NOTA_REDACAO,  col="red", xlab="Idade", ylab="Notas", main="Relação idade e notas da redação")
#IMAGEM Q7.png

cidade <- subset(dt, NO_MUNICIPIO_RESIDENCIA =='Maceió')

media_MT <- round (mean (cidade$NU_NOTA_MT), digits=2)
media_LC <- round (mean (cidade$NU_NOTA_LC), digits=2)
media_CN <- round (mean (cidade$NU_NOTA_CN), digits=2)
media_CH <- round (mean (cidade$NU_NOTA_CH), digits=2)

par (mfrow = c (1,4))

barplot (media_MT, col = "blue", xlab = "Matemática", names.arg = media_MT)
barplot (media_CH, col = "green", xlab = "Ciências Humanas", names.arg = media_CH)
barplot (media_CN, col = "yellow", xlab = "Ciências da Natureza", names.arg = media_CN)
barplot (media_LC, col = "red", xlab = "Linguagens", names.arg = media_LC)
#IMAGEM Q8.1.png

lin.ext <- table(cidade$TP_LINGUA)
labs <- paste(c("ESPANHOL = ", "INGLÊS = "),round(lin.ext/length(cidade$TP_LINGUA) * 100, digits=2), "%")
pie(lin.ext, col=rainbow(9), labels=labs)
#IMAGEM Q8.2.png
