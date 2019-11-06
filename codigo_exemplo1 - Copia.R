install.packages('readxl')
install.packages("descr")
library(readxl)
library(descr)
# cdata <- read_excel("~/Documents/UNISINOS/Disciplinas/MAT/ARQUIVOS ESTATISTICA/exemplo1.xlsx")
cdata <- read_excel("exemplo1.xlsx")
cdata <- exemplo1
View(cdata)

# Ajustar categorias da variável resposta e depois verificar comportamento:
cdata$tipo_pagador<-as.factor(ifelse(cdata$tipo_pagador == 1, "Bom", "Ruim"))

# Medidas descritivas para idade:
mean(cdata$A13_idade)
sd(cdata$A13_idade)
median(cdata$A13_idade)

#Histograma 
hist(cdata$A13_idade,nclass=20)

#frequencias
freq(cdata$A9_sexo)

#Gráfico de pizza:
  #Criando uma Data frame com as proporções de cada sexo na base de dados
data.prop<-as.data.frame(table(cdata$A9_sexo))
  #Verificando o output inserido em data.prop
data.prop
  #Criando as proporções para serem utilizadas em labels
labels.prop<-c(round(((data.prop[,2])/sum(data.prop[,2]))*100,2))
  #Criando as porcentagens para utilizarmos no argumento labels
labels.comp<-paste(labels.prop, "%", sep="")
  #Criando o gráfico de pizza para as proporções contidas em data.prop
pie(x=c(data.prop[1,2],data.prop[2,2]), labels=labels.comp, col=c("red", "blue"), main="Proporção entre homens e mulheres")
  #Plotando a legenda no gráfico no canto superos direito
legend("topright", pch=15, col=c("red","blue"), legend=c("Mulheres", "Homens"))

# Avaliando relação de uma variável qualitativa com a variável resposta binária:
x <- xtabs(~ tipo_pagador+ A15_tipo_resid, data = cdata)
cols <- c("#660d32", "#bc1a5e")
# Barras empilhadas.
barplot(x,
        beside = FALSE,
        xlab = "Tipo de residencia",
        ylab = "Frequência absoluta",
        col = cols)
legend("topleft",
       legend = levels(cdata$tipo_pagador),
       fill = cols,
       bty = "n")
box(bty = "L")

# Cria tabela com as proporções
u <- x %*% diag(1/colSums(x))
colnames(u) <- colnames(x)
u

# Barras empilhadas de proporções
barplot(u,
        beside = FALSE,
        xlab = "Tipo de residencia",
        ylab = "Frequência relativa",
        col = cols)
legend("topleft",
       inset = c(0.025, -0.12),
       xpd = TRUE,
       ncol = 2,
       legend = levels(cdata$tipo_pagador),
       fill = cols,
       bty = "n")
box(bty = "L")



# Avaliando relação de uma variável quantitativa com a variável resposta binária:
#Tabela + Gráfico boxplot categorizado:
box_rm <- compmeans(cdata$A5_valor_finan, cdata$tipo_pagador)
box_rm
plot(box_rm, ylab = "Valor do financiamento", xlab = "Situacao do Pagador")


