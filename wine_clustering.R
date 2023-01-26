# Pacotes necessários

# Carregando e tratando os dados

wine_data <- read.csv("Wine.csv")
str(wine_data)
wine_data <- wine_data[,1:13]
View(wine_data)
head(wine_data)

# Scatter Plot
pairs(wine_data)

# Normalizing the data

wine_padronizado <-as.data.frame(scale(wine_data[,1:13]))
View(wine_padronizado)

# Calculating the distance
distance <- dist(wine_padronizado)
print(distance,digits=3)

# Dendograma
dindogram <- hclust(distance,method="complete")
plot(dindogram)
