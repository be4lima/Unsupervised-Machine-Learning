# Clustering countries from percentage of urban population

pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "misc3d", #gráficos 3D
             "plot3D", #gráficos 3D
             "cluster", #função 'agnes' para elaboração de clusters hierárquicos
             "factoextra", #função 'fviz_dend' para construção de dendrogramas
             "ade4") #função 'ade4' para matriz de distâncias em var. binárias

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Loading rawdata
db <- read.csv("C:/Users/beatr/OneDrive/Documentos/MBA Data Science and Analytics/Módulo 1/Clustering/archive/country_profile_variables.csv")
View(db)

# Dataframe
percentage_urban_population <- db$Urban.population....of.total.population.
df <- cbind(db[1:2], percentage_urban_population)
View(df)

# Filtering just South America countries
south_america <- df %>%
  filter(Region == "SouthAmerica")

# Visualization
row.names(south_america) <- south_america$country
south_america %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

#---------- Esquema de aglomeração hierárquico ---------------------------------

#Matriz de Dissimilaridades
matriz_D <- south_america %>%
  select(percentage_urban_population) %>%
  dist(method = "euclidean")

# Method: parametrização da distância a ser utilizada

## "euclidean": distância euclidiana
## "euclidiana quadrática": elevar ao quadrado matriz_D (matriz_D^2)
## "maximum": distância de Chebychev;
## "manhattan": distância de Manhattan (ou distância absoluta ou bloco);
## "canberra": distância de Canberra;
## "minkowski": distância de Minkowski

data.matrix(matriz_D) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)
