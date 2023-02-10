# Instalação e carregamento dos pacotes utilizados

pacotes <- c("plotly", "GGally", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "PerformanceAnalytics", #função 'chart.Correlation' para plotagem
             "psych", #elaboração da fatorial e estatísticas
             "ltm", #determinação do alpha de Cronbach pela função 'cronbach.alpha'
             "Hmisc", # matriz de correlações com p-valor
             "readxl") # leitura de dados em Excel

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Análise exploratória dos dados
data <- read.csv("breast_cancer_classification.csv")
str(data)

# Visualização
View(data)
data %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Ajuste da base de dados
dados_ajustados <-data[,3:32]
dados_ajustados %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Correlação de Pearson


rho <- rcorr(as.matrix(dados_ajustados),type="pearson") # Coeficiente de correlação
matriz_corr<-rho$r # Matriz de correlações
valorp_coef <-rho$P # Matriz com p-valor dos coeficientes

# Visualização das distribuições das variáveis, scatters, valores das correlações
library(PerformanceAnalytics)
chart.Correlation(dados_ajustados,histogram = TRUE,col="grey")

# Mapa de calor com as correlações
ggcorr(dados_ajustados, name="corr", label=TRUE, label_size = 2, label_round=2, hjust=1) +
  labs(title="Breast cancer classification") +
  theme(plot.title=element_text(face='bold', color='black', hjust=0.5, size=8))

# Teste de esfericidade de Bartlett
cortest.bartlett(dados_ajustados)

## p-value menor que 5% -> rejeita a hipótese nula de que a matriz de dados é similar a uma matriz identidade ##

#############################################################
# Elaboração da análise fatorial por componentes principais #
fatorial <- principal(dados_ajustados,
                      nfactors = length(dados_ajustados),
                      rotate = "none",
                      scores = TRUE)
fatorial

# Eigenvalues (autovalores)
eigenvalues <- round(fatorial$values, 2)
eigenvalues

# Identificação da variância compartilhada em cada fator
variancia_compartilhada <- as.data.frame(fatorial$Vaccounted) %>% 
  slice(1:3)
rownames(variancia_compartilhada) <- c("Autovalores",
                                       "Prop. da Variância",
                                       "Prop. da Variância Acumulada")

# Variância compartilhada pelas variáveis originais para a formação de cada fator
round(variancia_compartilhada, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo dos scores fatoriais
fatores <- as.data.frame(fatorial$scores)

View(fatores)

# Coeficientes de correlação de Pearson para cada par de fatores (ortogonais)
rho <- rcorr(as.matrix(fatores), type="pearson")
round(rho$r, 4)

# Cálculo das cargas fatoriais
cargas_fatoriais <- as.data.frame(unclass(fatorial$loadings))

# Visualização das cargas fatoriais
round(cargas_fatoriais, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo das comunalidades
comunalidades <- as.data.frame(unclass(fatorial$communality)) %>%
  rename(comunalidades = 1)

# Visualização das comunalidades (analisando os 4 fatores, são iguais a 1)
round(comunalidades, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

### Fatores extraídos a partir de autovalores maiores que 1 ###
# Definição da quantidade de fatores com eigenvalues maiores que 1
k <- sum(eigenvalues > 1)
print(k)

# Elaboração da análise fatorial por componentes principais
# Com quantidade 'k' de fatores com eigenvalues maiores que 1
fatorial1 <- principal(dados_ajustados,
                       nfactors = k,
                       rotate = "none",
                       scores = TRUE)
fatorial1

# Cálculo das cargas fatoriais
cargas_fatoriais1 <- as.data.frame(unclass(fatorial1$loadings))

# Visualização das cargas fatoriais
round(cargas_fatoriais1, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)
# Cálculo das comunalidades com o primeiro fator ('k' = 1)
comunalidades1 <- as.data.frame(unclass(fatorial1$communality)) %>%
  rename(comunalidades = 1)

# Visualização das comunalidades
round(comunalidades1, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Cálculo dos scores fatoriais
scores_fatoriais1 <- as.data.frame(fatorial1$weights)

# Visualização dos scores fatoriais
round(scores_fatoriais1, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo do fator
fatores1 <- as.data.frame(fatorial1$scores)

# Adicionando o fator extraído ao banco de dados original
dados_ajustados <- bind_cols(dados_ajustados,
                            "fator1" = fatores1$PC1)
