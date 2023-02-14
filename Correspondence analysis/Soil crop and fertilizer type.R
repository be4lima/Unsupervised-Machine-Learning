# Análise de correspondência 
## Fonte dos dados: https://www.kaggle.com/code/nandinibagga/fertilizer-type-prediction/data

# Instalação e carregamento dos pacotes utilizados
pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "sjPlot", #elaboração de tabelas de contingência
             "FactoMineR", #função 'CA' para elaboração direta da Anacor
             "amap", #funções 'matlogic' e 'burt' para matrizes binária e de Burt
             "ade4") #função 'dudi.acm' para elaboração da ACM

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
library(magrittr)
library(kableExtra)

# Importando os dados
fertilizer_data <- read.csv("Correspondence Analysis/Fertilizer Prediction.csv")

# Visualização dos dados
fertilizer_data %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

#----------------- Análise de Correspondência Simples (ANACOR) -----------------

## 1ª Parte: Análise da associação por meio de tabelas

# Tabela de contingência com frequências absolutas observadas
tabela_contingencia <- table(fertilizer_data$Soil.Type,
                             fertilizer_data$Crop.Type)
tabela_contingencia

# Definição da quantidade de observações na tabela de contingência
n <- sum(tabela_contingencia)
n

# Estatística qui-quadrado e teste
qui2 <- chisq.test(x = tabela_contingencia)
qui2 # P-value < 5%

# Tabela de contingência com frequências absolutas observadas
qui2$observed

# Tabela de contingência com frequências absolutas esperadas
qui2$expected

# Tabela de contingência com frequências absolutas observadas e esperadas
sjt.xtab(var.row = fertilizer_data$Soil.Type,
         var.col = fertilizer_data$Crop.Type,
         show.exp = TRUE)

# Resíduos – diferenças entre frequências absolutas observadas e esperadas
qui2$observed - qui2$expected

# Resíduos padronizados
qui2$residuals

# Resíduos padronizados ajustados
qui2$stdres
