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

# Mapa de calor dos resíduos padronizados ajustados
data.frame(qui2$stdres) %>%
  rename(Soil.Type = 1,
         Crop.Type = 2) %>%
  ggplot(aes(x = fct_rev(Soil.Type), y = Crop.Type,
                         fill = Freq, label = round(Freq, 3))) +
           geom_tile () +
           geom_text(size = 5) +
           scale_fill_gradient2(low = "white", 
                                mid = "white", 
                                high = "purple",
                                midpoint = 1.96) +
           labs( x = 'Tipo de solo', y = 'Tipo de cobertura', fill = "Res. Pad. Ajustados") +
           coord_flip() +
           theme_bw()

## 2ª Parte: Análise da associação por meio do mapa perceptual

# Definição da matriz A
# Resíduos padronizados (qui2$residuals) divididos pela raiz quadrada do tamanho da amostra (n)
matrizA <- qui2$residuals/sqrt(n)
matrizA

# Definição da matriz W
# Multiplicação da matriz A transposta pela matriz A
matrizW <- t(matrizA) %*% matrizA
matrizW

# Definição da quantidade de dimensões
qtde_dimensoes <- min(nrow(matrizW) - 1, ncol(matrizW) - 1)
qtde_dimensoes

# Definição dos valores singulares
VS_AV <- svd(matrizA, nu = qtde_dimensoes, nv = qtde_dimensoes)

# Valores singulares de cada dimensão
valores_singulares <- VS_AV$d[1:qtde_dimensoes]
valores_singulares

# Autovalores (eigenvalues) de cada dimensão
eigenvalues <- (valores_singulares)^2
eigenvalues

# Cálculo da inércia principal total (a partir do qui-quadrado)
inercia_total <- as.numeric(qui2$statistic/sum(tabela_contingencia))
inercia_total

# Cálculo da variância explicada em cada dimensão
variancia_explicada <- eigenvalues / inercia_total
variancia_explicada

# Cálculo das massas das colunas (column profiles)
soma_colunas <- apply(tabela_contingencia, MARGIN = 1, FUN = sum)
soma_colunas

# Massas das colunas (column profiles)
massa_colunas <- soma_colunas / n
massa_colunas

# Cálculo das massas das linhas (row profiles)
soma_linhas <- apply(tabela_contingencia, MARGIN = 2, FUN = sum)
soma_linhas

# Massas das linhas (row profiles)
massa_linhas <- soma_linhas / n
massa_linhas

# Autovetores v das dimensões
autovetor_v <-VS_AV$v
autovetor_v

# Autovetores u das dimensões
autovetor_u <-VS_AV$u
autovetor_u

# Resumindo as informações até aqui
data.frame(Dimensão = paste("Dimensão", 1:qtde_dimensoes),
           `Valor Singular` = valores_singulares,
           `Inércia Principal Parcial eigenvalues` = eigenvalues) %>%
  mutate(`Percentual da Inércia Principal Total` = (`Inércia.Principal.Parcial.eigenvalues`/inercia_total) * 100,
         `Percentual da Inércia Principal Total Acumulada` = cumsum(`Percentual da Inércia Principal Total`),
         Qui2 = qui2$statistic[[1]] * `Percentual da Inércia Principal Total` / n,
         `Valor Singular` = `Valor.Singular`,
         `Inércia Principal Parcial eigenvalues` = Inércia.Principal.Parcial.eigenvalues) %>%
  select(Dimensão, `Valor Singular`, `Inércia Principal Parcial eigenvalues`,
         Qui2, `Percentual da Inércia Principal Total`,
         `Percentual da Inércia Principal Total Acumulada`) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 17)

# Calculando as coordenadas para plotar as categorias no mapa perceptual

# Variável em linha na tabela de contingência ('perfil')
# Coordenadas das abcissas
coord_abcissas_soil <- sqrt(valores_singulares[1]) * (massa_colunas^-0.5) * autovetor_u[,1]
coord_abcissas_perfil

# Coordenadas das ordenadas
coord_ordenadas_soil <- sqrt(valores_singulares[2]) * (massa_colunas^-0.5) * autovetor_u[,2]
coord_ordenadas_soil

# Variável em coluna na tabela de contingência ('aplicacao')
# Coordenadas das abcissas
coord_abcissas_crop <- sqrt(valores_singulares[1]) * (massa_linhas^-0.5) * autovetor_v[,1]
coord_abcissas_crop

# Coordenadas das ordenadas
coord_ordenadas_crop <- sqrt(valores_singulares[2]) * (massa_linhas^-0.5) * autovetor_v[,2]
coord_ordenadas_crop

# Mapa perceptual
# O resultado pode ser obtido por meio da função 'CA' do pacote 'FactoMineR'
anacor <- CA(tabela_contingencia, graph = TRUE)
