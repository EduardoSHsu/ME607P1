# PROJETO 1 (P1): MODELO TEÓRICO DE SÉRIES TEMPORAIS

# Lista de pacotes necessários 
pacotes_series <- c("quantmod")

# Instalação e carregamento automático
for (p in pacotes_series) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

# Coleta de Dados via FRED 
dados_fred <- getSymbols("RGDPNACNA666NRUG", src = "FRED", auto.assign = FALSE)
pib_china <- na.omit(dados_fred)

# Transformação 
taxa_crescimento <- diff(log(pib_china))
taxa_crescimento <- na.omit(taxa_crescimento)
taxa_num <- as.numeric(taxa_crescimento)

# 1. Gráfico de Linha: Série Bruta (PIB em Níveis)
plot(pib_china, 
     main = "", 
     ylab = "PIB Real (Milhões, Preços Constantes)", 
     xlab = "Ano",
     col = "darkblue",
     lwd = 2)

# 2. Gráfico de Linha: Série Transformada 
plot(taxa_crescimento, 
     main = "", 
     ylab = "Taxa de Crescimento", 
     xlab = "Ano",
     col = "darkred",
     lwd = 2)
abline(h = mean(taxa_crescimento), col = "black", lty = 2, lwd = 2)

# 3. Gráfico do Espectro: Periodograma Bruto
spectrum(taxa_num, 
         log = "no", 
         main = "", 
         xlab = "Frequência (ciclos por ano)",
         ylab = "Densidade Espectral (Energia)",
         col = "blue",
         lwd = 2)

# 4. Gráficos Lado a Lado: Dualidade Diagnóstica (FAC, FACP vs. Espectro)
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1)) 

acf(taxa_num, 
    main = "", 
    ylab = "Autocorrelação (FAC)", 
    xlab = "Defasagem (Anos)", 
    col = "darkblue", 
    lwd = 2)

pacf(taxa_num, 
     main = "", 
     ylab = "Autocorrelação Parcial (FACP)", 
     xlab = "Defasagem (Anos)", 
     col = "darkgreen", 
     lwd = 2)

spectrum(taxa_num, 
         log = "no", 
         spans = c(3, 3), 
         main = "", 
         xlab = "Frequência (ciclos por ano)",
         ylab = "Densidade Espectral (Suavizada)",
         col = "darkred",
         lwd = 2)

par(mfrow = c(1, 1)) 

# 5. Gráfico do Espectro Detalhado
par(mar = c(5, 4, 5, 2))

spec_suavizado <- spectrum(taxa_num, log = "no", spans = c(3, 3), plot = FALSE)

plot(spec_suavizado$freq, spec_suavizado$spec, type = "l", 
     main = "",
     xlab = "Frequência (ciclos por ano)", 
     ylab = "Densidade Espectral (Energia)", 
     col = "blue", 
     lwd = 2)

frequencias_marcacao <- c(0.1, 0.2, 0.3, 0.4, 0.5)
periodos_marcacao <- round(1 / frequencias_marcacao, 1)

axis(3, at = frequencias_marcacao, labels = periodos_marcacao)
mtext("Período Correspondente (Anos)", side = 3, line = 3, font = 2)

abline(v = 0.20, col = "red", lty = 2, lwd = 1.5)

par(mar = c(5, 4, 4, 2))