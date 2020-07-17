# Header----
# Grupo de Estudos sobre Atividades Acadêmicas - GESAA
# Universidade Federal da Grande Dourados
# Suporte ao texto 
# Estatísticas bem-vindas, outras nem tanto!
# Josué Raizer, professor da Faculdade de Ciências Biológicas e Ambientais

## Preparação
options(OutDec = ",") #vírgula como separador decimal nos resultados

## Simulação de alturas de 500 homens e 500 mulheres----
set.seed(7) #semente randômica para obter o mesmo resultado nas aleatorizações

### Alturas simuladas para homens----
### Amostra (pseudo)aleatória de 500 valores a partir de uma 
### distribuição normal com média 1,75 e desvio padrão 0,05:
(alturas_h <- round(rnorm(500, 1.75, 0.05), 2))


### Alturas simuladas para mulheres----
### Amostra (pseudo)aleatória de 500 valores a partir de uma 
### distribuição normal com média 1,6 e desvio padrão 0,05:
(alturas_m <- round(rnorm(500, 1.6, 0.05), 2)) 

### juntando as alturas:
alturas <- c(alturas_h, alturas_m) #todas alturas
round(summary(alturas), 2) #resumo estatístico

### Figura PNG da distribuição das 1000 alturas simuladas----
png("alturas_mil.png", width = 20, height = 15, units = "cm", res = 300)
par(mar = c(4, 1, 1, 1))
stripchart(alturas_h, 
           method = "stack",
           offset = 0.5,
           at = -.045,
           frame.plot = F,
           xpd = T,
           xlim = c(min(alturas), max(alturas)),
           xlab = "Alturas (m)",
           pch = -0x2642L,
           lwd = 1.5,
           cex = .9,
           col = "blue") 
stripchart(alturas_m, 
           method = "stack", 
           offset = 0.5,
           at = -.045,
           pch = -0x2640L,
           frame.plot = F,
           xpd = T,
           add = T,
           lwd = 1.5,
           cex = .9, 
           col = "blue") 
abline(v = c(1.63, 1.73), col = "blue", lwd = 2)
abline(v = 1.68, col = "blue", lty = 2, lwd = 2)
dev.off()



# Considerando-se uma precisão de 5 cm para as estimativas de altura, a chance 
# de uma pessoa ter altura entre 1,63 m e 1,73 m, ou seja, 5 cm ao redor da média 
# de 1,68 m na população de 1000 funcionários, é de aproximadamente 25% (nesta 
# simulação 256 em 1000). 
length(which(alturas > 1.63 & alturas < 1.73)) # frequência absoluta
length(which(alturas > 1.63 & alturas < 1.73))/10 # porcentagem

# Isso significa que uma amostra de uma pessoa tem uma chance aproximada de 25 % 
# de 'estimar bem' (precisão: 5 cm) a altura média na população de 
# 1000 funcionários.

### Função simples para extrair o número de observações dessa distribuição entre
### 1,63 m e 1,73 m
tam_amo <- function(dados, rep, n){
  for(i in 1:n){
    x <- replicate(rep, mean(sample(dados, i)))
  }
  length(which(x > 1.63 & x < 1.73))
  }

y <- numeric()
for(i in 1:20){
  y[i] <- tam_amo(alturas, 100, i)
}


### Figura PNG representatividade pelo tamanho amostral----
png("tam_amo.png", width = 17, height = 15, units = "cm", res = 300)
par(mar = c(4, 4, 3, 1))
plot(y, 
     xlab = "Tamanho amostral", 
     ylab = "Chance de obter 'boa estimativa'", 
     bty = "l",
     main = "precisão de 5 cm",
     col = "blue",
     cex = 2,
     lwd = 1.5,
     col.main = "blue")
abline(h = 100, col = "gray", lty = 2)
dev.off()


## Aumentando a precisão (estreitando o intervalo)----
length(which(alturas >= 1.67 & alturas <= 1.69)) # frequência absoluta
length(which(alturas >= 1.67 & alturas <= 1.69))/10 # porcentagem

png("alturas_mil2.png", width = 20, height = 15, units = "cm", res = 300)
par(mar = c(4, 1, 1, 1))
stripchart(alturas_h, 
           method = "stack",
           offset = 0.5,
           at = -.045,
           frame.plot = F,
           xpd = T,
           xlim = c(min(alturas), max(alturas)),
           xlab = "Alturas (m)",
           pch = -0x2642L,
           lwd = 1.5,
           cex = .9,
           col = "blue") 
stripchart(alturas_m, 
           method = "stack", 
           offset = 0.5,
           at = -.045,
           pch = -0x2640L,
           frame.plot = F,
           xpd = T,
           add = T,
           lwd = 1.5,
           cex = .9, 
           col = "blue") 
abline(v = c(1.67, 1.69), col = "blue", lwd = 2)
abline(v = 1.68, col = "lightblue", lty = 2)
dev.off()


### Função simples para extrair o número de observações dessa distribuição entre
### 1,67 m e 1,69 m
tam_amo2 <- function(n){
  for(i in 1:n){
    x <- replicate(100, mean(sample(alturas, i)))
  }
  length(which(x >= 1.67 & x <= 1.69))
}

y2 <- numeric()
for(i in 1:200){
  y2[i] <- tam_amo2(i)
}


### Figuras PNG representatividade maior precisão----
png("tam_amo2.png", width = 17, height = 15, units = "cm", res = 300)
par(mar = c(4, 4, 3, 1))
plot(y2, 
     xlab = "Tamanho amostral", 
     ylab = "Chance de obter 'boa estimativa'", 
     bty = "l",
     main = "precisão de 1 cm",
     col = "blue",
     cex = 2,
     lwd = 1.5,
     col.main = "blue")
abline(h = 80, col = "gray", lty = 2)
dev.off()

png("tam_amo3.png", width = 17, height = 15, units = "cm", res = 300)
par(mar = c(4, 1, 1, 5))
plot(1:10,
     type = "l",
     xaxt = "n",
     yaxt = "n",
     xlab = "Tamanho amostral",
     ylab = NA,
     bty = "l",
     lwd = 2,
     col = "blue")
text(10.5, 8, "Chance de obter\numa 'boa estimativa'", 
       col = "blue", 
       xpd = T)
lines(10:1,
      lwd = 2)
text(10.5, 2, "Viabilidade", 
     xpd = T)
segments(mean(1:10), 0, mean(1:10), mean(1:10), lty = 2, col = "gray", lwd = 2)
text(mean(1:10), 0, "ótimo", col = "gray30", xpd = T)
dev.off()

set.seed(7)
sex_amo <- replicate(250, 
                     length(which(sample(rep(c("mulheres", "homens"), 
                                             each = 500), 100) == "mulheres")))
png("sex_amo.png",  width = 17, height = 15, units = "cm", res = 300)
par(mar = c(4, 1, 2, 1))
stripchart(sex_amo, 
           method = "stack",
           offset = 0.5,
           at = 0,
           frame.plot = F,
           xpd = T,
           xlim = c(floor(min(sex_amo)) - 1, ceiling(max(sex_amo)) + 1),
           xlab = "Número de mulheres na amostra (%)",
           main = "250 amostras de 100 pessoas",
           pch = 21,
           lwd = 1.5,
           cex = 1.5) 
dev.off()

png("sex_amo_ep1.png",  width = 17, height = 15, units = "cm", res = 300)
par(mar = c(4, 1, 2, 1))
stripchart(sex_amo, 
           method = "stack",
           offset = 0.5,
           at = 0,
           frame.plot = F,
           xpd = T,
           xlim = c(floor(min(sex_amo)) - 1, ceiling(max(sex_amo)) + 1),
           xlab = "Número de mulheres na amostra (%)",
           main = "250 amostras de 100 pessoas",
           pch = 21,
           lwd = 1.5,
           cex = 1.5) 
abline(v = c(mean(sex_amo) - sd(sex_amo), 
             mean(sex_amo) + sd(sex_amo)), col = "blue", lwd = 2)
dev.off()

png("sex_amo_ep2.png",  width = 17, height = 15, units = "cm", res = 300)
par(mar = c(4, 1, 2, 1))
stripchart(sex_amo, 
           method = "stack",
           offset = 0.5,
           at = 0,
           frame.plot = F,
           xpd = T,
           xlim = c(floor(min(sex_amo)) - 1, ceiling(max(sex_amo)) + 1),
           xlab = "Número de mulheres na amostra (%)",
           main = "250 amostras de 100 pessoas",
           pch = 21,
           lwd = 1.5,
           cex = 1.5) 
abline(v = c(mean(sex_amo) - sd(sex_amo), 
             mean(sex_amo) + sd(sex_amo)), col = "lightblue", lwd = 2)
abline(v = c(mean(sex_amo) - 2 * sd(sex_amo), 
             mean(sex_amo) + 2 * sd(sex_amo)), col = "red", lwd = 2)
dev.off()

summary(sex_amo)
length(which(sex_amo > (mean(sex_amo) - sd(sex_amo)) & 
               sex_amo < (mean(sex_amo) + sd(sex_amo)))) 
174/250
length(which(sex_amo > (mean(sex_amo) - 2 * sd(sex_amo)) & 
               sex_amo < (mean(sex_amo) + 2 * sd(sex_amo)))) 
242/250

set.seed(7)
sex_amo2 <- replicate(2500, 
                     length(which(sample(rep(c("mulheres", "homens"), 
                                             each = 500), 500) == "mulheres")))


png("sex_amo2.png",  width = 17, height = 15, units = "cm", res = 300)
par(mar = c(4, 1, 2, 1))
stripchart(sex_amo2/5, 
           method = "stack",
           offset = 0.6,
           at = 0,
           frame.plot = F,
           xpd = T,
           xlim = c(floor(min(sex_amo2/5)) - 1, ceiling(max(sex_amo2/5)) + 1),
           xlab = "Número de mulheres na amostra (%)",
           main = "250 amostras de 500 pessoas",
           pch = 21,
           lwd = 1.5,
           cex = 1.5) 
abline(v = c(mean(sex_amo2/5) - sd(sex_amo2/5), 
             mean(sex_amo2/5) + sd(sex_amo2/5)), col = "lightblue", lwd = 2)
abline(v = c(mean(sex_amo2/5) - 2 * sd(sex_amo2/5), 
             mean(sex_amo2/5) + 2 * sd(sex_amo2/5)), col = "red", lwd = 2)
dev.off()

summary(sex_amo2/5)
length(which(sex_amo2/5 > (mean(sex_amo2/5) - sd(sex_amo2/5)) & 
               sex_amo2/5 < (mean(sex_amo2/5) + sd(sex_amo2/5)))) 
170/250
length(which(sex_amo2/5 > (mean(sex_amo2/5) - 2 * sd(sex_amo2/5)) & 
               sex_amo2/5 < (mean(sex_amo2/5) + 2 * sd(sex_amo2/5)))) 
240/250

png("alturas_mil_vies.png", width = 20, height = 15, units = "cm", res = 300)
par(mar = c(4, 1, 1, 1))
stripchart(alturas_h, 
           method = "stack",
           offset = 0.6,
           at = 0,
           frame.plot = F,
           xpd = T,
           xlim = c(min(alturas), max(alturas)),
           xlab = "Alturas (m)",
           pch = -0x2642L,
           lwd = 1.5,
           cex = .7) 
stripchart(alturas_m, 
           method = "stack", 
           offset = 0.6,
           at = 0,
           pch = -0x2640L,
           frame.plot = F,
           xpd = T,
           add = T,
           lwd = 1.5,
           cex = .7) 
abline(v = mean(alturas), col = "darkblue", lwd = 2)
abline(v = mean(replicate(250, 
                          mean(c(sample(alturas_h, 80), 
                                 sample(alturas_m, 20))))),
       col = "darkred", lwd = 2, lty = 2)
dev.off()

#amostra enviesada com 80 homens
mean(replicate(250, mean(c(sample(alturas_h, 80), sample(alturas_m, 20)))))
