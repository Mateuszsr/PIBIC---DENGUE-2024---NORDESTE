

data3 = read_excel(file.choose())


data3$Desmat = as.numeric(data3$Desmat)
data3$Codigo6 = as.numeric(data3$Codigo6)
data$Codigo6 = as.numeric(data$Codigo6)
data = left_join(data, data3, by = "Codigo6")

length(data$Estimados_Bayes)
length(unique(data$Desmat))
length(unique(data$Agua))

############################################
############################################
############################################
############################################
############################################
tiff("Figura - Bivariado.tiff", width = 3.5, height = 4.5, 
     res = 300, units = "in")
par(mai=c(0,0,0,0), mfrow = c(1,2))


result = cascadeKM(data$Desmat, inf.gr = 2,
                   sup.gr = 8, iter = 1000,
                   criterion = "ssi")
result$results

grupos = classIntervals(data$Desmat, n = 7,
                        style = "kmeans")

data$Grupos.Desmat = findInterval(data$Desmat, grupos$brks,
                                all.inside = TRUE)

require(RColorBrewer)
cores <- rev(brewer.pal(n = 7, "Greens"))

plot(mapaY, col = cores[data$Grupos.Desmat], border=NA)
plot(mapaX, add = TRUE)

legenda = leglabs(round(grupos$brks, 1), 
                  under = "<", over = ">")
legend("bottomright", legend = legenda, fill = cores,
       bty = "n", y.intersp = 0.9, cex = 0.6)
#text(x = -38.3, y = -9.7, "A)", cex=0.9, font = 2)


##############################################

matriz.dist = nb2listw(poly2nb(mapaY))

moran_bv(x = data$Estimados_Bayes, y = data$Desmat,
         listw = matriz.dist, nsim = 1000)
#0.06087959 

mapaYY = st_as_sf(mapaY)
matriz.dist = queen_weights(mapaYY)

resultado = local_bimoran(matriz.dist,
                          data.frame(data$Estimados_Bayes, data$Desmat))
data$LISA_BI_Desmat = lisa_clusters(resultado)

data$LISA_BI_Desmat = ifelse(data$LISA_BI_Desmat == 0, NA,
                           data$LISA_BI_Desmat)

cores = c("red", "blue", "green","yellow")
plot(mapaY, col = cores[data$LISA_BI_Desmat], border = NA)
plot(mapaX, add = TRUE)

legenda = c("Alto-Alto", "Baixo-Baixo","Baixo-Alto", "Alto-Baixo")
legend("bottomright", legend = legenda, fill = cores,
       bty = "n", y.intersp = 0.9, cex = 0.6)
#text(x = -38.3, y = -9.7, "B)", cex=0.9, font = 2)
dev.off()





data$GrupoDesmat <- case_when(
  data$Desmat < 31.3 ~ "Baixo",
  data$Desmat >= 31.3 & data$Desmat <= 58.5 ~ "Médio",
  data$Desmat > 58.5 ~ "Alto"
)

cores3 <- c("Baixo" = "#a6dba0", "Médio" = "#fddbc7", "Alto" = "#b2182b")
# Converter o fator para número para indexar as cores
data$GrupoDesmatNum <- as.numeric(factor(data$GrupoDesmat, 
                                         levels = c("Baixo", "Médio", "Alto")))

plot(mapaY, col = cores3[data$GrupoDesmat], border = NA)
plot(mapaX, add = TRUE)

legend("bottomright", 
       legend = c("Baixo", "Médio", "Alto"),
       fill = cores3,
       bty = "n", y.intersp = 0.9, cex = 0.8)


#filtrar só municípios com Alto desmatamento
alto <- data[data$GrupoDesmat == "Alto", ]

mapa_alto <- mapaY[data$GrupoDesmat == "Alto", ]
viz_alto <- poly2nb(mapa_alto)
peso_alto <- nb2listw(viz_alto, style = "W", zero.policy = TRUE)

#testando o moran bivariado entre o Desmatamento e Dengue
moran_bi_alto <- moran.test(alto$Desmat, peso_alto,
                            alt = "greater", zero.policy = TRUE)

print(moran_bi_alto)
moran_bivariado <- lm.morantest(lm(Incidencia ~ Desmat, data = alto), peso_alto)
print(moran_bivariado)



#baixo desmatamento
baixo <- data[data$GrupoDesmat == "Baixo", ]
mapa_baixo <- mapaY[data$GrupoDesmat == "Baixo", ]
viz_baixo <- poly2nb(mapa_baixo)
peso_baixo <- nb2listw(viz_baixo, style = "W", zero.policy = TRUE)

moran_bi_baixo <- moran.test(baixo$Desmat, peso_baixo,
                             alt = "greater", zero.policy = TRUE)
print(moran_bi_baixo)

moran_bivariado2 <- lm.morantest(lm(Incidencia ~ Desmat, data = baixo), peso_baixo)
print(moran_bivariado2)

################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################

data4 = read_excel(file.choose())

data4$SanBasic = as.numeric(data4$SanBasic)
data4$SanBasic = ifelse(is.na(data4$SanBasic), 0, data4$SanBasic)


data = left_join(data, data4, by = "code_muni")
data$SanBasic_Porc = (data$SanBasic / data$Pop.y) * 100
################################################################
################################################################

par(mai=c(0,0,0,0), mfrow = c(1,2))


result = cascadeKM(data$SanBasic_Porc, inf.gr = 2,
                   sup.gr = 8, iter = 1000,
                   criterion = "ssi")
result$results

grupos = classIntervals(data$SanBasic_Porc, n = 7,
                        style = "kmeans")

data$Grupos.SanBasic_Porc = findInterval(data$SanBasic_Porc, grupos$brks,
                                  all.inside = TRUE)

require(RColorBrewer)
cores = brewer.pal(n = 7, "YlOrBr")

plot(mapaY, col = cores[data$Grupos.SanBasic_Porc], border=NA)
plot(mapaX, add = TRUE)

legenda = leglabs(round(grupos$brks, 1), 
                  under = "<", over = ">")
legend("bottomright", legend = legenda, fill = cores,
       bty = "n", y.intersp = 0.9, cex = 0.6)



################################################################
################################################################



matriz.dist = nb2listw(poly2nb(mapaY))

moran_bv(x = data$Estimados_Bayes, y = data$SanBasic_Porc,
         listw = matriz.dist, nsim = 1000)
#0.06087959 

mapaYY = st_as_sf(mapaY)
matriz.dist = queen_weights(mapaYY)

resultado = local_bimoran(matriz.dist,
                          data.frame(data$Estimados_Bayes, data$SanBasic_Porc))
data$LISA_BI_Saneamento = lisa_clusters(resultado)

data$LISA_BI_Saneamento = ifelse(data$LISA_BI_Saneamento == 0, NA,
                           data$LISA_BI_Saneamento)

cores = c("red", "blue", "green","yellow")
plot(mapaY, col = cores[data$LISA_BI_Saneamento], border = NA)
plot(mapaX, add = TRUE)

legenda = c("Alto-Alto", "Baixo-Baixo","Baixo-Alto", "Alto-Baixo")
legend("bottomright", legend = legenda, fill = cores,
       bty = "n", y.intersp = 0.9, cex = 0.6)

#################################################################
#################################################################
#################################################################
library(stringr)
data5 = read_excel(file.choose())


data5$PPTotal = as.numeric(data5$PPTotal)
data5$Codigo6 = as.numeric(data5$Codigo6)
data5$PPTotal = ifelse(is.na(data5$PPTotal), 0, data5$PPTotal)


data = left_join(data, data5, by = "Codigo6")
data$PPTotal_Porc = (data$PPTotal.y.y / data$Pop) * 100
data$PPTotal_Porc = ifelse(is.na(data$PPTotal_Porc), 0, data$PPTotal_Porc)


matriz.dist = nb2listw(poly2nb(mapaY))

moran_bv(x = data$Estimados_Bayes, y = data$PPTotal_Porc,
         listw = matriz.dist, nsim = 1000)




par(mai=c(0,0,0,0), mfrow = c(1,2))


result = cascadeKM(data$PPTotal_Porc, inf.gr = 2,
                   sup.gr = 8, iter = 1000,
                   criterion = "ssi")
result$results

grupos = classIntervals(data$PPTotal_Porc, n = 7,
                        style = "kmeans")

data$Grupos.PPTotal_Porc = findInterval(data$PPTotal_Porc, grupos$brks,
                                         all.inside = TRUE)

require(RColorBrewer)
cores = brewer.pal(n = 7, "YlOrBr")

plot(mapaY, col = cores[data$Grupos.PPTotal_Porc], border=NA)
plot(mapaX, add = TRUE)

legenda = leglabs(round(grupos$brks, 1), 
                  under = "<", over = ">")
legend("bottomright", legend = legenda, fill = cores,
       bty = "n", y.intersp = 0.9, cex = 0.6)



################################################################
################################################################


mapaYY = st_as_sf(mapaY)
matriz.dist = queen_weights(mapaYY)

resultado = local_bimoran(matriz.dist,
                          data.frame(data$Estimados_Bayes, data$PPTotal_Porc))
data$LISA_BI_Coletalixo = lisa_clusters(resultado)

data$LISA_BI_Coletalixo = ifelse(data$LISA_BI_Coletalixo == 0, NA,
                                 data$LISA_BI_Coletalixo)

cores = c("red", "blue", "green","yellow")
plot(mapaY, col = cores[data$LISA_BI_Coletalixo], border = NA)
plot(mapaX, add = TRUE)

legenda = c("Alto-Alto", "Baixo-Baixo","Baixo-Alto", "Alto-Baixo")
legend("bottomright", legend = legenda, fill = cores,
       bty = "n", y.intersp = 0.9, cex = 0.6)








data5$Codigo6 = as.numeric(data5$Codigo6)
data = left_join(data, data5, by = "Codigo6")

data$TTCad_Porc = (data$TTCad / data$Pop.x) * 100
matriz.dist = nb2listw(poly2nb(mapaY))

moran_bv(x = data$Estimados_Bayes, y = data$TTCad_Porc,
         listw = matriz.dist, nsim = 1000)

par(mai=c(0,0,0,0), mfrow = c(1,2))


result = cascadeKM(data$TTCad_Porc, inf.gr = 2,
                   sup.gr = 8, iter = 1000,
                   criterion = "ssi")
result$results

grupos = classIntervals(data$TTCad_Porc, n = 7,
                        style = "kmeans")

data$Grupos.TTCad_Porc = findInterval(data$TTCad_Porc, grupos$brks,
                                  all.inside = TRUE)



require(RColorBrewer)
cores = brewer.pal(n = 7, "Blues")

plot(mapaY, col = cores[data$Grupos.TTCad_Porc], border=NA)
plot(mapaX, add = TRUE)

legenda = leglabs(round(grupos$brks, 1), 
                  under = "<", over = ">")
legend("bottomright", legend = legenda, fill = cores,
       bty = "n", y.intersp = 0.9, cex = 0.6)

resultado = local_bimoran(matriz.dist,
                          data.frame(data$Estimados_Bayes, data$TTCad_Porc))

data$LISA_BI_Coletalixo = lisa_clusters(resultado)

data$LISA_BI_Coletalixo = ifelse(data$LISA_BI_Coletalixo == 0, NA,
                                 data$LISA_BI_Coletalixo)
summary(data$Estimados_Bayes)
summary(data$TTCad_Porc)

cores = c("red", "blue", "green","yellow")
plot(mapaY, col = cores[data$LISA_BI_Coletalixo], border = NA)
plot(mapaX, add = TRUE)

legenda = c("Alto-Alto", "Baixo-Baixo","Baixo-Alto", "Alto-Baixo")
legend("bottomright", legend = legenda, fill = cores,
       bty = "n", y.intersp = 0.9, cex = 0.6)
