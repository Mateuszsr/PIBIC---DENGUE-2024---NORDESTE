

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
