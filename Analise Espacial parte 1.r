################################################################
################################################################
################################################################
################################################################
library(raster)
require(geobr)
require(sf)
require(sp)

##################
mapaX = read_state(code_state = "all", year = 2020)
mapaX = as_Spatial(mapaX)
mapaX = mapaX[mapaX$code_region == 2,]
plot(mapaX)

################################################################
################################################################
################################################################
################################################################

mapaY = read_municipality(code_muni = "all", year = 2022)
mapaY = as_Spatial(mapaY)
mapaY = mapaY[mapaY$code_region == 2,]
mapaY = mapaY[mapaY$name_muni != "Fernando de Noronha",]
plot(mapaY)



################################################################
################################################################
################################################################
################################################################
#setwd("C:\\Users\\rodri\\Dropbox\\Trabalho\\PIBIC\\2024 - PIBIC (2025)")

require(readxl)
require(stringr)
require(dbplyr)

data = as.data.frame(mapaY)
data$Codigo6 = str_sub(data$code_muni, end = 6)

#dados_pibic = read_excel("Dados_Pibic_24.xlsx", col_names = TRUE)
dados <- read_excel(file.choose())
dados <- dados[dados$ID_MUNICIP != 260545, ]
dados <- dados[dados$ID_MN_RESI != 420910 & 
                 dados$ID_MN_RESI != 330455 & 
                 dados$ID_MN_RESI != 420910 & 
                 dados$ID_MN_RESI != 355030 & 
                 dados$ID_MN_RESI != 411990, ]
library(tidyverse)
dados_pibic <- dados %>%
  filter(CLASSI_FIN == 10)

#Filtrar pro projeto
library(dplyr)
teste = data.frame(table(dados_pibic$ID_MN_RESI))
colnames(teste) = c("Codigo6","Casos")
teste$Codigo6 = as.character(teste$Codigo6)
data = left_join(data, teste, by = "Codigo6")
data$Casos = ifelse(is.na(data$Casos), 0, data$Casos)

#Ajustar População
#dados_pop = read_excel("População Residente.xlsx", col_names = TRUE)
dados_pop <- read_excel(file.choose())
# Criando a nova coluna que combina UF com o código do município de 5 dígitos
dados_pop <- dados_pop %>%
  mutate(UF_COD_MUNICIPIO = paste0(UF, `COD. MUNIC`))

colnames(dados_pop) = c("ESTADO","UF","COD_M","MUNICIPIO","Pop","code_muni")
dados_pop$code_muni = as.numeric(dados_pop$code_muni)
data = left_join(data, dados_pop, by = "code_muni")

##########
data$Incidencia = data$Casos * 100000 / data$Pop

require(vegan)
require(classInt)

result = cascadeKM(data$Incidencia, inf.gr = 4, sup.gr = 8, iter = 1000, criterion = "ssi")
result$results


grupos = classIntervals(data$Incidencia, n = 8,  style = "kmeans")
grupos

data$Grupos = findInterval(data$Incidencia, grupos$brks, all.inside = TRUE)


#################################################
require(RColorBrewer)
library(sp)

display.brewer.all()
cores = brewer.pal(n = 8, "Reds")

tiff("FIGURA INCIDENCIA.tiff", width = 4, height = 4, res = 300, units = "in")

par(mai=c(0,0,0,0), mfrow = c(1, 3))
plot(mapaY,col=cores[data$Grupos], border=NA)
plot(mapaX, add = TRUE)
gruposX=leglabs(round(grupos$brks, digits=0), under = "<", over = ">")
legend("bottomright",legend=gruposX, fill=cores, bty="n",cex=0.7, y.intersp = 0.8, border = cores)

dev.off()

####################################

require(spdep)

matriz.dist = nb2listw(poly2nb(mapaY))
moran.test(data$Incidencia, matriz.dist)

#######Função para criar Legenda
leglabs <- function(vec, under = "under", over = "over", between = "-", 
                    reverse = FALSE) {
  x <- vec
  lx <- length(x)
  if (lx < 3) 
    stop("vector too short")
  if (reverse) {
    x <- rev(x)
    under <- "over"
    over <- "under"
  }
  res <- character(lx - 1)
  res[1] <- paste(under, x[2])
  for (i in 2:(lx - 2)) res[i] <- paste(x[i], between, x[i + 
                                                           1])
  res[lx - 1] <- paste(over, x[lx - 1])
  res
}




#############################################################################
#############################################################################



require(spdep)

matriz.dist = nb2listw(poly2nb(mapaY))
moran.test(data$Incidencia, matriz.dist)


###################################

estimador = EBlocal(ni = data$Pop, ri = data$Casos, nb = poly2nb(mapaY))
data$Estimados_Bayes = estimador$est*100000

result = cascadeKM(data$Estimados_Bayes, inf.gr = 4, sup.gr = 8, iter = 1000, criterion = "ssi")
result$results

grupos = classIntervals(data$Estimados_Bayes, n = 8,  style = "kmeans")
grupos

data$Grupos_Bays = findInterval(data$Estimados_Bayes, grupos$brks, all.inside = TRUE)


#################################################
require(RColorBrewer)
#display.brewer.all()
cores2 = brewer.pal(n = 8, "Reds")

#tiff("FIGURA INCIDENCIA - BAYES.tiff", width = 4, height = 4, res = 300, units = "in")
tiff("FIGURA - BAYES - LISA - CLUSTERS.tiff", width = 6, height = 3, res = 300, units = "in")


par(mai=c(0,0,0,0), mfrow = c(1, 3))
plot(mapaY,col=cores2[data$Grupos_Bays], border=NA)
plot(mapaX, add = TRUE)
gruposX=leglabs(round(grupos$brks, digits=0), under = "<", over = ">")
legend("bottomright",legend=gruposX, fill=cores2, bty="n",cex=0.7, y.intersp = 0.8, border = cores2)

#dev.off()

matriz.dist = nb2listw(poly2nb(mapaY))
moran.test(data$Estimados_Bayes, matriz.dist)

#########################################
#########################################
#########################################
#########################################
library(spdep)

require(rgeoda)
library(sf)

mapaYY = st_as_sf(mapaY)
matriz.dist = queen_weights(mapaYY)
resultado = local_moran(matriz.dist, data.frame(data$Estimados_Bayes))

resultado$p_vals
table(lisa_clusters(resultado))
data$LISA = lisa_clusters(resultado)
data$LISA = ifelse(data$LISA==0, NA, data$LISA)

cores = c("red2", "blue", "green4","gold")
cores_legenda = c("red2", "blue", "green4","gold", "white")

#tiff("FIGURA LISA.tiff", width = 4, height = 4, res = 300, units = "in")

par(mai=c(0,0,0,0))
plot(mapaY,col=cores[data$LISA], border=NA)
plot(mapaX, add = TRUE)
gruposX=c("Alto-Alto", "Baixo-Baixo", "Baixo-Alto","Alto-Baixo","N.S.")
legend("bottomright",legend=gruposX, fill=cores_legenda, bty="n",cex=0.7, 
       y.intersp = 0.8, border = c(cores,"black"))

#dev.off()







#########################################################################
#########################################################################
require(rflexscan)

require(spdep)

coords <- coordinates(mapaY)
####
# load sample data (North Carolina SIDS data)
# calculate the expected numbers of cases
expected <- data$Pop * sum(data$Casos) / sum(data$Pop)
matriz.dist = poly2nb(mapaY)


# run FleXScan
fls <- rflexscan(lon = coords[,1], lat  = coords[,2],
                 observed = data$Casos,
                 expected = expected,
                 name = data$name_muni,
                 clustersize = 10,
                 nb = matriz.dist)
# print rflexscan object
print(fls)
fls$cluster[[1]]$name

i=1
data$Risco = NA

while(fls$cluster[[i]]$pval < 0.05) {
  
  data$Risco[fls$cluster[[i]]$area] = fls$cluster[[i]]$RR 
  
  i = i + 1  
}


# Extraindo valores de RR
rr_values <- sapply(fls$cluster, function(x) x$RR)
# Calcula os quartis dos valores de RR
breaks <- quantile(rr_values, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
cor_ns <- "white"
cores1 = brewer.pal(n = 4, "Reds")
# Criação dos rótulos da legenda com duas casas decimais e "N.S." para áreas em branco
legenda_rr <- c(
  paste("<", sprintf("%.2f", breaks[2])),                   # "< 1.5"
  paste(sprintf("%.2f", breaks[2]), "-", sprintf("%.2f", breaks[3])),  # "1.5 - 2.0"
  paste(sprintf("%.2f", breaks[3]), "-", sprintf("%.2f", breaks[4])),  # "2.0 - 3.5"
  paste(">", sprintf("%.2f", breaks[4])),                   # "> 3.5"
  "N.S."  # Rótulo para áreas sem significância
)

# Inicialize um vetor para armazenar a cor das regiões
cores_regioes <- rep("white", length(data$name_muni))  # Inicialmente todas as regiões serão cinzas  
  
# Percorrer cada cluster e colorir conforme o RR
for (i in seq_along(fls$cluster)) {
    cluster_municipios <- fls$cluster[[i]]$name       # Nomes dos municípios do cluster
    cluster_rr <- fls$cluster[[i]]$RR                 # Risco relativo do cluster
    cluster_indices <- which(data$name_muni %in% cluster_municipios)  # Índices dos municípios no dataset
    
    # Determine a cor baseada no intervalo de RR
    cor_cluster <- cores1[findInterval(cluster_rr, breaks)]
    
    # Aplique a cor do cluster aos municípios do cluster
    cores_regioes[cluster_indices] <- cor_cluster
}


#Plotar o mapa com as cores de RR
par(mai=c(0,0,0,0))
plot(mapaY, col=cores_regioes, border=NA)  # Mapa com os clusters coloridos por RR
plot(mapaX, add = TRUE)  # Mapa de fundo
  
# Criar a legenda para os níveis de RR
legend("bottomright", legend = legenda_rr, 
        fill = c(cores1, cor_ns), bty = "n", cex = 0.7, y.intersp = 0.8)

dev.off()
