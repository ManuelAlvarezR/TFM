library(tidyverse) # metapackage with lots of helpful functions
library(jsonlite) # for reading in json

geo_mean<- function(x) {
  n <- length(x)
  media_geom <- prod(x)^(1/n)
  return(media_geom)
}

## LENDA 
# "LinearTime": tiempo total de la interacción de RAPOSa con el solver lineal =~
#     "LinearGenerationTime": Tiempo total de la generación de los problemas lineales
#     +"LinearSolvingTime": Tiempo total de la resolución de los problemas lineales (contado por nosotros)
#     +"LinearPostProcessingTime": Tiempo total de postprocesado de la solución de los problemas lineales
# "LinearSolvingTimeBySolver": Tiempo total de la resolución de los problemas lineales (el tiempo que devuelve el solver)



#capture.output(str(read_json("d4n12R0R7d001d05.json")))

####---- CONFIGURACION 0.75 Simplex 0.25 RAPOSa ----#####
s75r25 <-"C:/Users/Usuario/OneDrive - Universidade de Santiago de Compostela/Escritorio/RAPOsa/Tempos/d9a77a62-698d-41e4-8a44-c2d6570c6e68/0.75 simplex 0.25 raposa"
arquivos_json <- list.files(s75r25, pattern = "\\.json$", full.names = TRUE)

lista_json <- map(arquivos_json, ~{
  conteudo <- fromJSON(.x)
  if (length(conteudo) > 0) {
    return(conteudo)
  } else {
    return(NULL)
  }
})

# Filtra os elementos nulos da lista
ind <- !sapply(lista_json, is.null)
nome_problemas <- tools::file_path_sans_ext(basename(arquivos_json))[!sapply(lista_json, is.null)]
ind_prob<-nome_problemas
lista_json <- lista_json[ind]

# Crea o data frame
dt_75_25 <- as.data.frame(do.call(rbind, lista_json),row.names = nome_problemas)
dt_75_25[] <- lapply(dt_75_25,unlist)
# Escollemos aqueles que non se resolveron en 600" e teñen mais de 50 iter de raposa
dim(dt_75_25)

# Comprobamos que LT = LGenerationT + LSolvingT + LPostProcessingT)
hist((dt_75_25$LinearTime-(dt_75_25$LinearGenerationTime+dt_75_25$LinearSolvingTime+dt_75_25$LinearPostProcessingTime))) #en segundos
hist((dt_75_25$LinearTime-(dt_75_25$LinearGenerationTime+dt_75_25$LinearSolvingTime+dt_75_25$LinearPostProcessingTime))/dt_75_25$LinearTime*100,breaks = 15) #en procentaxe respecto o LP

# Comprobamos que LSolvingT(calculado por nos) = LSolvingTBySolver (que nos devolve Gurobi)
hist((dt_75_25$LinearSolvingTime-dt_75_25$LinearSolvingTimeBySolver)/dt_75_25$LinearSolvingTime*100)

####---- CONFIGURACION 0.75 Simplex 0.5 RAPOSa ----#####
s75r5 <-"C:/Users/Usuario/OneDrive - Universidade de Santiago de Compostela/Escritorio/RAPOsa/Tempos/d9a77a62-698d-41e4-8a44-c2d6570c6e68/0.75 simplex 0.5 raposa"
arquivos_json <- list.files(s75r5, pattern = "\\.json$", full.names = TRUE)

lista_json <- map(arquivos_json, ~{
  conteudo <- fromJSON(.x)
  if (length(conteudo) > 0) {
    return(conteudo)
  } else {
    return(NULL)
  }
})
ind <- !sapply(lista_json, is.null)
nome_problemas <- tools::file_path_sans_ext(basename(arquivos_json))[!sapply(lista_json, is.null)]
ind_prob<-nome_problemas
lista_json <- lista_json[ind]
dt_75_5 <- as.data.frame(do.call(rbind, lista_json),row.names = nome_problemas)
dt_75_5[] <- lapply(dt_75_5,unlist)
dim(dt_75_5)

####---- CONFIGURACION 0.75 Simplex 0.75 RAPOSa ----#####
s75r75 <-"C:/Users/Usuario/OneDrive - Universidade de Santiago de Compostela/Escritorio/RAPOsa/Tempos/d9a77a62-698d-41e4-8a44-c2d6570c6e68/0.75 simplex 0.75 raposa"
arquivos_json <- list.files(s75r75, pattern = "\\.json$", full.names = TRUE)

lista_json <- map(arquivos_json, ~{
  conteudo <- fromJSON(.x)
  if (length(conteudo) > 0) {
    return(conteudo)
  } else {
    return(NULL)
  }
})
ind <- !sapply(lista_json, is.null)
nome_problemas <- tools::file_path_sans_ext(basename(arquivos_json))[!sapply(lista_json, is.null)]
ind_prob<-nome_problemas
lista_json <- lista_json[ind]
dt_75_75 <- as.data.frame(do.call(rbind, lista_json),row.names = nome_problemas)
dt_75_75[] <- lapply(dt_75_75,unlist)
dim(dt_75_75)

####---- CONFIGURACION 0.25 Simplex 0.25 RAPOSa ----#####
s25r25 <-"C:/Users/Usuario/OneDrive - Universidade de Santiago de Compostela/Escritorio/RAPOsa/Tempos/d9a77a62-698d-41e4-8a44-c2d6570c6e68/0.25 simplex 0.25 raposa"
arquivos_json <- list.files(s25r25, pattern = "\\.json$", full.names = TRUE)

lista_json <- map(arquivos_json, ~{
  conteudo <- fromJSON(.x)
  if (length(conteudo) > 0) {
    return(conteudo)
  } else {
    return(NULL)
  }
})
ind <- !sapply(lista_json, is.null)
nome_problemas <- tools::file_path_sans_ext(basename(arquivos_json))[!sapply(lista_json, is.null)]
ind_prob<-nome_problemas
lista_json <- lista_json[ind]
dt_25_25 <- as.data.frame(do.call(rbind, lista_json),row.names = nome_problemas)
dt_25_25[] <- lapply(dt_25_25,unlist)
dim(dt_25_25)
####---- CONFIGURACION 0.25 Simplex 0.5 RAPOSa ----#####
s25r5 <-"C:/Users/Usuario/OneDrive - Universidade de Santiago de Compostela/Escritorio/RAPOsa/Tempos/d9a77a62-698d-41e4-8a44-c2d6570c6e68/0.25 simplex 0.5 raposa"
arquivos_json <- list.files(s25r5, pattern = "\\.json$", full.names = TRUE)

lista_json <- map(arquivos_json, ~{
  conteudo <- fromJSON(.x)
  if (length(conteudo) > 0) {
    return(conteudo)
  } else {
    return(NULL)
  }
})
ind <- !sapply(lista_json, is.null)
nome_problemas <- tools::file_path_sans_ext(basename(arquivos_json))[!sapply(lista_json, is.null)]
ind_prob<-nome_problemas
lista_json <- lista_json[ind]
dt_25_5 <- as.data.frame(do.call(rbind, lista_json),row.names = nome_problemas)
dt_25_5[] <- lapply(dt_25_5,unlist)
dim(dt_25_5)
####---- CONFIGURACION 0.25 Simplex 0.75 RAPOSa ----#####
s25r75 <-"C:/Users/Usuario/OneDrive - Universidade de Santiago de Compostela/Escritorio/RAPOsa/Tempos/d9a77a62-698d-41e4-8a44-c2d6570c6e68/0.25 simplex 0.75 raposa"
arquivos_json <- list.files(s25r75, pattern = "\\.json$", full.names = TRUE)

lista_json <- map(arquivos_json, ~{
  conteudo <- fromJSON(.x)
  if (length(conteudo) > 0) {
    return(conteudo)
  } else {
    return(NULL)
  }
})
ind <- !sapply(lista_json, is.null)
nome_problemas <- tools::file_path_sans_ext(basename(arquivos_json))[!sapply(lista_json, is.null)]
ind_prob<-nome_problemas
lista_json <- lista_json[ind]
dt_25_75 <- as.data.frame(do.call(rbind, lista_json),row.names = nome_problemas)
dt_25_75[] <- lapply(dt_25_75,unlist)
dim(dt_25_75)

####---- CONFIGURACION ESTANDAR ----##### 
#(Executar despois de 0.75 0.25)

# CONFIGURACIONS

stand_evrim <-"C:/Users/Usuario/OneDrive - Universidade de Santiago de Compostela/Escritorio/RAPOsa/Tempos/d9a77a62-698d-41e4-8a44-c2d6570c6e68/Standard__evrim"
stand_minlp <-"C:/Users/Usuario/OneDrive - Universidade de Santiago de Compostela/Escritorio/RAPOsa/Tempos/d9a77a62-698d-41e4-8a44-c2d6570c6e68/Standard__minlplib"
stand <-"C:/Users/Usuario/OneDrive - Universidade de Santiago de Compostela/Escritorio/RAPOsa/Tempos/d9a77a62-698d-41e4-8a44-c2d6570c6e68/Standard"

# Ler todolos arquivos JSON da configuracion correspondente
arquivos_json <- list.files(stand, pattern = "\\.json$", full.names = TRUE)
lista_json <- map(arquivos_json, fromJSON)
nome_problemas=tools::file_path_sans_ext(basename(arquivos_json))

# Combine todos os elementos da lista nun único data frame
datos_combinados <- as.data.frame(do.call(rbind, lista_json),row.names=nome_problemas) #bind_rows(lista_json)
datos_combinados[] <- lapply(datos_combinados,unlist)
# Escollemos aqueles que non se resolveron en 600" e teñen mais de 50 iter de raposa
dt <-datos_combinados[ind_prob,]#subset(datos_combinados, datos_combinados$TotalTime > 599 & datos_combinados$Iterations > 50)
dim(datos_combinados)
dim(dt)

# Comprobamos que LT = LGenerationT + LSolvingT + LPostProcessingT)
datos0=(abs(dt$LinearTime-(dt$LinearGenerationTime+dt$LinearSolvingTime+dt$LinearPostProcessingTime))/dt$LinearTime*100)
hist((dt$LinearTime-(dt$LinearGenerationTime+dt$LinearSolvingTime+dt$LinearPostProcessingTime))) #en segundos
hist(datos0, freq = TRUE,
     col = "#75AADB",  # Color de las barras
     border = "white",  # Color del borde de las barras
     main = "Erro relativo ó descompoñer o tempo linear total",  # Erro relativo ó descompoñer o tempo linear total na configuración estándar
     xlab = "Erro relativo",  # Etiqueta del eje x
     ylab = "Nº de problemas",  # Etiqueta del eje y
     xlim = c(0,15),  # Rango del eje x
     breaks = 10,  # Número de barras
     las = 1,  # Orientación de los números en los ejes (horizontal)
     cex.axis = 0.8,  # Tamaño de las etiquetas en los ejes
     cex.lab = 0.9,  # Tamaño de las etiquetas de los ejes x e y
     cex.main = 1.2,  # Tamaño del título
     col.axis = "black",  # Color de las etiquetas de los ejes
     col.lab = "black"  # Color de las etiquetas de los ejes x e y
) #en procentaxe respecto o LP
grid(col = "lightgray", lty = "dotted")
quantile(datos0,0.95)# Solamente no 5% dos problemas a diferencia relativa é maior do 5.6% repecto o tempo LP (é maior o noso)

# Comprobamos que LSolvingT(calculado por nos) = LSolvingTBySolver (que nos devolve Gurobi)
hist(abs((dt$LinearSolvingTime-dt$LinearSolvingTimeBySolver)/dt$LinearSolvingTimeBySolver*100), freq = TRUE,
     col = "#75AADB",  # Color de las barras
     border = "white",  # Color del borde de las barras
     main = "Error relativo entre os tempos de resolución",  # Título del gráfico
     xlab = "Error relativo",  # Etiqueta del eje x #respecto o tempo do solver
     ylab = "Nº de problemas",  # Etiqueta del eje y
     xlim = c(0,15),  # Rango del eje x
     breaks = 10,  # Número de barras
     las = 1,  # Orientación de los números en los ejes (horizontal)
     cex.axis = 0.8,  # Tamaño de las etiquetas en los ejes
     cex.lab = 0.9,  # Tamaño de las etiquetas de los ejes x e y
     cex.main = 1.2,  # Tamaño del título
     col.axis = "black",  # Color de las etiquetas de los ejes
     col.lab = "black"  # Color de las etiquetas de los ejes x e y
) 
grid(col = "lightgray", lty = "dotted")
quantile(abs((dt$LinearSolvingTime-dt$LinearSolvingTimeBySolver)/dt$LinearSolvingTimeBySolver*100),.95)# Solamente no 5% dos problemas a diferencia relativa é maior do 6.5% repecto o do solver, ie, o tempo calculado por  nos é como moito un 6.5 maior o do solver. (no 95% dos casos)

# Tempo LP vs Tempo Total
datos2=dt$LinearTime/dt$TotalTime*100
hist(datos2,
     freq = TRUE,
     col = "#75AADB",  # Color de las barras
     border = "white",  # Color del borde de las barras
     main = "Tempo linear fronte o tempo total",  # Título del gráfico
     xlab = "Porcentaxe do tempo total correspondente ós LPs",  # Etiqueta del eje x
     ylab = "Nº de problemas",  # Etiqueta del eje y
     xlim = c(0,100),  # Rango del eje x
     breaks = 10,  # Número de barras
     las = 1,  # Orientación de los números en los ejes (horizontal)
     cex.axis = 0.8,  # Tamaño de las etiquetas en los ejes
     cex.lab = 0.9,  # Tamaño de las etiquetas de los ejes x e y
     cex.main = 1.2,  # Tamaño del título
     col.axis = "black",  # Color de las etiquetas de los ejes
     col.lab = "black"  # Color de las etiquetas de los ejes x e y
)
grid(col = "lightgray", lty = "dotted")
quantile(datos2,0.2)# En máis do 80% dos problemas selecionados o tempo LP representa máis do 75% do tempo total

# Tempo LP Solver (por nos) vs Tempo 
# Proporción do tempo de resolución no solver (o que debería ser simplex) frente o tempo linear e o total.
datos1=dt$LinearSolvingTime/dt$TotalTime*100
hist(datos1,
     freq = TRUE,
     col = "#75AADB",  # Color de las barras
     border = "white",  # Color del borde de las barras
     main = "Tempo linear de resolución fronte o tempo total",  # Título del gráfico
     xlab = "Porcentaxe do tempo total adicado a resolución dos LPs",  # Etiqueta del eje x
     ylab = "Nº de problemas",  # Etiqueta del eje y
     xlim = c(0,100),  # Rango del eje x
     breaks = 15,  # Número de barras
     las = 1,  # Orientación de los números en los ejes (horizontal)
     cex.axis = 0.8,  # Tamaño de las etiquetas en los ejes
     cex.lab = 0.9,  # Tamaño de las etiquetas de los ejes x e y
     cex.main = 1.2,  # Tamaño del título
     col.axis = "black",  # Color de las etiquetas de los ejes
     col.lab = "black"  # Color de las etiquetas de los ejes x e y
)
grid(col = "lightgray", lty = "dotted")
quantile(datos1,0.16)# No 85% dos problemas selecionados o tempo de resolución é mais da metade do tempo total


# Tempo LP Solve vs Tempo LP
datos3=dt$LinearSolvingTime/dt$LinearTime*100
hist(datos3,
     freq = TRUE,
     col = "#75AADB",  # Color de las barras
     border = "white",  # Color del borde de las barras
     main = "Versión Estándar",  # Título del gráfico
     xlab = "Porcentaxe do tempo linear correspondente a resolver LP",  # Etiqueta del eje x
     ylab = "Nº de problemas",  # Etiqueta del eje y
     xlim = c(0,100),  # Rango del eje x
     breaks = 10,  # Número de barras
     las = 1,  # Orientación de los números en los ejes (horizontal)
     cex.axis = 0.8,  # Tamaño de las etiquetas en los ejes
     cex.lab = 0.9,  # Tamaño de las etiquetas de los ejes x e y
     cex.main = 1.2,  # Tamaño del título
     col.axis = "black",  # Color de las etiquetas de los ejes
     col.lab = "black"  # Color de las etiquetas de los ejes x e y
)
grid(col = "lightgray", lty = "dotted")
quantile(datos3,0.5)# No metade dos problemas selecionados o tempo de resolucion dos LP é mais do 80% do tempo LP. Non obstante observamos o histograma e vemos que en xeral o tempo de resolución é a meirante parte do tempo LP


# Tempo LP Solve (por Gurobi) vs Tempo LP
datos4=dt$LinearSolvingTimeBySolver/dt$LinearTime*100
hist(datos4,
     freq = TRUE,
     col = "#75AADB",  # Color de las barras
     border = "white",  # Color del borde de las barras
     main = "Versión Estándar",  # Título del gráfico
     xlab = "Porcentaxe do tempo linear correspondente a resolver LP segundo Gurobi",  # Etiqueta del eje x
     ylab = "Nº de problemas",  # Etiqueta del eje y
     xlim = c(0,100),  # Rango del eje x
     breaks = 10,  # Número de barras
     las = 1,  # Orientación de los números en los ejes (horizontal)
     cex.axis = 0.8,  # Tamaño de las etiquetas en los ejes
     cex.lab = 0.9,  # Tamaño de las etiquetas de los ejes x e y
     cex.main = 1.2,  # Tamaño del título
     col.axis = "black",  # Color de las etiquetas de los ejes
     col.lab = "black"  # Color de las etiquetas de los ejes x e y
)
grid(col = "lightgray", lty = "dotted")
quantile(datos4,0.5)# No metade dos problemas selecionados o tempo de resolucion dos LP é mais do 80% do tempo LP. Non obstante observamos o histograma e vemos que en xeral o tempo de resolución é a meirante parte do tempo LP

mean(datos4)
geo_mean(datos4)

# Ver como evoluciona a tasa entre LP e LPsolve a medida que TLP aumenta
ind_ord <- order(dt$LinearTime)
ind_ord_p<-row.names(dt[ind_ord,])
plot(dt[ind_ord_p,]$LinearTime,dt[ind_ord_p,]$LinearSolvingTimeBySolver/dt[ind_ord_p,]$LinearTime,
     main = "Porcentaxe do Tempo LP correspondente a resolver LP (polo Solver)", #type="lines"
     xlab = "Índice dos problemas en orde crecente segundo tempo LP", ylab = "Porcentaxe", pch=16)
# vemos que cando LP é pequeno, a tasa é menor, ie, Cando os tempos LP son pequenos,
# hai tempo de procesamenteo e eso que non diminues cando si o fan os de resolucion 
# (porque o problema ten menos var, menos denso, menor grao...)
# Non obstante, como estamos escollendo problemas que non se soluiconan e 600", 
# os tempos LP son moi grandes, polo que este efecto non debería afectar as conclusion 
# que se obteñan para este conxunto


####---- COMPARACIONS 2a2 ----#####
# MIRAR en detalle os PROBLEMAS MOI DENSOS (podo coller os que teñen mas de 1 segundo por nodo), para relacionalo ca grafcica que fixera pos FO
####---- COMPARACIONS MULTIPLES ----#####
####---- 0.75 SIMPLEX ----#####

# Densidade (KDE) dos tempos LP
plot(density(dt$LinearTime), col = "blue", main = "Densidad do Tempo LP", xlim = c(min(dt$LinearTime, dt_75_25$LinearTime), max(dt$LinearTime, dt_75_25$LinearTime)))
lines(density(dt_75_25$LinearTime), col = "purple")
lines(density(dt_75_5$LinearTime), col = "green")
lines(density(dt_75_75$LinearTime), col = "red")
legend("topleft", legend = c("dt", "dt_75_25","dt_75_5","dt_75_75"), fill = c("blue","purple" ,"green","red"))
# Densidade (KDE) dos tempos LPsolve
plot(density(dt$LinearSolvingTime), col = "blue", main = "Tempo linear de resolución", xlim = c(min(dt$LinearTime, dt_75_25$LinearTime), max(dt$LinearTime, dt_75_25$LinearTime)))
lines(density(dt_75_25$LinearSolvingTime), col = "purple", lt=2)
lines(density(dt_75_5$LinearSolvingTime), col = "green",lt=3)
lines(density(dt_75_75$LinearSolvingTime), col = "red",lt=4)
legend("bottomright", legend = c("Estándar", "75% símplex dual 25% RAPOSa","75% símplex dual 50% RAPOSa","75% símplex dual 75% RAPOSa"), fill = c("blue","purple" ,"green","red"), cex = 0.75)
## con ggplot2
dt$group <- "Estándar"
dt_75_25$group <- "75% símplex dual 25% RAPOSa"
dt_75_5$group <- "75% símplex dual 50% RAPOSa"
dt_75_75$group <- "75% símplex dual 75% RAPOSa"
all_data <- rbind(dt, dt_75_25, dt_75_5, dt_75_75)
ggplot(all_data, aes(x = LinearSolvingTime, color = group, linetype = group)) +
  geom_density() +
  scale_color_manual(values = c("purple", "green", "red", "blue")) +
  scale_linetype_manual(values = c(4, 3, 2, 1)) +
#  labs(title = "Tempo linear de resolución") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.2), # Coordenadas para colocar la leyenda en la esquina inferior derecha
        legend.title = element_blank()) +
  xlim(min(c(dt$LinearSolvingTime, dt_75_25$LinearSolvingTime, dt_75_5$LinearSolvingTime, dt_75_75$LinearSolvingTime)), 
       max(c(dt$LinearSolvingTime, dt_75_25$LinearSolvingTime, dt_75_5$LinearSolvingTime, dt_75_75$LinearSolvingTime))) 
  #guides(color = guide_legend(title = "Configuración"), linetype = guide_legend(title = "Configuración"))

#### Tempos LP (x problemas ordenados por tempos y tempos color conf)
ind_ord <- order(dt$LinearTime)
ind_ord_p<-row.names(dt[ind_ord,])
plot(dt[ind_ord_p,]$LinearTime, col = "blue", main = "Linear Time") #type="lines"
points(dt_75_25[ind_ord_p,]$LinearTime, col = "purple")
points(dt_75_5[ind_ord_p,]$LinearTime, col = "green")
points(dt_75_75[ind_ord_p,]$LinearTime, col = "red")


# Tempos LP solve (por Gurobi) (x=problemas ordenados por tempos y=tempos color=conf)
ind_ord <- order(dt$LinearSolvingTime)
ind_ord_p<-row.names(dt[ind_ord,])
plot(dt[ind_ord_p,]$LinearSolvingTime, col = "blue",
     main = "Tempo (polo Solver) de resolver LP", #type="lines"
     xlab = "Índice", ylab = "Tempo", pch=16)
points(dt_75_25[ind_ord_p,]$LinearSolvingTime, col = "purple",pch=16)
points(dt_75_5[ind_ord_p,]$LinearSolvingTime, col = "green",pch=16)
points(dt_75_75[ind_ord_p,]$LinearSolvingTime, col = "red",pch=16)
grid()
legend("bottomright",legend = c("Estándar", "75% símplex dual 25% RAPOSa","75% símplex dual 50% RAPOSa","75% símplex dual 75% RAPOSa"),
       col = c("blue","purple" ,"green","red"),
       cex = 0.75,pch = 16, title = "Configuracións")
####
ggplot() +
  geom_point(data = dt[ind_ord_p,], aes(x = seq_along(LinearSolvingTime), y = LinearSolvingTime, color = "Estándar"), pch = 16) +
  geom_point(data = dt_75_25[ind_ord_p,], aes(x = seq_along(LinearSolvingTime), y = LinearSolvingTime, color = "75% símplex dual 25% RAPOSa"), pch = 16) +
  geom_point(data = dt_75_5[ind_ord_p,], aes(x = seq_along(LinearSolvingTime), y = LinearSolvingTime, color = "75% símplex dual 50% RAPOSa"), pch = 16) +
  geom_point(data = dt_75_75[ind_ord_p,], aes(x = seq_along(LinearSolvingTime), y = LinearSolvingTime, color = "75% símplex dual 75% RAPOSa"), pch = 16) +
  labs(title = "", x = "Instancia", y = "Tempo linear de resolución") +
  scale_color_manual(name = "Configuración", values = c("Estándar" = "blue", "75% símplex dual 25% RAPOSa" = "purple", "75% símplex dual 50% RAPOSa" = "green", "75% símplex dual 75% RAPOSa" = "red")) +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.2), # Coordenadas para colocar la leyenda en la esquina inferior derecha
        legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(pch = 16)))




plot(dt[ind_ord_p,]$LinearSolvingTime, col = "blue",
     main = "Tempo (polo Solver) de resolver LP", type="lines",
     xlab = "Índice", ylab = "Tempo", pch=16)
points(dt_75_25[ind_ord_p,]$LinearSolvingTime, type="lines",col = "purple",pch=16)
points(dt_75_5[ind_ord_p,]$LinearSolvingTime,type="lines", col = "green",pch=16)
points(dt_75_75[ind_ord_p,]$LinearSolvingTime,type="lines", col = "red",pch=16)
grid()
legend("bottomright",legend = c("dt", "dt_75_25","dt_75_5","dt_75_75"),
       col = c("blue","purple" ,"green","red"),
       cex = 0.75,pch = 16, title = "Configuracións")

### Tempos medio por nodo de resolver LP (medido por Gurobi) (x=problemas ordenados por tempos y=tempos color=conf)
ind_ord <- order(dt$LinearSolvingTime/dt$VisitedNodes)
ind_ord_p<-row.names(dt[ind_ord,])
plot(dt[ind_ord_p,]$LinearSolvingTimeBySolver/dt[ind_ord_p,]$VisitedNodes, col = "blue",
     main = "Tempo medio por nodo (medido polo Solver) de resolver LP", 
     xlab = "Índice", ylab = "Tempo", pch=16)
points(dt_75_25[ind_ord_p,]$LinearSolvingTimeBySolver/dt_75_25[ind_ord_p,]$VisitedNodes, col = "purple",pch=16)
points(dt_75_5[ind_ord_p,]$LinearSolvingTimeBySolver/dt_75_5[ind_ord_p,]$VisitedNodes, col = "green",pch=16)
points(dt_75_75[ind_ord_p,]$LinearSolvingTimeBySolver/dt_75_75[ind_ord_p,]$VisitedNodes, col = "red",pch=16)
legend("topleft",legend = c("dt", "dt_75_25","dt_75_5","dt_75_75"),
       col = c("blue","purple" ,"green","red"),
       cex = 0.75,pch = 16, title = "Configuracións")
grid()
####
ggplot() +
  geom_point(data = dt[ind_ord_p,], aes(x = seq_along(LinearSolvingTimeBySolver), y = LinearSolvingTimeBySolver / VisitedNodes, color = "dt"), pch = 16) +
  geom_point(data = dt_75_25[ind_ord_p,], aes(x = seq_along(LinearSolvingTimeBySolver), y = LinearSolvingTimeBySolver / VisitedNodes, color = "dt_75_25"), pch = 16) +
  geom_point(data = dt_75_5[ind_ord_p,], aes(x = seq_along(LinearSolvingTimeBySolver), y = LinearSolvingTimeBySolver / VisitedNodes, color = "dt_75_5"), pch = 16) +
  geom_point(data = dt_75_75[ind_ord_p,], aes(x = seq_along(LinearSolvingTimeBySolver), y = LinearSolvingTimeBySolver / VisitedNodes, color = "dt_75_75"), pch = 16) +
  labs(title = "", x = "Instancia", y = "Tempo linear medio de resolución (por nodo)", color = "Configuracións") +
  scale_color_manual(values = c("dt" = "blue", "dt_75_25" = "purple", "dt_75_5" = "green", "dt_75_75" = "red"), name = "Configuracións") +
  theme_minimal() +
  theme(legend.position = c(0.1, 0.85), # Coordenadas para colocar la leyenda en la esquina inferior derecha
        legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(pch = 16)))###

ind_ord <- order(dt$LinearSolvingTimeBySolver/dt$VisitedNodes)
plot((dt_75_25[ind_ord,]$LinearSolvingTimeBySolver/dt_75_25[ind_ord,]$VisitedNodes)/(dt[ind_ord,]$LinearSolvingTimeBySolver/dt[ind_ord,]$VisitedNodes),
     col = "purple",
     main = "Tasa do tempo medio por nodo (medido por Gurobi) de resolución do LP respecto á versión estándar", # En porcentaxe
     xlab = "Índice", ylab = "Tempo conf/estand", pch=16,yaxt="n")
points((dt_75_5[ind_ord,]$LinearSolvingTimeBySolver/dt_75_5[ind_ord,]$VisitedNodes)/(dt[ind_ord,]$LinearSolvingTimeBySolver/dt[ind_ord,]$VisitedNodes),col="green",pch=16)
points((dt_75_75[ind_ord,]$LinearSolvingTimeBySolver/dt_75_75[ind_ord,]$VisitedNodes)/(dt[ind_ord,]$LinearSolvingTimeBySolver/dt[ind_ord,]$VisitedNodes),col="red",pch=16)
legend("topleft",legend = c("dt_75_25","dt_75_5","dt_75_75"),
       col = c("purple" ,"green","red"),
       cex = 0.75, pch = 16, title = "Configuracións")
abline(h=1)
axis(2, tck = 1, lty = 2, col = "gray",at = seq(0, 2, by = 0.25))

## Similares os feitos con AMPL
datos=as.data.frame(cbind(dt$LinearSolvingTimeBySolver/dt$VisitedNodes,dt_75_25$LinearSolvingTimeBySolver/dt_75_25$VisitedNodes,dt_75_5$LinearSolvingTimeBySolver/dt_75_5$VisitedNodes,dt_75_75$LinearSolvingTimeBySolver/dt_75_75$VisitedNodes))
datos=datos/datos[,1]*100
colnames(datos) <- c("Estándar", "75% símplex dual 25% RAPOSa", "75% símplex dual 50% RAPOSa", "75% símplex dual 75% RAPOSa")

## Boxplot 
library(ggplot2)
datos_long <- reshape2::melt(datos[,-1])

# Crear o boxplot con ggplot2
ggplot(datos_long, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightblue", color = "blue", alpha = 0.7) +
  labs(title = "",
       x = "Configuración",
       y = "Porcentaxe ") +
  scale_y_continuous()+ #breaks = c(25,75, 100,125 ,175)
  theme_minimal() # intercambia colores do grid co fondo

####---- RESTO ----#####
LinearTimeNode=LinearTime/VisitedNodes # Tempo Linear medio por nodo
LinearSolvingTimeNode=LinearSolvingTime/VisitedNodes # Tempo medio que leva resolver un LP

hist(LinearTimeNode,breaks = 20)
Pind_with_small_LTN=which(LinearTimeNode<quantile(LinearTimeNode, 0.25))
Pind_without_big_LTN=which(LinearTimeNode<quantile(LinearTimeNode, 0.75))
hist(TotalTime[Pind_with_small_LTN])
hist(TotalTime[-Pind_without_big_LTN])

hist(TotalTime[which(TotalTime>=600)])

LinearTime
LinearGenerationTime
LinearSolvingTime
LinearPostProcessingTime
LinearSolvingTimeBySolver

detach(datos_filtrados)


write.csv(datos,file="simplex75.csv",row.names = TRUE)
write.csv(datos2,file="simplx25.csv",row.names = TRUE)


geo_mean(c(11000,1500,400,1000,600,4000,2500))
mean(c(11000,1500,400,1000,600,4000,2500))
median(c(11000,1500,400,1000,600,4000,2500))
