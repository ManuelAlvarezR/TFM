setwd("C:/Users/Usuario/OneDrive - Universidade de Santiago de Compostela/Escritorio/RAPOsa/Benchmarking/runfiles_run")

###############################################################

# Crear unha lista cos nomes dos problemas
problemas<-read.table("pp.txt",header=FALSE)
problemas=problemas[,1]
problemas <- c(problemas,c("d3n16R4R9d1d1", "d3n16R8R9d1d1", "d3n16R8R9d1d05", "d5n8R4R6d01d05", "d6n6R0R6d01d05", "d6n6R1R6d01d1", "d7n5R0R6d01d05", "d7n5R1R6d01d05", "d7n5R1R6d005d05")) #d7n5R2R6d005d05  d7n5R2R6d01d05   d7n5R2R6d01d1
problemas <- c(problemas,c("2658", "2698", "3385", "ex5_3_3", "ex8_4_1", "kall_circles_c8a", "kall_congruentcircles_c71", "wastewater12m1", "wastewater15m2", "waterund17"))
# which((table(problemas))==2)

# Crear una lista cos sufixos
sufixos <- c("time", "FO","time_elapsed","time_user")

# Inicializar unha lista valeira para almacenar os datos
lista_de_datos <- list()

# Bucle externo para iterar a través dos problemas
for (problema in problemas) {
  # Inicializar una lista vacía para cada problema
  datos_problema <- list()
  
  # Bucle interno para iterar a través dos sufixos
  for (sufixo in sufixos) {
    # Construir el nombre del archivo
    nombre_archivo <- paste(problema, sufixo, sep = "_")
    # Leer el archivo
    datos <- read.table(paste(nombre_archivo,".log",sep=""), header = FALSE, col.names = c(nombre_archivo))
    # Agregar los datos a la lista del problema actual
    datos_problema[[sufixo]] <- datos
  }
  
  datos_fo <- as.matrix(datos_problema[["FO"]])
  datos_time_elapsed <- as.matrix(datos_problema[["time_elapsed"]])
  
  RG <- round(abs(datos_fo[11,1] - datos_fo[,1])/max(abs(datos_fo[11,1]-datos_fo[1,1]),1),2)*100 # % gap relativo = |valor optimo-valor optimo parando|/max(|valor optimo-valor optimo co 10% (das iter)|,1) *100
  RG_time_elapsed <- round((datos_time_elapsed[11,1] - datos_time_elapsed[,1]) / datos_time_elapsed[11,1]*100,2) # % gap relativo tempo
  
  # Agregar os datos de Gap Relativo (RG) a lista do problema actual
  datos_problema[["RG"]] <- RG
  datos_problema[["RG_time_elapsed"]] <- RG_time_elapsed
  
  
  # Combinar os datos do problema actual nun data.frame
  datos_problema_combinados <- do.call(cbind, datos_problema)
  
  # Agregar os datos do problema actual a lista general
  lista_de_datos[[problema]] <- datos_problema_combinados
}
sufixos<- c(sufixos, "RG","RG_time_elapsed")

# Imprimir los datos por pantalla (un data.frame por cada problema)
#for (problema in problemas) {
#  print(lista_de_datos[[problema]])
#}

# Crear a matriz cos valores do gap relativo (RG)
matriz_RG <- sapply(lista_de_datos, function(datos) datos[["RG"]])
iter_max_perc<-seq(10,110,by=10)
df_RG <- data.frame(iter_max_perc,matriz_RG)

# O mesmo para os tempos
matriz_RG_time_elapsed <- sapply(lista_de_datos, function(datos) datos[["RG_time_elapsed"]])
iter_max_perc<-seq(10,110,by=10)
df_RG_time_elapsed <- data.frame(iter_max_perc,matriz_RG_time_elapsed)


#----- Gap Relativo vs max_iter ------
cc=seq(10,110,by=10)
# Datos
RG <- lapply(1:length(problemas), function(i) matriz_RG[,i])
# Crea un data frame con los datos
df <- data.frame(cc = rep(cc, times = length(problemas)), Valor = unlist(RG), Problema = factor(rep(1:length(problemas), each = length(cc))))

##### Gráfica 1####
library(ggplot2)
windows()
ggplot(df, aes(x = cc, y = Valor, color = Problema)) +
  geom_line() +
  geom_point() +
  xlim(10, 100) +
  ylim(0, 100) +
  labs(x = "% do número total de iteracións", y = "% Gap relativo ", color = "Problemas") +
  # scale_color_manual(values = c("ex","kall","pool","water","d","others"),labels = c("#F7BC00","#F70000","#F700F1","#006FFF",1,"green")) +
  # scale_color_viridis(discrete = TRUE)
  theme_minimal()+
  theme(legend.position = "none")
# legend("topright", legend = c("ex","kall","pool","water","d","others"), col=c("#F7BC00","#F70000","#F700F1","#006FFF",1,"green"),lty = 1)
####### Grafica 2 #########
library(plotly)
p <- ggplot(df, aes(x = cc, y = Valor, color = Problema)) +
  geom_line() +
  geom_point() +
  xlim(10, 100) +
  ylim(0, 100) +
  labs(x = "% do número total de iteracións", y = "% Gap relativo ", color = "Problemas") +
  theme_minimal()

ggplotly(p)
##### Grafica 3 #########
ggplot(df, aes(x = cc, y = Valor, color = Problema)) +
  geom_line() +
  geom_point() +
  xlim(10, 100) +
  ylim(0, 100) +
  labs(x = "% do número total de iteracións", y = "% Gap relativo ", color = "Problemas") +
  theme_minimal() +
  facet_wrap(~ Problema, scales = "free_y")+
  theme(legend.position = "none")
##### Gráfica 4 #######
ggplot(df, aes(x = cc, y = Valor, color = Problema)) +
  geom_line() +
  geom_point() +
  xlim(10, 100) +
  ylim(0, 100) +
  labs(x = "% do número total de iteracións", y = "% Gap relativo ", color = "Problemas") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_smooth(aes(x = cc, y = Valor), method = "loess", color = "black", se = FALSE)  # suavizado LOESS
#### Gráfica 5 #####
ggplot(df[-seq(11,dim(df)[1],by=11),], aes(x = as.factor(cc), y = Valor)) +
  geom_boxplot(fill = "lightblue", color = "blue", alpha = 0.7) +
  labs(x = "% do número total de iteracións", y = "% Gap relativo") +
  theme_minimal()+scale_y_continuous(breaks = seq(0, 200, by = 20))

#### Gap dos problemas selecionados para o exemplo do TFM #c("d7n5R2R6d1d05","kall_diffcircles_7","d7n5R2R6d0005d05","0975")
windows()
# Crear un vector cos nomes dos problemas
problem_names <- c("0975","d7n5R2R6d0005d05","d7n5R2R6d1d05","kall_diffcircles_7")
# Supoñendo que os problemas que queres cambiar son 1, 40, 50 e 56
problem_mapping <- setNames(problem_names, c(1, 40, 50, 56))
# Modificar o dataframe para substituír os números polos nomes
df$Problema <- factor(df$Problema, levels = c(1, 40, 50, 56), labels = problem_names)
ggplot(df[df$Problema %in% problem_names,], aes(x = cc, y = Valor, color = Problema,linetype = Problema)) +
  geom_line() +
  geom_point() +
  xlim(10, 100) +
  ylim(0, 100) +
  labs(x = "% do número total de iteracións", y = "% Gap relativo ", color = "Problema", linetype = "Problema") +
  theme_minimal() +
  scale_color_manual(values = alpha(c("green", "blue", "red", "purple"), 0.5), 
                     breaks = problem_names, 
                     labels = problem_names)+
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash"),
                      breaks = problem_names,
                      labels = problem_names)

####### COLOR ##########
ex<-grep("ex",problemas)
kall<-grep("kall",problemas)
pool<-grep("pool",problemas)
st<-grep("st_",problemas)
water<-grep("water",problemas)
d<-grep("^d",problemas)
ind<-c(ex,kall,pool,st,water,d)
problemas[-ind]

num_problemas_d <- length(problemas[d])
colores<-numeric(length(problemas))
colores[ex]<-"#F7BC00" #naranja
colores[kall]<-"#F70000" #rojo
colores[pool]<-"#F700F1" #granate
colores[st]<-"#F3FF00" #amarillo
colores[water]<-"#006FFF" #azul
colores[-ind]<-"green"

#coloresd <- colorRampPalette(c("lightgreen", "darkgreen"))(6) # 6 clases de densidade de maior a menor (1 0.5 0.1 0.05 0.01 0.005)
library(RColorBrewer)
library(scales)
coloresd <- brewer.pal(7, "Paired")[-7]
  # viridis_pal(end = 0.9,direction = 1,option = "F")(6)
  # viridis_pal(end = 1,direction = -1,option = "D")(6) 
  # brewer.pal(6, "Paired")
calcular_ind <- function(ff) {
  ncff <- nchar(ff)
  ff_as_integer <- as.integer(ff)
  if (ncff == 4) {
    ind <- 1
  } else if (ncff == 1) {
    ind <- 6
  } else if (ncff == 2) {
    if (ff_as_integer == 5) {
      ind <- 5
    } else if (ff_as_integer == 1) {
      ind <- 4
    } else {
      ind <- 2
    }
  } else if (ncff == 3) {
    if (ff_as_integer == 5) {
      ind <- 3
    } else {
      ind <- 2
    }
  } else {
    ind <- 2
  }
  return(ind)
}

for (i in d){
  ff<-strsplit(problemas[i],"d")[[1]];ff
  ff<-ff[length(ff)-1];ff
  colores[i]<-coloresd[calcular_ind(ff)] #asocias o problema i o color verde con intensidad asociada a densidade
}

######### PLOTS COLOR ##########

#----- Gap Relativo vs max_iter (Sin ggplot) ------
windows(1600,1200)

plot(cc, as.matrix(as.data.frame(lista_de_datos[1])[5]),xlim=c(10,100),ylim=c(0,100),col=1,type = "l",lwd=1,xlab="% do número total de iteracións",ylab="% Gap relativo ")
grid()
for (i in 1:length(problemas)) {
  points(cc, as.matrix(as.data.frame(lista_de_datos[i])[5]), col = colores[i])
  lines(cc, as.matrix(as.data.frame(lista_de_datos[i])[5]), col = colores[i])
}
# Lenda
legend("topright", legend = c("ex","kall","pool","water","d","others"), col=c("#F7BC00","#F70000","#F700F1","#006FFF",1,"green"),lty = 1)
# d_types<-c(1,0.5,0.1,0.05,0.01,0.005)


###############-------------- Ver Graficas de COlores por separado -------------#####################
# d problemas
windows(1600,1200)
plot(c(10,100), c(100,0),xlim=c(10,100),ylim=c(0,100),col=0,type = "l",lwd=1,xlab="% do número total de iteracións",ylab="% Gap relativo ")
grid()
for (i in d) {
  points(cc, as.matrix(as.data.frame(lista_de_datos[i])[5]), col = colores[i])
  lines(cc, as.matrix(as.data.frame(lista_de_datos[i])[5]), col = colores[i])
}
d_types<-c("1","0.5","0.1","0.05","0.01","0.005")
legend("topright", legend = d_types, col=rev(coloresd),lty = 1,title="Densidade")
# d problemas separados por densidade
for (j in 1:length(coloresd)){
  windows(1600,1200)
  plot(c(10,100), c(100,0),xlim=c(10,100),ylim=c(0,100),col=1,type = "l",lwd=1,xlab="% do número total de iteracións",ylab="% Gap relativo ")
  grid()
  for (i in which(colores==coloresd[j])) {
    points(cc, as.matrix(as.data.frame(lista_de_datos[i])[5]), col = colores[i])
    lines(cc, as.matrix(as.data.frame(lista_de_datos[i])[5]), col = colores[i])
  }
}

# ex problemas
windows(1600,1200)
plot(c(10,100), c(100,0),xlim=c(10,100),ylim=c(0,100),col=1,type = "l",lwd=1,xlab="% do número total de iteracións",ylab="% Gap relativo ")
grid()
for (i in ex) {
  points(cc, as.matrix(as.data.frame(lista_de_datos[i])[5]), col = colores[i])
  lines(cc, as.matrix(as.data.frame(lista_de_datos[i])[5]), col = colores[i])
}

# kall problemas
windows(1600,1200)
plot(c(10,100), c(100,0),xlim=c(10,100),ylim=c(0,100),col=1,type = "l",lwd=1,xlab="% do número total de iteracións",ylab="% Gap relativo ")
grid()
for (i in kall) {
  points(cc, as.matrix(as.data.frame(lista_de_datos[i])[5]), col = colores[i])
  lines(cc, as.matrix(as.data.frame(lista_de_datos[i])[5]), col = colores[i])
}
# water problemas
windows(1600,1200)
plot(c(10,100), c(100,0),xlim=c(10,100),ylim=c(0,100),col=1,type = "l",lwd=1,xlab="% do número total de iteracións",ylab="% Gap relativo ")
grid()
for (i in water) {
  points(cc, as.matrix(as.data.frame(lista_de_datos[i])[5]), col = colores[i])
  lines(cc, as.matrix(as.data.frame(lista_de_datos[i])[5]), col = colores[i])
}
# st problemas
windows(1600,1200)
plot(c(10,100), c(100,0),xlim=c(10,100),ylim=c(0,100),col=1,type = "l",lwd=1,xlab="% do número total de iteracións",ylab="% Gap relativo ")
grid()
for (i in st) {
  points(cc, as.matrix(as.data.frame(lista_de_datos[i])[5]), col = colores[i])
  lines(cc, as.matrix(as.data.frame(lista_de_datos[i])[5]), col = colores[i])
}
# pool problemas
windows(1600,1200)
plot(c(10,100), c(100,0),xlim=c(10,100),ylim=c(0,100),col=1,type = "l",lwd=1,xlab="% do número total de iteracións",ylab="% Gap relativo ")
grid()
for (i in pool) {
  points(cc, as.matrix(as.data.frame(lista_de_datos[i])[5]), col = colores[i])
  lines(cc, as.matrix(as.data.frame(lista_de_datos[i])[5]), col = colores[i])
}

###############################################################################
#------- FO vs max_iter -----------
cc=seq(10,110,by=10)
#par(mfrow = c(7, 7))
#  c(1, 40, 50, 56)
# c("solid", "dashed", "dotted", "dotdash")
# alpha(c("green", "blue", "red", "purple"), 0.5)
for (p in 1:length(problemas)){#p=1
  windows()
  plot(cc[-11], as.matrix(as.data.frame(lista_de_datos[p])[2])[-11,], type = "l", col = "purple",lty="dotdash", lwd=1,xlab="% do número total de iteracións",ylab="Función obxectivo")
  #points(cc[-11], as.matrix(as.data.frame(lista_de_datos[p])[2])[-11,],col=4,pch=16)
  # Etiquetas de ejes y títulos
  axis(1, at = cc, labels = cc)
  #axis(2, las = 1)
  #title(main = "Evolución do valor da función obxectivo no símplex dual")
  grid()
  legend("topleft", legend = problemas[p], col ="purple",lty = "dotdash")
}
#------- time vs max_iter -----------
# Graficamos los cuatro vectores en un mismo gráfico
cc=seq(10,110,by=10)

#for (p in 1:length(problemas)){
p=1
windows()
plot(cc, as.matrix(as.data.frame(lista_de_datos[p])[3]), type = "l", col = 4,lwd=1,xlab="% do número total de iteracións",ylab="Función obxectivo")
# Etiquetas de ejes y títulos
axis(1, at = cc, labels = cc)
#axis(2, las = 1)
title(main = "Tempos no Simplex Dual")
grid()
legend("topleft", legend = problemas[p], col =4,lty = 1)
#}


#---- Gap TEMPOS vs max_iter  (Sin ggplot) --------


############################### EXCEL #################################################
# Cargar la biblioteca openxlsx
library(openxlsx)

# Crear un nuevo libro de Excel
wb <- createWorkbook()

# Creating a style for the headers in the style sheet
hs1 <- createStyle(fgFill = "#DCE6F1", halign = "CENTER", textDecoration = "bold", border = c("top", "bottom", "left", "right") )


# Crear hojas para cada problema y agregar las tablas correspondientes
for (problema in problemas) {
  # Crear una hoja con el nombre del problema
  addWorksheet(wb, sheetName = problema)
  
  # Obtener los datos del problema actual
  datos_problema <- lista_de_datos[[problema]]
  
  # Escribir los datos en la hoja de Excel
  writeData(wb, sheet = problema, x = datos_problema, borders="surrounding",headerStyle = hs1)
}

# Guardar el libro de Excel en un archivo
saveWorkbook(wb, file = "resultados2.xlsx")

# Imprimir un mensaje de confirmación
cat("Los resultados se han exportado a 'resultados2.xlsx'\n")

wb <- createWorkbook()
addWorksheet(wb, sheetName = "RG")
writeData(wb, sheet = "RG", x = df_RG,borders="surrounding",headerStyle = hs1)
saveWorkbook(wb, file = "tabla_RG_2.xlsx")
cat("La tabla RG se ha exportado a 'tabla_RG.xlsx'\n")
