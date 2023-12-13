# Lectura de datos
problemas<-read.table("pp.txt",header=FALSE)
problemas=problemas[,1]
problemas <- c(problemas,c("d3n16R4R9d1d1", "d3n16R8R9d1d1", "d3n16R8R9d1d05", "d5n8R4R6d01d05", "d6n6R0R6d01d05", "d6n6R1R6d01d1", "d7n5R0R6d01d05", "d7n5R1R6d01d05", "d7n5R1R6d005d05")) #d7n5R2R6d005d05  d7n5R2R6d01d05   d7n5R2R6d01d1
problemas <- c(problemas,c("2658", "2698", "3385", "ex5_3_3", "ex8_4_1", "kall_circles_c8a", "kall_congruentcircles_c71", "wastewater12m1", "wastewater15m2", "waterund17"))

ex<-grep("ex",problemas)
kall<-grep("kall",problemas)
pool<-grep("pool",problemas)
st<-grep("st_",problemas)
water<-grep("water",problemas)
d<-grep("^d",problemas)
ind<-c(ex,kall,pool,st,water,d)
problemas[-ind]

num_problemas_d <- length(problemas[d])
colores<-numeric(length(problemas))+1
colores[ex]<-"#F7BC00" #naranja
colores[kall]<-"#F70000" #rojo
colores[pool]<-"#F700F1" #granate
colores[st]<-"#F3FF00" #amarillo
colores[water]<-"#006FFF" #azul


coloresd <- colorRampPalette(c("lightgreen", "darkgreen"))(6) # 6 clases de densidade de maior a menor (1 0.5 0.1 0.05 0.01 0.005)

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

