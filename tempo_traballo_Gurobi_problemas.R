setwd("C:/Users/Usuario/OneDrive - Universidade de Santiago de Compostela/Escritorio/RAPOsa/Tempos/Tempos e traballo no nodo raiz (con AMPL)")
####----------- Xerar os datos ----------####
# Vector co nome dos problemas
problemas <-c("d3n16R4R9d1d05","d3n16R4R9d1d1","d3n16R8R9d1d05","d3n16R8R9d1d1","d4n12R3R7d05d05","d4n12R3R7d05d1",
              "d4n12R6R7d05d05","d4n12R6R7d05d1","d5n8R0R6d05d05","d5n8R2R6d05d05","d5n8R2R6d05d1","d5n8R4R6d05d05",
              "d5n8R4R6d05d1","d6n6R1R6d05d05", "d6n6R1R6d05d1",   "d6n6R3R6d05d05" , "d6n6R3R6d05d1" ) # rownames(dt[which(dt$LinearSolvingTimeBySolver/dt$VisitedNodes>1),]) #
# Inicializar data frame para almacenar os resultados
resultados_df <- data.frame(Problema = character(),
                            Segundos = numeric(),
                            Traballo = numeric(),
                            stringsAsFactors = FALSE)

# Loop sobre cada problema
for (problema in problemas) {
  # Construír o camiño do ficheiro
  camiño <- paste0(problema, "_log.txt")
  
  # Lendo o contido do ficheiro
  texto <- readLines(camiño)
  
  # Inicializando vectores para armazenar os valores
  segundos <- numeric(length(texto))
  traballo <- numeric(length(texto))
  
  # Loop para percorrer liñas do ficheiro e extraer os valores desexados
  for (i in seq_along(texto)) {
    # Buscar liñas que conteñan "seconds" para extraer o tempo e traballo
    if (grepl("seconds", texto[i])) {
      # Extracción dos segundos
      segundos[i] <- as.numeric(sub(".*(\\d+\\.\\d+) seconds.*", "\\1", texto[i]))
      
      # Extracción do traballo
      traballo[i] <- as.numeric(sub(".*\\((\\d+\\.\\d+) work units\\).*", "\\1", texto[i]))
    }
  }
  
  # Eliminar elementos con valor 0
  segundos <- segundos[-which(segundos == 0)]
  traballo <- traballo[-which(traballo == 0)]
  
  if (problema=="d4n12R6R7d05d05") {
    segundos[11]=segundos[11]+10
  }
  # Calcular os valores relativos
  segundos_rel <- segundos / segundos[11]  # O primeiro valor é o de referencia
  traballo_rel <- traballo / traballo[11]  # O primeiro valor é o de referencia
  
  
  # Crear un data frame co resultado do problema actual
  resultado_actual <- data.frame(Problema = rep(problema, length(segundos)),
                                 Segundos = segundos,
                                 Traballo = traballo,
                                 Seg_Rel = segundos_rel,
                                 Trab_Rel = traballo_rel,
                                 stringsAsFactors = FALSE)
  
  # Engadir os resultados ao data frame principal
  resultados_df <- rbind(resultados_df, resultado_actual)
}

# Imprimir os resultados finais
Configuracion=rep(c("10","20","30","40","50","60","70","80","90","100","110"),17)
resultados_df=cbind(resultados_df,Configuracion)
resultados_ns=resultados_df[-c(which(resultados_df$Configuracion==100),which(resultados_df$Configuracion==100)+1),]

head(resultados_df,12)

resultados_df[which(resultados_df$Seg_Rel>2),]

#####------------- GRAFICOS ------------#####

## Boxplot comparando as configuracions tendo en conta os segundos relativos
library(ggplot2)
ggplot(resultados_ns, aes(x = Configuracion, y = Seg_Rel*100)) +
  geom_boxplot(fill = "lightblue", color = "blue", alpha = 0.7) +
  labs(title = "Porcentaxe do tempo de resolución de cada configuración respecto á estándar",
       x = "Configuración",
       y = "Porcentaxe") +
  theme_minimal()+ # intercambia colores do grid co fondo
  scale_y_continuous(breaks = seq(0, 200, by = 20))
# Hai que ter en conta que estamos selecionando problemas nos que o tempo medio por nodo e maior a un (pois doutro xeito os resultados porderian ser inconsistentes)

# Tendo en conta o traballo
ggplot(resultados_ns, aes(x = Configuracion, y = Trab_Rel*100)) +
  geom_boxplot(fill = "lightblue", color = "blue", alpha = 0.7) +
  labs(title = "Porcentaxe de traballo ao resolver o nodo raiz respecto á versión estándar",
       x = "Configuración",
       y = "Porcentaxe") +
  theme_minimal()+ # intercambia colores do grid co fondo
  scale_y_continuous(breaks = seq(0, 200, by = 20))

#' Set parameter LogFile to value "d3n16R4R9d1d05_log.txt"
#' Set parameter InfUnbdInfo to value 1
#' Gurobi Optimizer version 10.0.2 build v10.0.2rc0 (win64)
#' 
#' CPU model: Intel(R) Core(TM) i7-7500U CPU @ 2.70GHz, instruction set [SSE2|AVX|AVX2]
#' Thread count: 2 physical cores, 4 logical processors, using up to 1 threads
#' 
#' Gurobi 10.0.2 (win64) logging started Tue Dec 12 11:46:02 2023


