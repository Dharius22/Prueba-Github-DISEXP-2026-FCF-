set.seed(2007) # Para reproducibilidad

# Número de observaciones
n <- 20

# Generar horas de estudio (entre 1 y 10)
Horas_estudio <- sample(1:10, n, replace = TRUE)

# Asignar probabilidad de aprobar en función de horas de estudio
# A más horas, más alta probabilidad
Resultado <- sapply(Horas_estudio, function(horas) {
  ifelse (runif(1) < (horas / 10), "Aprobado", "Reprobado")
})

# Crear data frame
estudio <- data.frame(
  Estudiante = 1:n,
  Horas_estudio,
  Resultado)

# Crear variable dicotómica: 1 = Aprobado, 0 = Reprobado
estudio$Resultado_bin <- ifelse(estudio$Resultado == "Aprobado",1,0)
head(estudio)

cor.test(estudio$Horas_estudio, estudio$Resultado_bin, method = "pearson")