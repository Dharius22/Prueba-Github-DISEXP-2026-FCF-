#Coeficiente de correlación de Spearman ----------------------------------
  
  
  resp <- data.frame (
    Tiempo =c(12, 15, 17, 18, 20, 21, 22, 26),
    Edad = c(14, 25, 20, 35, 45, 30, 60, 95)
  )
  resp
  
  plot(resp$Tiempo, resp$Edad,
       col = "black",
       pch = 19,
       xlab = "Tiempo de respuesta",
       ylab= "Edad")
  
  resp$Rango_Tiempo <- rank(resp$Tiempo, ties.method = "first")
  resp$Rango_Edad <- rank(resp$Edad, ties.method = "first")
  
  # Ver resultado
  resp
  
  plot(resp$Rango_Tiempo, resp$Rango_Edad,
       col = "purple", #color de datos
       pch = 19, #cambia el simbolo
       cex = 1.2, #modifica el tamaño del simbolo
       xlab = "Rango variable tiempo", #Eje x
       ylab= "Rango edad" #eje Y
)
  
  cor.test(resp$Rango_Tiempo, resp$Rango_Edad, method = "spearman")
  
  
  
  # Tau de Kendall ----------------------------------------------------------
  
  
  tau <- data.frame(
    A = c(1, 2, 3, 4, 5, 6),
    B = c(3, 1, 4, 2, 6, 5)
  )
  cor.test(tau$A, tau$B, method = "kendall")
  
  
  plot(tau$A, tau$B,
       col = "purple", #color de datos
       pch = 19, #cambia la forma del simbolo
       cex = 1.4, #modifica el tamaño del simbolo
       xlab = "Evaluación med. A", #Eje x
       ylab= "Evaluación med. B" #eje Y)
  )
  
  # La evaluacion de la salud de los pacientes por ambos medicos de forma individual no es coincidente
  # su coefciente de correlacion es medio (0.46)
  # y no presenta significancia estadistica (p-value= 0.2722)