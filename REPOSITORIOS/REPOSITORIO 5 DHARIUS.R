# Datos climaticos de cumbres
  
  cumbres <- read.csv("cumbres.csv", header = T)
  boxplot(cumbres$TEMP,
          col = "pink",
          ylim = c(10, 35),
          xlab = "Cumbres",
          ylab = "Temperatura")
  abline(h=21.8, col ="indianred", lwd =3, lty ="dashed") # media observada
  abline(h=mean(cumbres$TEMP), col = "violet", lwd=3, lty= "solid")
  mean(cumbres$TEMP) #media historica
  
  t.test(mu=21.8, cumbres$TEMP)
 
  
   # Muestras dependientes
    
    inventario <- read.csv("inventario.csv", header = T)
  inventario$Fecha <- as.factor(inventario$Fecha)
  boxplot(inventario$Kilogramo ~ inventario$Fecha,
          xlab= "Año",
          ylab= "Semilla",
          col= "lightgreen",
          ylim= c(40, 60))
  
  tapply(inventario$Kilogramo, inventario$Fecha, mean)
  
  T2012 <- subset(inventario$Kilogramo, inventario$Fecha == "T2012")
  T2014 <- subset(inventario$Kilogramo, inventario$Fecha == "T2014")
  
  t.test(T2012, T2014, paired = T)