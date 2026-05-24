# Pruebas de T

calidad <- read.csv("ejemplo_2.csv", header =T)

calidad$Tratamiento <- as.factor(calidad$Tratamiento)


boxplot(calidad$IE ~ calidad$Tratamiento,
        xlab = "Nivel de factor",
        ylab = "Frecuencia",
        col= "orange")

# Prueba de T con variables independientes 
# Hipotesis nula, no hay diferencia en la media del inidice de esbeltez de plantas con y sin fertilizantes
# Hipotesis alternativa, si hay diferencias en la media 

# Separar los niveles de factor

Ctrl <- (subset(calidad$IE, calidad$Tratamiento == "Ctrl"))
Fert <- (subset(calidad$IE, calidad$Tratamiento != "Ctrl"))

shapiro.test(Ctrl)
shapiro.test(Fert)

var(Ctrl)
var(Fert)

bartlett.test(calidad$IE ~ calidad$Tratamiento)

t.test(calidad$IE ~ calidad$Tratamiento, var.equal = T)