#Anova

localidad <- read.csv("Datos_Rascon_Anova.csv", header = T)
View(localidad)
localidad$Paraje <- as.factor(localidad$Paraje)

boxplot(localidad$DAP ~ localidad$Paraje,
        xlab= "Paraje",
        ylab = "DBH (cm)",
        col= "yellow")
#Normalidad de datos DBH o DAP
shapiro.test(localidad$DAP)

#revisas a profundidad los datos de DAP

hist(localidad$DAP)

mean(localidad$EDAD)

localidad$logDAP <- log10(localidad$DAP)
localidad$sqrDAP <- sqrt(localidad$DAP)
hist(localidad$logDAP)
shapiro.test(localidad$logDAP)
hist(localidad$sqrDAP)
shapiro.test(localidad$logDAP)
shapiro.test(localidad$sqrDAP)

bartlett.test(localidad$sqrDAP ~ localidad$Paraje)


#Se cumplen la normalidad y homogeneidad de varianzas de la 
#variable transformada por raiz cuadrada (sqrt)

#Procedo a realizar el ANOVA con sqrDAP
#funcion aov

Paraje.aov <- aov(localidad$sqrDAP ~ localidad$Paraje)

Paraje.aov

summary(Paraje.aov)

TukeyHSD(Paraje.aov)

plot(TukeyHSD(Paraje.aov))
