# importar datos

cumb <- read.csv ("cumbres.csv", header = T)

boxplot(cumb$TEMP
        col = "skyblue",
        ylim = c(10,35),
        xlab = "Cumbres",
        ylab = "Temperatura")

abline(h=23, col = "indianred", lwd =2, lty = "dashed")
abline(h=mean(cumb$TEMP), col = "darkgray", lwd=3, lty = "solid")
mean(cumb$TEMP)

t.test(mu= 21.8, cumb$TEMP)

# muestras dependientes

inv <- read.csv("inventario.csv", header = T)
inv$Fecha <- as.factor(inv$Fecha)
boxplot(inv$Kilogramo ~ inv$Fecha,
        xlab = "Año",
        ylab = "semilla (kg/ind)",
        col = "lightgreen",
        ylim = c(40, 60) )

tapply(inv$Kilogramo, inv$Fecha, mean)

T2012 <- subset(inv$Kilogramo, inv$Fecha == "T2012")
T2014 <- subset(inv$Kilogramo, inv$Fecha == "T2014")

t.test(T2012, T2014, paired = T)

