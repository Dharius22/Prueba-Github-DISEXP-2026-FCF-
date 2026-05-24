# MAGT
# 14/04/2026
# Datos climatologicos
# Cumbres NL, Villagran Tamps, Carranza Coahuila

library(lubridate)
clima <- read.csv("Climatologia (1).csv", header = TRUE)

clima$Localidad <- as.factor(clima$Localidad)
clima$Fecha <- as.Date(clima$Fecha, format = "%d/%m/%Y")
clima$fechaHora <- as.POSIXct(paste(clima$Fecha, clima$Hora),
                              format = "%Y-%m-%d %H:%M:%S")

# Boxplot de Temperatura por Localidad
boxplot(clima$TEMP ~ clima$Localidad,  # Cambié TA por TEMP
        col = "indianred",
        ylab = "Temperatura (°C)",     # Cambié el label también
        xlab = "Localidades",
        ylim = c(10, 35),
        outcol = "indianred",
        outpch = 19)

# Promedio por localidad
tapply(clima$TEMP, clima$Localidad, mean)  # Cambié TA por TEMP

# Temperatura en el curso de la semana - Cumbres
Cumbres <- subset(clima, Localidad == "Cumbres")
Cumbres <- Cumbres[order(Cumbres$fechaHora), ]  # usa fechaHora con minúscula

plot(Cumbres$fechaHora, Cumbres$TEMP,  # Cambié FechaHora por fechaHora y TA por TEMP
     type = "l",
     col = "indianred",
     lwd = 2,
     xaxt = "n",
     xlab = "Fecha",
     ylab = "Temperatura (°C)",
     main = "Serie de tiempo Temp - Cumbres")
axis.POSIXct(1, at = pretty(Cumbres$fechaHora), format = "%d/%m")  # Para que salgan fechas en el eje X

# Normalidad de datos - ejemplo con Shapiro por localidad
tapply(clima$TEMP, clima$Localidad, shapiro.test)

# ANOVA
clim.aov <- aov(clima$TEMP ~ clima$Localidad)
summary(clim.aov)

# Aceptamos H1, por lo tanto realizamos una prueba de Tukey
TukeyHSD(clim.aov)

# Homogeneidad de varianza
tapply(clima$TEMP, clima$Localidad, var)
bartlett.test(clima$TEMP ~ clima$Localidad)
