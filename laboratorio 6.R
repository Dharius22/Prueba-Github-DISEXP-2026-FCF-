#MAGT
#14/04/2026
#Datos climatologicos
#Cumbres NL, Villagran Tamps, Carranza Coahuila

clima <- read.csv("climatologia.csv",header = T)
climatologia.csv <-clima$Localidad <- as.factor(clima$Localidad)
clima$Fecha <- as.Date(clima$Fecha, format= "%d/%m/%Y")
# clima$Hora <- as.POSIXct(clima$Hora, format = %H:%M:%S)

 boxplot(clima$TEMP~ clima$Localidad,
        col= "indianred",
        ylab= "Temp("c)",
        xlab= "Localidades",
        ylim =c(10,35),
        outcol= "indianred", #marcar outliers en rojo
        outpch = 19) # cambiar la forma  A circulo relleno
        
        tapply(clima$TEMP, clima$Localidad, mean)
        
        # Temperatura en el curso de la semana
        
        cumbres <- subset(clima, Localidad = "Cumbres")
        cumbres <- cumbres [order(cumbres$Fecha),]
        
        Plot(cumbres$Fecha, cumbres$TEMP,
        type = 1
        col= "indianred",
        1wd = 2,
        xaxt = "n"
        xlab = "Localidad EMAs"
        ylab = "Temperatura ( C )")
        
        
        
        
        
        
        