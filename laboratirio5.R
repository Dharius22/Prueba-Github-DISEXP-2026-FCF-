#MAGT
#14/04/2026
#Datos climatologicos
#Cumbres NL, Villagran Tamps, Carranza Coahuila

clima <- read.csv("Climatologia.csv",header = T)
climatologia.csv <-clima$Localidad <- as.factor(clima$Localidad)
clima$Fecha <- as.Date(clima$Fecha, format= "%d/%m/%Y")
# clima$Hora <- as.POSIXct(clima$Hora, format = %H:%M:%S)
clima$Fecha <- as.Date(clima$Fecha, format= "%d%m%Y")

cumbres <- subset(clima,Localidad=cumbres)
cumbres <- cumbres [order(cumbres$fecha),]

plot(cumbres$Fechahora, cumbres$temp,
     
     type =1
     col= "indianred",
     
    lwd=2,

     xlab= "fecha"
     ylab= "temp("C"),
     main= serie de tiempo variable temp")
     