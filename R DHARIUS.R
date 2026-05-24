# 23/02/07
# MAGT

# Ingresar datos en R ----------------------------------------------------------

altura <- c(156, 187, 178, 167, 173, 175)

set.seed(123)
altura <- sample (x=160:185, size = 100, replace = TRUE)
mean(altura)
var (altura)
hist(altura,
     col= "lightgreen",
     xlab = "Altura (cm)",
     ylab = "Frecuencia",
     main = "",
     ylim = c(0,30))

stem (altura)

boxplot(altura,
        col= "pink",
        ylab = "Altura (cm)")


shapiro.test(altura)
