# NOMBRE: Dharius Antonio Cabrera Valladares
# MATRÍCULA: 2141703

#============================================================
# EXAMEN DE DISEÑOS EXPERIMENTALES
# Prueba t independiente
# Problema 1: Crecimiento de plántulas con y sin cercado
# ============================================================

# ------------------------------------------------------------
# 1. Cargar paquetes
# ------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("car")

library(tidyverse)
library(car)

# ------------------------------------------------------------
# 2. Crear el conjunto de datos
# ------------------------------------------------------------

datos <- data.frame(
  tratamiento = rep(c("Cercado", "Sin cercado"), each = 25),
  altura_cm = c(
    34.8, 35.2, 36.1, 35.7, 36.5,
    34.9, 35.6, 36.2, 35.4, 36.0,
    35.8, 36.4, 35.1, 35.9, 36.3,
    34.7, 35.5, 36.1, 35.3, 36.6,
    35.7, 36.2, 35.0, 35.8, 36.4,
    
    28.1, 27.9, 29.0, 28.4, 29.1,
    27.8, 28.6, 29.3, 28.2, 28.7,
    28.5, 29.2, 27.7, 28.8, 29.0,
    27.6, 28.3, 29.1, 28.0, 29.4,
    28.5, 28.9, 27.8, 28.6, 29.2
  )
)

# ------------------------------------------------------------
# 3. Revisar estructura de los datos
# ------------------------------------------------------------

# Respuesta del estudiante:


# Promedio por grupo
aggregate(altura ~ tratamiento, datos, mean)

# Desviación estándar
aggregate(altura ~ tratamiento, datos, sd)

# 
# Número de grupos comparados:
# 
2
# ------------------------------------------------------------
# 4. Estadística descriptiva por grupo
# ------------------------------------------------------------

datos %>%
  group_by(tratamiento) %>%
  summarise(
    n = n(),
    media = mean(altura_cm),
    desviacion_estandar = sd(altura_cm),
    minimo = min(altura_cm),
    maximo = max(altura_cm)
  )

# Respuesta del estudiante:
# ¿Qué grupo presentó mayor promedio? el cercado con 35.73
# 
# ¿La diferencia observada parece relevante desde el punto de vista forestal?si, ya que debido debido cerca alcanzaron una altura promedio promedio mas alta que las plantas sijn proteccion 
#
# ------------------------------------------------------------
# 5. Gráfico exploratorio
# ------------------------------------------------------------

ggplot(datos, aes(x = tratamiento, y = altura_cm)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, size = 2) +
  labs(
    title = "Altura de plántulas por tratamiento",
    x = "Tratamiento",
    y = "Altura de plántulas (cm)"
  ) +
  theme_minimal()

# Respuesta del estudiante:
# Describa la distribución de los datos en cada grupo:
#  el tratamiento con cercado tiene valores mas altos, los 2 grupos presentan una dispersion creo uniforme , esto nos demuestra valores tipó atipicos, las distribuciones parecen simetricas y adecuadas para  hacer pruebas parametricass
# ------------------------------------------------------------
# 6. Prueba de normalidad Shapiro-Wilk
# ------------------------------------------------------------

by(datos$altura_cm, datos$tratamiento, shapiro.test)

# Respuesta del estudiante:
# Valor p para el grupo Cercado: 0.381
# 
# Valor p para el grupo Sin cercado: 0.243
# 
# ¿Se cumple el supuesto de normalidad? si, ya que los dos valores son mayorres a 0.5
# 

# ------------------------------------------------------------
# 7. Evaluación de homogeneidad de varianzas
# ------------------------------------------------------------

leveneTest(altura_cm ~ tratamiento, data = datos)

# Respuesta del estudiante:
# Valor p de la prueba de Levene: p= 0.963
# 
# ¿Se cumple el supuesto de homogeneidad de varianzas? si, pq el valor de la prueba es mayor a 0.05, esto significa que la varianza en ambos grupós por decirlom asi son iguales
# 

# ------------------------------------------------------------
# 8. Aplicar prueba t independiente
# ------------------------------------------------------------

resultado_t <- t.test(
  altura_cm ~ tratamiento,
  data = datos,
  var.equal = TRUE
)

resultado_t

# Respuesta del estudiante:
# Estadístico t: 45.66
# 
# Grados de libertad: 48
# 
# Valor p: 3.28 × 10⁻⁴
# 
# Intervalo de confianza:  6.864 a 7.496
# 

# ------------------------------------------------------------
# 9. Interpretación estadística
# ------------------------------------------------------------

# Respuesta del estudiante:
# ¿Se rechaza o no se rechaza la hipótesis nula? si sw rechaZA debido a que el valor de P es mucho menor a 0.5
# 
# ¿Existe diferencia estadísticamente significativa entre tratamientos? si ya que el valor obtenido de p en las pruebas es menor a 0.05 esto demuestra que el cercado influye en el crecimiento de las plantulas
# 

# ------------------------------------------------------------
# 10. Interpretación forestal
# ------------------------------------------------------------

# Respuesta del estudiante:
# Explique qué significa el resultado en el contexto del crecimiento de plántulas:  es mejor el tratamiento con cercasdo ya con este tratamiento se aclcanzan alturas promedias mucho mas altasn que sin el cercado
# 
