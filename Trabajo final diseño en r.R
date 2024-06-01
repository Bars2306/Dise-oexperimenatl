# Instalar y cargar paquetes necesarios
if (!require(readxl)) install.packages("readxl", dependencies=TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies=TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies=TRUE)
if (!require(kableExtra)) install.packages("kableExtra", dependencies=TRUE)
if (!require(car)) install.packages("car", dependencies=TRUE)
if (!require(pwr)) install.packages("pwr", dependencies=TRUE)
if (!require(lmtest)) install.packages("lmtest", dependencies=TRUE)
if (!require(plotly)) install.packages("plotly", dependencies=TRUE)
install.packages("dunn.test")

# Cargar el paquete dunn.test
library(dunn.test)
library(readxl)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(car)
library(pwr)
library(lmtest)
library(plotly)
library(dunn.test)

# Definir la ruta del archivo Excel
ruta_archivo <- "C:/Users/juseb/OneDrive/Documentos/gifthub/Diseño.rebote.xlsx"

# Cargar datos desde el archivo Excel
datos <- read_excel(ruta_archivo)

# Mostrar las primeras filas de los datos para verificar la carga correcta
print(head(datos))

# Cargar paquetes necesarios
library(knitr)
library(kableExtra)

# Crear datos para la tabla resumen de factores
factores <- data.frame(
  Factor = c("Material de Superficie", "Altura de Caída", "Tipo de Pelota"),
  Codificación = c("Cemento (-1), Pasto Sintético (0), Madera (1)", 
                   "50 cm (-1), 100 cm (0), 150 cm (1)", 
                   "Plástico (0), Espuma (1), Goma (-1)"),
  Descripción = c("Tipo de superficie sobre la cual se realiza el experimento", 
                  "Altura desde la cual se deja caer la pelota", 
                  "Material de la pelota utilizada en el experimento"),
  Tipo = c("Categórico", "Numérico", "Categórico")
)

# Crear tabla usando kableExtra para visualización elegante
library(kableExtra)
tabla_factores <- kable(
  factores,
  col.names = c("Factor", "Codificación", "Descripción", "Tipo"),
  caption = "Tabla Resumen de Factores"
) %>%
  kable_styling(full_width = FALSE, font_size = 12)

# Mostrar tabla
tabla_factores


# Identificar y remover outliers usando boxplot.stats
outliers <- boxplot.stats(datos$`Altura de Rebote (cm)`) $out

# Si hay outliers, se eliminan de los datos
if (length(outliers) > 0) {
  datos_limpios <- datos[!datos$`Altura de Rebote (cm)` %in% outliers, ]
} else {
  datos_limpios <- datos
}

# Análisis de medias descriptivas por grupos usando dplyr
medias <- datos_limpios %>%
  group_by(`Material de Superficie`, `Altura de Caída (cm)`, `Tipo de Pelota`) %>%
  summarise(
    media_rebote = mean(`Altura de Rebote (cm)`, na.rm = TRUE),
    sd_rebote = sd(`Altura de Rebote (cm)`, na.rm = TRUE),
    n = n(),
    .groups = "drop"  # Anular la agrupación automática
  )

# Visualización de las medias de rebote usando ggplot2
# Gráfico de barras agrupadas por Material de Superficie y Altura de Caída, facetado por Tipo de Pelota
ggplot(medias, aes(x = `Material de Superficie`, y = media_rebote, fill = factor(`Altura de Caída (cm)`))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ `Tipo de Pelota`) +
  labs(x = "Material de Superficie", y = "Media de Altura de Rebote (cm)",
       title = "Media de Altura de Rebote por Material de Superficie, Altura de Caída y Tipo de Pelota",
       fill = "Altura de Caída (cm)") +
  theme_minimal()

#Media de Altura de Rebote por Material de Superficie y Altura de Caída
#Esta gráfica es un diagrama de barras apiladas que muestra la media de la altura de rebote para diferentes combinaciones de material de superficie y altura de caída.
#En general, la altura de rebote es mayor en superficies de madera (1) y cemento (-1) comparado con el pasto sintético (0).
#La altura de rebote es mayor cuando la altura de caída es de 50 cm (1), especialmente notable en la superficie de madera.
#Para todas las superficies, las alturas de rebote parecen seguir un patrón consistente en función de la altura de caída, con 50 cm siendo la mayor, seguida por 150 cm y luego 100 cm.

# Gráfico de cajas para Material de Superficie
ggplot(datos_limpios, aes(x = factor(`Material de Superficie`), y = `Altura de Rebote (cm)`, fill = factor(`Material de Superficie`))) +
  geom_boxplot() +
  labs(x = "Material de Superficie", y = "Altura de Rebote (cm)", title = "Altura de Rebote por Material de Superficie") +
  theme_minimal()

# Gráfico de cajas para Altura de Caída
ggplot(datos_limpios, aes(x = factor(`Altura de Caída (cm)`), y = `Altura de Rebote (cm)`, fill = factor(`Altura de Caída (cm)`))) +
  geom_boxplot() +
  labs(x = "Altura de Caída (cm)", y = "Altura de Rebote (cm)", title = "Altura de Rebote por Altura de Caída") +
  theme_minimal()

# Gráfico de cajas para Tipo de Pelota
ggplot(datos_limpios, aes(x = factor(`Tipo de Pelota`), y = `Altura de Rebote (cm)`, fill = factor(`Tipo de Pelota`))) +
  geom_boxplot() +
  labs(x = "Tipo de Pelota", y = "Altura de Rebote (cm)", title = "Altura de Rebote por Tipo de Pelota") +
  theme_minimal()
#- *Observaciones*:
#Cemento (-1)*: Tiene una mediana alrededor de 75 cm y un rango intercuartílico que indica una variabilidad moderada en las alturas de rebote.
#Pasto Sintético (0)*: Tiene la mediana más baja, alrededor de 50 cm, con un rango más estrecho comparado con otras superficies.
#Madera (1)*: La mediana es alrededor de 75 cm, similar al cemento, pero con una mayor variabilidad en los datos.
#Espuma (-1)*: Tiene la mediana más baja (aproximadamente 50 cm) y un rango intercuartílico más estrecho, indicando menor variabilidad en la altura de rebote.
#Plástico (0)*: Presenta una mediana intermedia (alrededor de 60 cm) con un rango moderado.
#Goma (1)*: Tiene la mediana más alta (aproximadamente 75 cm) y muestra la mayor variabilidad en la altura de rebote, lo que sugiere que este tipo de pelota rebota más y de manera más inconsistente.

# Gráfico de interacción entre Material de Superficie y Altura de Caída, para cada Tipo de Pelota
interaction.plot(
  x.factor = datos_limpios$`Material de Superficie`,
  trace.factor = datos_limpios$`Altura de Caída (cm)`,
  response = datos_limpios$`Altura de Rebote (cm)`,
  type = "b",  # Tipo de gráfico (b para líneas con puntos)
  fixed = TRUE,  # Fijar el orden de los factores (Material de Superficie y Altura de Caída)
  xlab = "Material de Superficie",
  ylab = "Altura de Rebote (cm)",
  main = "Interacción: Material de Superficie y Altura de Caída, por Tipo de Pelota"
)

# Gráfico de interacción entre Material de Superficie y Tipo de Pelota, para cada Altura de Caída
interaction.plot(
  x.factor = datos_limpios$`Material de Superficie`,
  trace.factor = datos_limpios$`Tipo de Pelota`,
  response = datos_limpios$`Altura de Rebote (cm)`,
  type = "b",  # Tipo de gráfico (b para líneas con puntos)
  fixed = TRUE,  # Fijar el orden de los factores (Material de Superficie y Tipo de Pelota)
  xlab = "Material de Superficie",
  ylab = "Altura de Rebote (cm)",
  main = "Interacción: Material de Superficie y Tipo de Pelota, por Altura de Caída"
)

# Gráfico de interacción entre Altura de Caída y Tipo de Pelota, para cada Material de Superficie
interaction.plot(
  x.factor = datos_limpios$`Altura de Caída (cm)`,
  trace.factor = datos_limpios$`Tipo de Pelota`,
  response = datos_limpios$`Altura de Rebote (cm)`,
  type = "b",  # Tipo de gráfico (b para líneas con puntos)
  fixed = TRUE,  # Fijar el orden de los factores (Altura de Caída y Tipo de Pelota)
  xlab = "Altura de Caída (cm)",
  ylab = "Altura de Rebote (cm)",
  main = "Interacción: Altura de Caída y Tipo de Pelota, por Material de Superficie"
)

# Ajuste del modelo de regresión lineal
modelo_regresion <- lm(`Altura de Rebote (cm)` ~ `Material de Superficie` * `Altura de Caída (cm)` * `Tipo de Pelota`, data = datos_limpios)

# Obtener coeficientes del modelo de regresión
coeficientes <- coef(modelo_regresion)
print(coeficientes)

# Resumen del ANOVA
anova_resultados <- summary(modelo_regresion)
print(anova_resultados)
 
#Altura de Caída y Tipo de Pelota: Son variables significativas en este modelo y tienen un impacto claro en la altura de rebote. A medida que aumenta la altura de caída, la altura de rebote tiende a disminuir, mientras que el tipo de pelota utilizado afecta positivamente la altura de rebote.
#Material de Superficie: No muestra un efecto significativo en la altura de rebote en este modelo particular. Esto sugiere que, en el contexto de las otras variables controladas, el tipo de superficie no influye de manera significativa en la altura de rebote.
#Interacciones: Las interacciones entre las variables no son significativas, lo que indica que las combinaciones específicas de las variables independientes no tienen un efecto adicional o diferente en la altura de rebote que el que se esperaría de manera independiente.

# Obtener los residuos estandarizados del modelo
residuos <- rstandard(modelo_regresion)

# Gráfico de residuos vs valores ajustados
ggplot(data.frame(residuos = residuos, valores_ajustados = fitted(modelo_regresion)), 
       aes(x = valores_ajustados, y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Valores Ajustados", y = "Residuos Estandarizados",
       title = "Gráfico de Residuos vs Valores Ajustados")
#El gráfico no muestra patrones evidentes que indicarían problemas graves como no linealidad, heterocedasticidad, o falta de independencia de los residuos. Sin embargo, se observan algunos posibles outliers que podrían requerir una evaluación adicional. En general, 
#el gráfico sugiere que el modelo de regresión se ajusta adecuadamente a los datos.
# Gráfico Q-Q de los residuos
qqnorm(residuos, main = "Gráfico Q-Q de los Residuos")
qqline(residuos)

# Gráfico de residuos vs predictoras
par(mfrow = c(2, 2))  # Dividir la ventana gráfica en 2x2
plot(modelo_regresion)

#*Linealidad*: Hay indicios de no linealidad en el gráfico de residuos vs valores ajustados.
#*Normalidad de Residuos*: Los residuos en su mayoría siguen una distribución normal, aunque hay algunos outliers.
#*Heterocedasticidad*: Puede haber alguna heterocedasticidad, como se sugiere en el gráfico de escala-localización.
#*Puntos Influyentes*: Hay varios puntos influyentes que podrían estar afectando el modelo de manera significativa.

# Test de Shapiro-Wilk para la normalidad de los residuos
shapiro_test <- shapiro.test(residuos)
print(shapiro_test)
#Basado en el valor p obtenido, rechazamos la hipótesis nula de normalidad. Esto sugiere que los residuos del modelo no siguen una distribución normal, 
#lo cual es importante para interpretar correctamente las pruebas estadísticas y las conclusiones derivadas del modelo de regresión.



#Prueba de Kruskal-Wallis
kruskal.test(`Altura de Rebote (cm)` ~ `Material de Superficie`, data = datos_limpios)
#El valor p es 0.04384, que es menor que el umbral común de 0.05. Esto sugiere que hay diferencias 
#significativas en las distribuciones de las alturas de rebote entre al menos dos de los grupos de "Material de Superficie". En otras palabras, la mediana de la altura de rebote no es la misma para todos los materiales de superficie.



# Realizar la prueba de Dunn
dunn_test <- dunn.test(datos_limpios$`Altura de Rebote (cm)`, datos_limpios$`Material de Superficie`, method = "bonferroni")

# Mostrar los resultados
print(dunn_test)

#La diferencia en las alturas de rebote entre las superficies codificadas como -1 (cemento) y 0 (pasto sintético) es estadísticamente significativa después de ajustar por múltiples comparaciones (p = 0.0187).
#No se encontraron diferencias significativas entre las comparaciones -1 vs 1 y 0 vs 1 después de la corrección de Bonferroni (p > 0.05).


# Convertir Material de Superficie y Tipo de Pelota a factor
datos_limpios$`Material de Superficie` <- as.factor(datos_limpios$`Material de Superficie`)
datos_limpios$`Tipo de Pelota` <- as.factor(datos_limpios$`Tipo de Pelota`)

# Test de Levene para la igualdad de varianzas
levene_test <- leveneTest(`Altura de Rebote (cm)` ~ `Material de Superficie` * `Altura de Caída (cm)` * `Tipo de Pelota`, data = datos_limpios)
print(levene_test)
#Valor p (Pr(>F)): Es el resultado clave aquí. Un valor p alto (en este caso 1) sugiere que no hay suficiente evidencia para rechazar 
#la hipótesis nula de igualdad de varianzas. Esto significa que no hay diferencias significativas en las varianzas de la altura de rebote entre los diferentes grupos definidos por tus factores.

modelo_regresion <- lm(`Altura de Rebote (cm)` ~ `Material de Superficie` * `Altura de Caída (cm)` * `Tipo de Pelota`, data = datos_limpios)


material_surface <- unique(datos_limpios$`Material de Superficie`)
altura_caida <- unique(datos_limpios$`Altura de Caída (cm)`)
tipo_pelota <- unique(datos_limpios$`Tipo de Pelota`)
mesh_data <- expand.grid(`Material de Superficie` = material_surface, 
                         `Altura de Caída (cm)` = altura_caida, 
                         `Tipo de Pelota` = tipo_pelota)


mesh_data$Altura_Rebote <- predict(modelo_regresion, newdata = mesh_data)

library(plotly)

plot_ly(mesh_data, 
        x = ~`Material de Superficie`, 
        y = ~`Altura de Caída (cm)`, 
        z = ~Altura_Rebote, 
        type = "surface") %>%
  layout(scene = list(
    xaxis = list(title = "Material de Superficie"),
    yaxis = list(title = "Altura de Caída (cm)"),
    zaxis = list(title = "Altura de Rebote (cm)")
  )) %>%
  config(displayModeBar = FALSE)  # Para ocultar la barra de modo de visualización

head(mesh_data)

install.packages("plotly")
library(plotly)
install.packages("pwr")
library(pwr)

# Evaluación del número de réplicas usadas en el experimento

# Cálculos de potencia para cada factor (Material de Superficie, Altura de Caída, Tipo de Pelota)
potencia_material <- pwr.anova.test(
  k = length(unique(datos_limpios$`Material de Superficie`)),
  n = NULL,  # Esto se calculará
  f = 0.142,  # Tamaño del efecto pequeño a moderado
  sig.level = 0.05,
  power = 0.9  # Potencia deseada del 90%
)

n_material <- ceiling(potencia_material$n)

potencia_altura <- pwr.anova.test(
  k = length(unique(datos_limpios$`Altura de Caída (cm)`)),
  n = NULL,  # Esto se calculará
  f = 0.42,  # Tamaño del efecto pequeño a moderado
  sig.level = 0.05,
  power = 0.9  # Potencia deseada del 90%
)

n_altura <- ceiling(potencia_altura$n)

potencia_pelota <- pwr.anova.test(
  k = length(unique(datos_limpios$`Tipo de Pelota`)),
  n = NULL,  # Esto se calculará
  f = 0.182,  # Tamaño del efecto pequeño a moderado
  sig.level = 0.05,
  power = 0.9  # Potencia deseada del 90%
)

n_pelota <- ceiling(potencia_pelota$n)

# Mostrar resultados
potencia_material
cat("Número de réplicas necesarias para Material de Superficie:", n_material, "\n")

potencia_altura
cat("Número de réplicas necesarias para Altura de Caída:", n_altura, "\n")

potencia_pelota
cat("Número de réplicas necesarias para Tipo de Pelota:", n_pelota, "\n")

# Calcular la potencia para cada factor

potencia_material <- pwr.anova.test(
  k = length(unique(datos_limpios$`Material de Superficie`)),
  n = 3,  
  f = 0.142,  # Tamaño del efecto pequeño a moderado
  sig.level = 0.05  # Nivel de significancia
)


# Potencia para Altura de Caída
potencia_altura <- pwr.anova.test(
  k = length(unique(datos_limpios$`Altura de Caída (cm)`)),
  n = 3,  
  f = 0.42,  
  sig.level = 0.05
)

# Potencia para Tipo de Pelota
potencia_pelota <- pwr.anova.test(
  k = length(unique(datos_limpios$`Tipo de Pelota`)),
  n = 3,  
  f = 0.182,  
  sig.level = 0.05
)

# Mostrar resultados
cat("Potencia alcanzada para cada factor:\n")
cat("Material de Superficie:", potencia_material$power, "\n")
cat("Altura de Caída:", potencia_altura$power, "\n")
cat("Tipo de Pelota:", potencia_pelota$power, "\n")

#Material de Superficie: Se calculó que se necesitan aproximadamente 211 réplicas por grupo para detectar un efecto de tamaño moderado (f = 0.142) con una potencia del 90% y un nivel de significancia del 5%.
#Altura de Caída: Se determinó que se requieren alrededor de 25 réplicas por grupo para detectar un efecto de tamaño moderado (f = 0.42) con una potencia del 90% y un nivel de significancia del 5%.
#Tipo de Pelota: Se estimó que se necesitan aproximadamente 129 réplicas por grupo para detectar un efecto de tamaño moderado (f = 0.182) con una potencia del 90% y un nivel de significancia del 5%.

#terial de Superficie: La potencia alcanzada para detectar diferencias significativas entre los grupos de materiales de superficie es de aproximadamente 0.0587. Esto significa que hay una baja probabilidad de detectar un efecto real si existe un efecto de tamaño pequeño a moderado (f = 0.142).

#Altura de Caída: La potencia alcanzada para detectar diferencias significativas entre los grupos de alturas de caída es de aproximadamente 0.1316. Esta potencia también es relativamente baja, indicando que hay una probabilidad moderada de detectar un efecto real si existe un tamaño de efecto de 0.42.

#Tipo de Pelota: La potencia alcanzada para detectar diferencias significativas entre los tipos de pelota es de aproximadamente 0.0644. Esta potencia es similarmente baja, lo que sugiere que hay una baja probabilidad de detectar un efecto real si existe un tamaño de efecto de 0.182.

