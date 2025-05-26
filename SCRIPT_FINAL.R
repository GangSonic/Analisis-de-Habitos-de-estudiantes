# -------------------------------------------------
#SEMANA 1 
# -------------------------------------------------
# Cargar paquetes necesarios o install.packages() si no los tienes instalados
library(dplyr)
library(tidyr)
library(ggplot2)
library(gapminder)

# Cargamos los datos
data <- as_tibble(gapminder)

# EJERCICIOS

# 1. Filtrar los datos solo para el año 2007
data_2007 <- data %>% filter(year == 2007)

# 2. Seleccionar las columnas país, continente y esperanza de vida
data_selected <- data %>% select(country, continent, lifeExp)

# 3. Renombrar la columna lifeExp a esperanza_vida
data_renamed <- data %>% rename(esperanza_vida = lifeExp)

# 4. Ordenar los datos por PIB per cápita de mayor a menor
data_sorted <- data %>% arrange(desc(gdpPercap))

# 5. Agregar una nueva columna con el PIB total (pop * gdpPercap)
data_mutated <- data %>% mutate(PIB_total = pop * gdpPercap)

# 6. Reubicar la columna PIB_total al inicio
data_relocated <- data_mutated %>% relocate(PIB_total)

# 7. Filtrar países con una esperanza de vida mayor a 70 años
data_lifeExp70 <- data %>% filter(lifeExp > 70)

# 8. Contar cuántos países hay por continente
data_continent_count <- data %>% count(continent)

# 9. Agrupar por continente y calcular la media de esperanza de vida
data_grouped <- data %>% group_by(continent) %>% summarise(media_vida = mean(lifeExp))

# 10. Encontrar el país con la mayor población en 2007
data_max_pop <- data %>% filter(year == 2007) %>% arrange(desc(pop)) %>% slice(1)

# 11. Convertir los datos de formato ancho a largo
data_long <- data %>% pivot_longer(cols = c(lifeExp, pop, gdpPercap), names_to = "indicador", values_to = "valor")

# 12. Convertir los datos largos a formato ancho
data_wide <- data_long %>% pivot_wider(names_from = "indicador", values_from = "valor")

# 13. Separar la columna país en dos (si tuviera un delimitador "-")
data_separated <- data %>% separate(country, into = c("parte1", "parte2"), sep = "-", extra = "merge", fill = "right")

# 14. Unir dos columnas en una sola (ejemplo con país y continente)
data_united <- data %>% unite("pais_continente", country, continent, sep = " - ")

# 15. Reemplazar valores NA en una columna con la media
data_na_handled <- data %>% mutate(lifeExp = replace_na(lifeExp, mean(lifeExp, na.rm = TRUE)))

# 16. Crear un histograma de la esperanza de vida
ggplot(data, aes(x = lifeExp)) + geom_histogram(binwidth = 5, fill = "blue", alpha = 0.5)

# 17. Crear un diagrama de dispersión de PIB per cápita vs. esperanza de vida
ggplot(data, aes(x = gdpPercap, y = lifeExp)) + geom_point(alpha = 0.5) + scale_x_log10()

# 18. Crear un boxplot de esperanza de vida por continente
ggplot(data, aes(x = continent, y = lifeExp)) + geom_boxplot()

# 19. Graficar la tendencia de la esperanza de vida en un país específico a lo largo del tiempo
data %>% filter(country == "Argentina") %>% ggplot(aes(x = year, y = lifeExp)) + geom_line() + geom_point()

# 20. Facetado por continente para visualizar la relación entre PIB per cápita y esperanza de vida
ggplot(data, aes(x = gdpPercap, y = lifeExp)) + geom_point() + facet_wrap(~continent) + scale_x_log10()


# -------------------------------------------------
#SEMANA 2
# -------------------------------------------------


##FUENTE 
##Fuente:https://www.kaggle.com/datasets/aryan208/student-habits-and-academic-performance-dataset

##OBJETIVO 
##En la vida estudiantil padecemos de diversos problemas que nos llevan a mejorar o bajar nuestro
##rendimiento académico, por lo que con este framework de dataset se planteó identificar qué
##hábitos o condiciones podrían mejorarse para ayudar a la población estudiantil.
## PROBLEMA: (PREGUNTA) 
##  ¿Cuáles son los factores que más influyen en el rendimiento académico de los estudiantes, medido por puntaje de exámenes?

##JUSTIFICACION 
# Este análisis tiene como objetivo identificar los hábitos, condiciones psicológicas y factores
# socioeconómicos que más influyen en el desempeño académico de los estudiantes.
# Se busca que los hallazgos contribuyan a mejorar planes de estudio o estrategias de apoyo
# en instituciones educativas.

# El dataset utilizado es adecuado para responder esta pregunta, ya que contiene variables clave como:
# - Hora de estudio y sueño
# - Niveles de estrés y ansiedad
# - Participación en actividades extracurriculares
# - Apoyo familiar
# - Nivel socioeconómico
# - Indicadores de rendimiento académico (como calificaciones de exámenes)

# El dataset tiene 80,000 registros y 20 variables de distintos tipos, lo cual es manejable en R.


###VISUALIZACION DE DATASET
data <- read.csv("estudiantes_habitos.csv")
head(data)
str(data)



# -------------------------------------------------
#SEMANA 3
# -------------------------------------------------

library(dplyr)
library(ggplot2)
glimpse(data)  # Vista rápida de la estructura

###LIMPIEZA 

#Sumariy() 
summary(data) #Resumen estadistico 

#Eliminar NA 
### Verificar si hay NA en el dataset 
colSums(is.na(data))
sum(is.na(data))


#Convertir variables a formato numerico 
str(data)

##Cambiar la edad a numerico 
data$age <- as.numeric(data$age)

#Calcular medidas de tendecia central 
media <- mean(data$age, na.rm = TRUE)  # Media aritmética

mediana <- median(data$age, na.rm = TRUE)  # Mediana

media_recortada <- mean(data$age, trim = 0.1)  # Media recortada

desviacion_estandar <- sd(data$age, na.rm = TRUE)  # Desviación estándar

varianza <- var(data$age, na.rm = TRUE)  # Varianza

tabla_frecuencia <- table(cut(data$age, breaks = 10))  # Agrupar en intervalos

prop_frecuencia <- prop.table(tabla_frecuencia)  # Frecuencias relativas

####Visualizacion avanzada 
#Diagramas de dispersion 
ggplot(data, aes(x = study_hours_per_day, y = exam_score, color = gender)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(
    title = "Horas de estudio y calificaciones del examen",
    x = "Horas de estudio por dia",
    y = "Calificacion del examen"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

###La pregunta es, si hay una relacion con las horas estudiadas y la calificacion obtenida
###Sin embargo, podemos ver en el grafico una dispersion de datos, lo qu epodemos deducir que no hay una relacion 
###entre estas variables. Pero se ve como los datos tienden a subir levemente lo que no dice que
###entre mas estudien los estudiantes mejor les va. 

#Histogramas 

ggplot(data, aes(x = sleep_hours, fill = gender)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +
  labs(title = "Horas de sueño por género",
       x = "Horas de sueño",
       y = "Frecuencia") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

###Con este grafico se puede interpretar las horas de sueño que tienen los estudiantes respecto a su genero
###Podemo decir que la mayoria de todos los estudiantes duermen bien entre 6 y 8 horas 
###Y que si bien no son muchos hay que tener presente que hay estudiantes que se desvelan mucho y puede afectar
###Su rendimiento escolar. 



