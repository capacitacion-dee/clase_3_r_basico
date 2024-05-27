
library(dplyr)
install.packages("tidyr")
library(tidyr)

# Estructuración de tablas ------------------------------------------------

## Función pivot_longer() ----

# Formato wide

ventas_wide <- data.frame(producto = c("A", "B", "C"),
                          `2018` = c(100, 150, 200),
                          `2019` = c(120, 180, 220),
                          `2020` = c(130, 160, 210),
                          check.names=FALSE)
ventas_wide

# Formato long

# Función pivot_longer(): es una función del paquete "tidyr" utilizada para pivotear un dataframe de formato ancho a uno de formato largo. 
# Esta función incrementa el número de filas y disminuye el número de columnas.
# - Sus argumentos son:
# data: el dataframe a ser modificado.
# cols: el nombre de las columnas que se van a pivotear.
# names_to: el nombre de la nueva columna que contendrá los nombres de las categorías.
# values_to: el nombre de la nueva columna que contendrá los valores correspondientes a cada categoría.

ventas_long <- ventas_wide %>% 
  pivot_longer(cols = -producto, names_to = "anio", values_to = "ventas")

ventas_long

# Nota: Queremos transformar todas las columnas menos producto, por eso usamos el signo "-"

## Función pivor_wider() ----

# Función pivot_wider(): es una función del paquete "tidyr" utilizada para pivotear un dataframe de formato largo a uno de formato ancho. 
# Esta función disminuye el número de filas y aumenta el número de columnas.
# - Sus argumentos son:
# data: el dataframe a ser modificado.
# names_from: qué columnas se extenderán y cuyos valores se convertirán en nombres de nuevas columnas.
# values_from: qué columnas se utilizarán para llenar los valores en las nuevas columnas creadas por la extensión de la variable especificada en names_from.

ventas_wide <- ventas_long %>% 
  pivot_wider(names_from = anio, values_from = ventas, names_prefix = "anio_")

ventas_wide

# Ejercicio 1 -------------------------------------------------------------
# 1. Cargue la librería "datos" y almacene el dataframe "encuesta" como "encuesta".

# 2. Agrupe el dataframe encuesta por las variables "anio" e "ingreso" y, usando un summarise(), calcule las horas_tv promedio usando la función mean().
# Llame a esta variable "horas_prom".
# Almacene el dataframe resultante como "encuesta_agrupado".
# Nota: recuerde usar el argumento na.rm = TRUE para que no considere los valores NA.


# 3. Utilizando la función pivot_wider(), convierta el dataframe "encuesta_agrupado" a formato ancho. 
# El nombre de las columnas debe ser el ingreso y los valores las horas de tv promedio. 
# Almacene el dataframe resultante como "encuesta_wide"


# 4. Transforme el dataframe "encuesta_wide" a su estructura original, es decir, en formato long, usando pivot_longer(). 
# El dataframe resultante debe ser igual a "encuesta_agrupado". Almacene este dataframe como "encuesta_long".







# Unión de tablas ---------------------------------------------------------

## left_join() ----

# Caso 1: existe una variable llave o identificadora que identifica de forma única cada observación.
# En ambos dataframes esta variable se llama igual (por ej: "name")

band_members

band_instruments

band_members %>% left_join(band_instruments, by = "name")

# Caso 2: existe una variable llave o identificadora que identifica de forma única cada observación.
# Esta variable tiene distintos nombres en cada dataframe (por ej: "name" y "artist")

band_members

band_instruments2

band_members %>% left_join(band_instruments2, by = c("name"="artist"))

# Caso 3: existe una combinación de variables llaves o identificadores que identifican de forma única cada observación

# Conjunto de datos de ventas
ventas <- data.frame(vendedor = c("Juan", "María", "Juan", "Pedro", "María", "Pedro"),
                     departamento = c("Marketing", "Marketing", "Ventas", "Ventas", "Marketing", "Ventas"),
                     ventas = c(100, 150, 200, 120, 180, 160))
ventas
# Conjunto de datos de vendedores
vendedores <- data.frame(vendedor = c("Juan", "María", "Pedro", "Juan"),
                         departamento = c("Ventas", "Marketing", "Ventas", "Marketing"),
                         experiencia = c(5, 7, 3, 2))
vendedores

# En este ejemplo podemos ver que el nombre "Juan" se repite más de una vez en el dataframe "vendedores", es decir, esta variable no
# identifica de forma única a cada vendedor.
# En este caso usamos la combinación "vendedor" y "departamento" para hacer la unión, ya que esta combinación sí identifica de forma única a
# "Juan" de "Ventas" y a "Juan" de "Marketing".

ventas %>% left_join(vendedores, by = c("vendedor","departamento"))

## right_join() ----

band_members

band_instruments

band_members %>% right_join(band_instruments, by = "name")

## full_join() ----

band_members

band_instruments

band_members %>% full_join(band_instruments, by = "name")

## anti_join() ----

band_members

band_instruments

band_members %>% anti_join(band_instruments, by = "name")

# Ejercicio 2: ------------------------------------------------------------

# 1. Instale y cargue el paquete "nycflights13". 
# 2. Sobrescriba el dataframe "flights" seleccionando las columnas "time_hour", "origin", "dest", "tailnum" y "carrier".
# 3. Haga un left_join entre "flights" y "airlines" usando la llave correspondiente.
# 4. Haga un left_join entre "flights" y "planes" usando la llave correspondiente.
# 5. Haga un left_join entre "flights" y "weather" usando las llaves correspondientes.
# 6. Haga un anti_join entre "flights" y "planes" usando la llave correspondiente.





# Visualización de datos con ggplot ---------------------------------------

## Gráfico de barras ----

# Instalar y cargar el paquete gapminder
install.packages("gapminder")
library(gapminder)

summary(gapminder)

# Instalar y cargar el paquete ggplot2
install.packages("ggplot2")
library(ggplot2)

# Gráfico de barras simple que cuenta el número de observaciones
gapminder %>% 
  filter(year==2007) %>% 
  ggplot() +
  aes(x = continent) +
  geom_bar()

# Seteando la longitud de las barras usando "identity"
gapminder %>% 
  filter(year==2007) %>%
  group_by(continent) %>% 
  summarise(gdpPercap_prom = mean(gdpPercap)) %>% 
  ggplot() +
  aes(x = continent, y = gdpPercap_prom) +
  geom_bar(stat = "identity")

# Agregar más elementos como etiquetas, títulos, color
gapminder %>% 
  filter(year==2007) %>% 
  group_by(continent) %>% 
  summarise(gdpPercap_prom = mean(gdpPercap)) %>% 
  mutate(gdpPercap_prom = round(gdpPercap_prom, 0)) %>% 
  ggplot() +
  aes(x = continent, y = gdpPercap_prom, label = gdpPercap_prom) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Continente", y = "PIB per cápita promedio", title = "PIB per cápita promedio por continente (2017)") +
  geom_label()

# Colocar barras una al lado de la otra
gapminder %>% 
  filter(year==1997 | year==2007) %>% 
  filter(country %in% c("Chile", "Argentina", "Bolivia", "Brazil", "Colombia")) %>% 
  mutate(pop = pop/1000,
         year = as.factor(year)) %>% 
  ggplot() +
  aes(x = country, y = pop, fill = year) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "País", y = "Población (en miles)")

## Gráfico de líneas ----
gapminder %>%
  group_by(continent, year) %>%
  summarise(lifeExp_prom = mean(lifeExp)) %>%
  ggplot(aes(x=year, y=lifeExp_prom, color=continent)) +
  geom_line(size = 1)

## Gráfico de puntos o scatterplot ----
gapminder %>% 
  ggplot() +
  aes(x = log(gdpPercap), y = lifeExp) +
  geom_point()

# Usando el parámetro color en aes()
gapminder %>% 
  ggplot() +
  aes(x = log(gdpPercap), y = lifeExp, color=continent) +
  geom_point()

# Usando facet_wrap()
gapminder %>% 
  ggplot() +
  aes(x = log(gdpPercap), y = lifeExp, color=continent) +
  geom_point() +
  facet_wrap(~continent)


# Ejercicio 3 -------------------------------------------------------------

# 1. Genere un gráfico de barras simple que cuente el número de veces que aparece cada aerolínea usando el dataframe "vuelos".
# 2. Para generar el siguiente gráfico de barras, agrupe el dataframe por "aerolínea" y usando la función summarise() calcule la distancia promedio, 
#    llamando a esta variable "dist_prom". Utilice ggplot para generar un gráfico de barras donde el largo del eje y sea la distancia promedio de 
#    cada aerolínea.
# 3. Genere un gráfico de líneas agrupando el dataframe por "mes" y "origen", y usando la función summarise() calcule el tiempo de vuelo promedio, 
#    llamando a esta variable "tiempo_prom". Utilice ggplot para generar un gráfico de líneas donde el eje x sea el "mes" y el eje y sea "tiempo_prom". 
#    Agregue el parámetro color = origen dentro de aes() para que cada línea tenga su color.
#    Nota: utilice el argumento na.rm = TRUE dentro de la función mean() para que no tome en cuenta los valores NA.
# 4. Filtre las filas del dataframe "vuelos" donde destino sea igual a "ATL" y replique el siguiente gráfico:

library(datos)
vuelos






