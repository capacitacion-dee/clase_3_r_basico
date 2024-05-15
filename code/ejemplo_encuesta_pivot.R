

library(readxl)
library(tidyr)
library(dplyr)

encuesta <- read_excel("data/encuesta.xlsx")

encuesta_agrupado <- encuesta %>% 
  group_by(anio, ingreso) %>% 
  summarise(horas_prom = mean(horas_tv, na.rm = TRUE)) 

encuesta_wide <- encuesta_agrupado %>% 
  pivot_wider(names_from = ingreso, values_from = horas_prom)

encuesta_long <- encuesta_wide %>% 
  pivot_longer(cols = -anio, names_to = "ingreso", values_to = "horas_prom")
