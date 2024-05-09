

library(readxl)
library(dplyr)

vuelos <- read_xlsx("data/vuelos.xlsx")

vuelos2 <- vuelos %>% 
  group_by(origen, destino) %>% 
  summarise(suma = sum(tiempo_vuelo, na.rm = TRUE)) %>% 
  pivot_wider(names_from = destino, values_from = suma)
