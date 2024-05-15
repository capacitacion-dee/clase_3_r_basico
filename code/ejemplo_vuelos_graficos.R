
library(readxl)
vuelos <- read_excel("data/vuelos.xlsx")

ggplot(vuelos, aes(x = aerolinea))+
  geom_bar()

vuelos_agrupado <- vuelos %>% 
  group_by(aerolinea) %>% 
  summarise(dist_prom = mean(distancia, na.rm = TRUE))

vuelos_agrupado

ggplot(vuelos_agrupado, aes(x = aerolinea, y = dist_prom)) +
  geom_bar(stat = "identity")

vuelos %>% 
  group_by(mes, origen) %>% 
  summarise(tiempo_prom = mean(tiempo_vuelo, na.rm = TRUE)) %>% 
  ggplot() +
  aes(x = mes, y = tiempo_prom, color = origen)+
  geom_line()


vuelos %>% 
  group_by(origen, destino) %>% 
  summarise(dist_prom = mean(distancia, na.rm = TRUE)) %>% 
  arrange(-dist_prom) %>% 
  slice_head(n = 3) %>% 
  ggplot() +
  aes(x = origen, y = dist_prom, fill = destino) +
  geom_bar(stat = "identity", position = "dodge")


vuelos %>% 
  group_by(origen, destino) %>% 
  summarise(dist_prom = n()) %>% 
  arrange(-dist_prom) %>% 
  slice_head(n = 3) %>% 
  ggplot() +
  aes(x = origen, y = dist_prom, fill = destino) +
  geom_bar(stat = "identity", position = "dodge")


vuelos %>% 
  filter(destino == "ATL") %>% 
  ggplot() +
  aes(x = atraso_salida, y = atraso_llegada) +
  geom_point()


vuelos %>% 
  filter(destino == "ATL") %>% 
  ggplot() +
  aes(x = atraso_salida, y = atraso_llegada) +
  geom_point()


vuelos %>% 
  filter(destino == "ATL") %>% 
  ggplot() +
  aes(x = atraso_salida, y = atraso_llegada, color = origen) +
  geom_point()+
  facet_wrap(~origen)

