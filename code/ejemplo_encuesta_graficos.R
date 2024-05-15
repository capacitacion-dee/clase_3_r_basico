

encuesta %>% 
  ggplot() +
  aes(x = estado_civil) +
  geom_bar()


encuesta %>%
  group_by(estado_civil) %>% 
  summarise(horas_prom = mean(horas_tv, na.rm = TRUE)) %>% 
  ggplot() +
  aes(x = estado_civil, y = horas_prom) +
  geom_bar(stat = "identity")

encuesta %>% 
  group_by(anio) %>% 
  summarise(horas_prom = mean(horas_tv, na.rm = TRUE)) %>% 
  ggplot() +
  aes(x = anio, y = horas_prom)+
  geom_line()


encuesta %>% 
  group_by(anio, raza) %>% 
  summarise(horas_prom = mean(horas_tv, na.rm = TRUE)) %>% 
  ggplot() +
  aes(x = anio, y = horas_prom, fill = raza, color = raza)+
  geom_line()
