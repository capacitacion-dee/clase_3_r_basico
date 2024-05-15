
emat <- read_excel("data/enero-2017-a-octubre-2018.xlsx")

head(emat)
colnames(emat)

emat <- emat %>% 
  rename(mes = Mes,
         anio = Año,
         clase = Clase,
         region = Región,
         destino_turistico = `Destino Turístico`,
         total_llegadas = `Total Llegadas`,
         total_pernoctacion = `Total Pernoctación`,
         factor_expansion = `Factor de Expansión`) %>% 
  select(mes, anio, clase, region, total_llegadas, total_pernoctacion, factor_expansion)

emat <- emat %>% 
  mutate(total_llegadas_exp = total_llegadas*factor_expansion,
         total_pernoctaciones_exp = total_pernoctacion*factor_expansion)

emat <- emat %>% 
  mutate(fecha = paste(anio, mes, 1, sep = "-"),
         fecha = ymd(fecha))

emat <- emat %>% 
  select(mes, anio, fecha, clase, region, total_llegadas_exp, total_pernoctaciones_exp)

writexl::write_xlsx(emat, "data/emat.xlsx")

emat %>% 
  filter(anio == 2018) %>% 
  ggplot() +
  aes(y = region) +
  geom_bar()
  
emat %>% 
  filter(anio == 2018) %>% 
  group_by(region) %>% 
  summarise(suma_llegadas = sum(total_llegadas_exp)) %>% 
  ggplot() +
  aes(x = suma_llegadas, y = region) +
  geom_bar(stat = "identity")


emat %>% 
  mutate(anio = as.factor(anio)) %>% 
  group_by(region, anio) %>% 
  summarise(suma_llegadas = sum(total_llegadas_exp)) %>% 
  ggplot() +
  aes(x = suma_llegadas, y = region, fill = anio) +
  geom_bar(stat = "identity", position = "dodge")

emat %>% 
  mutate(anio = as.factor(anio)) %>% 
  filter(!is.na(clase)) %>% 
  group_by(clase, anio) %>% 
  summarise(suma_llegadas = sum(total_llegadas_exp)) %>% 
  ggplot() +
  aes(x = clase, y = suma_llegadas, fill = anio) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Clase", y = "Total llegadas", title = "Total llegadas por clase (2018)")


emat %>% 
  filter(anio == 2017) %>% 
  ggplot() +
  aes(x = total_pernoctaciones_exp, y = total_llegadas_exp) +
  geom_point()

emat %>% 
  filter(anio == 2017) %>% 
  ggplot() +
  aes(x = total_pernoctaciones_exp, y = total_llegadas_exp, color = clase) +
  geom_point()

emat %>% 
  filter(anio == 2018 & !is.na(clase)) %>% 
  group_by(mes, clase) %>%
  summarise(suma_pernoctaciones = sum(total_pernoctaciones_exp)) %>% 
  ggplot() +
  aes(x = mes, y = suma_pernoctaciones, color=clase) +
  geom_line()

emat %>% 
  filter(!is.na(clase)) %>% 
  group_by(fecha, clase) %>% 
  summarise(suma_pernoctaciones = sum(total_pernoctaciones_exp)) %>% 
  ggplot() +
  aes(x = fecha, y = suma_pernoctaciones, color = clase) +
  geom_line()


pernoctaciones_wide <- emat %>% 
  group_by(anio, region) %>% 
  summarise(suma_pernoctaciones = sum(total_pernoctaciones_exp, na.rm = TRUE)) %>% 
  pivot_wider(names_from = anio, values_from = suma_pernoctaciones, names_prefix = "pernoctaciones_")

llegadas_wide <- emat %>% 
  group_by(anio, region) %>% 
  summarise(suma_llegadas = sum(total_llegadas_exp, na.rm = TRUE)) %>% 
  pivot_wider(names_from = anio, values_from = suma_llegadas, names_prefix = "llegadas_")

emat %>% 
  group_by(anio, region) %>% 
  summarise(suma_llegadas = sum(total_llegadas_exp),
            suma_pernoctaciones = sum(total_pernoctaciones_exp)) %>% 
  pivot_wider(names_from = anio, values_from = c(suma_llegadas, suma_pernoctaciones), names_prefix = c("llegadas_", "pernoctaciones_"))

pernoctaciones_join <- pernoctaciones_wide %>% 
  left_join(llegadas_wide, by = c("region"))

llegadas_wide %>% 
  pivot_longer(cols = -region, names_to = "anio", values_to = "llegadas")
