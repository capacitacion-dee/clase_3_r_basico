
flights2 <- flights |> 
  select(time_hour, origin, dest, tailnum, carrier)
flights2

flights2 |>
  left_join(airlines, by = c("carrier"))

flights2 %>% 
  left_join(weather, by = c("time_hour", "origin"))

flights2 |> 
  left_join(planes, by = c("tailnum"))

flights2 %>% 
  anti_join(airports, by = c("dest"="faa"))

flights2 |>
  anti_join(planes, by = c("tailnum")) |> 
  distinct(tailnum)
