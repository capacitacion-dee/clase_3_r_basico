
library(gapminder)
library(writexl)

str(gapminder)
glimpse(gapminder)
summary(gapminder)

writexl::write_xlsx(gapminder, "data/gapminder.xlsx")

gapminder %>% 
  filter(year==2007) %>% 
  ggplot() +
  aes(x = continent) +
  geom_bar()

gapminder %>% 
  filter(year==2007) %>% 
  ggplot() +
  aes(x = continent, fill = continent) +
  geom_bar()

gapminder %>% 
  group_by(continent) %>% 
  summarise(gdpPercap_prom = mean(gdpPercap)) %>% 
  ggplot() +
  aes(x = continent, y = gdpPercap_prom) +
  geom_bar(stat = "identity")

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

options(scipen = 999)

gapminder %>% 
  filter(year==1997 | year==2007) %>% 
  filter(country %in% c("Chile", "Argentina", "Bolivia", "Brazil", "Colombia")) %>% 
  ggplot() +
  aes(x = country, y = pop/1000, fill = as.factor(year)) +
  geom_bar(stat = "identity", position = "dodge2") +
  labs(x = "País", y = "Población (en miles)") +
  scale_fill_manual(values=c("#999999", "#56B4E9")) +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "top",
        legend.title = element_blank(),
        panel.background = element_rect(fill="#ffffff"),
        panel.grid.major.y = element_line(color = "gray60",
                                          linetype = "longdash"),
        axis.line = element_line(colour = "grey60"))

gapminder %>% 
  filter(year==1997 | year==2007) %>% 
  filter(country %in% c("Chile", "Argentina", "Bolivia", "Brazil", "Colombia")) %>% 
  mutate(pop = pop/1000,
         year = as.factor(year)) %>% 
  ggplot() +
  aes(x = country, y = pop, fill = year) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "País", y = "Población (en miles)")


gapminder %>% 
  filter(year==1997 | year==2007) %>% 
  filter(country %in% c("Chile", "Argentina", "Bolivia", "Brazil", "Colombia")) %>% 
  mutate(pop = pop/1000,
         year = as.factor(year)) %>% 
  ggplot() +
  aes(x = country, y = pop, fill = year, label = pop) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "País", y = "Población (en miles)") +
  geom_label(position = position_dodge(width = 1))


gapminder %>%
  group_by(continent, year) %>%
  summarise(lifeExp_prom = mean(lifeExp)) %>%
  ggplot(aes(x=year, y=lifeExp_prom, color=continent)) +
  geom_line(size=1) + 
  geom_point(size=1.5)

gapminder %>%
  group_by(continent, year) %>%
  summarise(lifeExp_prom = mean(lifeExp)) %>%
  ggplot(aes(x=year, y=lifeExp_prom, linetype=continent)) +
  geom_line(size=1)

gapminder %>%
  group_by(continent, year) %>%
  summarise(lifeExp_prom = mean(lifeExp)) %>%
  ggplot(aes(x=year, y=lifeExp_prom, size=continent)) +
  geom_line()

gapminder %>%
  group_by(continent, year) %>%
  summarise(lifeExp_prom = mean(lifeExp)) %>%
  ggplot(aes(x=year, y=lifeExp_prom, group = continent)) +
  geom_line(color = "red")


# let’s explore the relationship between life expectancy and GDP with a scatterplot

gapminder %>% 
  ggplot() +
  aes(x = log(gdpPercap), y = lifeExp) +
  geom_point()

gapminder %>% 
  filter(year==2007) %>% 
  ggplot() +
  aes(x = log(gdpPercap), y = lifeExp, color=continent) +
  geom_point()

gapminder %>% 
  filter(year==2007) %>% 
  ggplot() +
  aes(x = log(gdpPercap), y = lifeExp, color=continent, size = pop) +
  geom_point()

gapminder %>% 
  ggplot() +
  aes(x = log(gdpPercap), y = lifeExp, color=continent) +
  geom_point() +
  facet_wrap(~continent)


# DEnsidad
# There are several continuous variables in this data set: life expectancy (lifeExp), population (pop) and gross domestic product per capita (gdpPercap) 
# for each year and country. For such variables, density plots provide a useful graphical summary.
ggplot(data=gapminder, aes(x=lifeExp)) + 
  geom_density()

# Diferencias por país
ggplot(data=gapminder, aes(x=lifeExp, fill=continent)) +
  geom_density(alpha=0.3)

  
  
  