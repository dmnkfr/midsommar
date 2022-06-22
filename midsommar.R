## Load Libraries
library(tidyverse)
library(stringr)
library(chron)
library(ggallin)
library(showtext)

## Get Fonts 
font_add_google(name = "Teko", family = "teko")
font_add_google(name = "Lobster", family = "lobster")
font_add_google(name = "Amatic SC", family = "amatic")
font_add_google(name = "Bebas Neue", family = "bebas")
font_add_google(name = "Righteous", family = "righteous")
showtext_auto()

## Load and Tidy Data
rain <- read.csv(
  file = "smhi-opendata_rain.csv",
  skip = 9,
  sep = ";"
) %>%
  select(Representativt.dygn, Nederbördsmängd) %>%
  rename(
    Date = Representativt.dygn,
    Precipitation = Nederbördsmängd
  ) %>%
  filter(str_detect(Date, "06-23")) %>%
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"),
    Precipitation = Precipitation * -1
  ) %>%
  select(Date, Precipitation)


## Load and Tidy Data
temp <- read.csv(
  file = "smhi-opendata.csv",
  skip = 9,
  sep = ";"
) %>%
  select(Datum, Tid..UTC., Lufttemperatur) %>%
  rename(
    Time = Tid..UTC.,
    Date = Datum,
    Temperature = Lufttemperatur
  ) %>%
  filter(str_detect(Date, "06-23")) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  select(Date, Time, Temperature) %>%
  group_by(Date) %>%
  summarise(Mean_Temperature = round(mean(Temperature), 2))

data <- rain %>%
  left_join(temp) %>%
  mutate(year = substr(Date, 1, 4)) %>%
  select(-Date) %>%
  pivot_longer(cols = -year, names_to = "measure", values_to = "value")

head(data)

## Plot
p <- ggplot(
  data = filter(data, year >= 1860),
  mapping = aes(
    x = year,
    y = value,
    group = measure,
    fill = measure
  )
) +
  geom_area(show.legend = FALSE) +
  scale_fill_manual(values = c(
    "#FFB81C", "#25A1B3"
  )) +
  scale_x_discrete(breaks = seq(1860, 2020, 22)) +
  labs(
    title = "Weather on Midsummer in Stockholm",
    subtitle = "1860-2020",
    caption = "Data Source : SMHI | Visualization: Dominik Freunberger"
  ) +
  theme_void() +
  geom_hline(yintercept = 0, linetype = "solid", color = "white", size = 1) +
  geom_hline(yintercept = 10, linetype = "dotted", color = "white", size = 1, alpha = .7) +
  geom_hline(yintercept = 20, linetype = "dotted", color = "white", size = 1, alpha = .7) +
  geom_hline(yintercept = -10, linetype = "dotted", color = "white", size = 1, alpha = .7) +
  geom_hline(yintercept = -20, linetype = "dotted", color = "white", size = 1, alpha = .7) +
  geom_text(aes(
    x = 1,
    y = 12,
    label = "10°C"
  ),
  family = "amatic",
  colour = "white",
  hjust = -0,
  size = 16,
  alpha = .7
  ) +
  geom_text(aes(
    x = 1,
    y = 22,
    label = "20°C"
  ),
  family = "amatic",
  colour = "white",
  hjust = -0,
  size = 16,
  alpha = 0.1
  ) +
  geom_text(aes(
    x = 1,
    y = -18,
    label = "20mm"
  ),
  family = "amatic",
  colour = "white",
  hjust = -0,
  size = 16,
  alpha = 0.1
  ) +
  geom_text(aes(
    x = 1,
    y = -8,
    label = "10mm"
  ),
  family = "amatic",
  colour = "white",
  hjust = -0,
  size = 16,
  alpha = 0.1
  ) +
  geom_text(aes(
    x = 114,
    y = 26,
    label = "26°C in 1973"
  ),
  family = "amatic",
  colour = "white",
  hjust = -0,
  size = 12,
  alpha = 0.1
  ) +
  geom_text(aes(
    x = 25,
    y = -24,
    label = "25mm in 1884"
  ),
  family = "amatic",
  colour = "white",
  hjust = -0,
  size = 12,
  alpha = 0.1
  ) +
  theme(
    panel.background = element_rect(fill = "#000806", colour = "#000806"),
    plot.background = element_rect(fill = "#000806", colour = "#000806"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      family = "amatic", colour = "white", face = "bold", size = 100, hjust = 0.5,
      margin = margin(t = 10)
    ),
    plot.subtitle = element_text(
      family = "amatic", colour = "white", size = 50, hjust = 0.5, vjust = 3,
      margin = margin(t = 10, b = 5)
    ),
    plot.caption = element_text(
      colour = "white", size = 25, hjust = 0.5,
      margin = margin(t = 5, b = 5)
    )
  )

ggsave("glad_midsommar.png", p, dpi = 320, width = 12, height = 6)
