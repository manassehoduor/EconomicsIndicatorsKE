
# load libraries
pacman::p_load(tidyverse,janitor,scales,hrbrthemes,viridis,ggtext,showtext,extrafont,shadowtext)


# Import fonts
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'>  Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | Source: KNBS | #EconomicIndicatorsKE")

# load flight data
cpi_df <- read.csv(file = "CPI.csv", header = TRUE, sep = ",")

# data wrangling
cpi_df <- cpi_df |>
  clean_names()

cpi_df_decade <- cpi_df |>
  mutate(month = factor(month)) |>
  filter(year>2011)

lbl <-  data.frame(y = 2012:2023) |> 
  mutate(x = "Jan")

# heat map
ggplot(cpi_df_decade) +
  geom_tile(aes(month, year, fill = cpi), color = "grey99") +
  geom_text(aes(month, 2025, label = month), family = "Roboto Condensed", 
            size = 5, stat = "unique", colour = "#435710") +
  shadowtext::geom_shadowtext(data = lbl, aes(x, y, label = y, 
                                              color = if_else(y == 2012, "white", "white")), 
                              size = 2, family = "Roboto Condensed", bg.color = "#add50d") +
  scale_color_identity() +
  scale_fill_viridis_c(option = "mako", direction = -1,
                       name = "Consumer Price Index (CPI)") +
  coord_polar(clip = "off") +
  scale_x_discrete(limits = month.abb) +
  theme_minimal() +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.title = element_markdown(colour = "black",face = "bold",
                                      size = 15, hjust = 0.5, family = "Roboto Condensed"),
        plot.subtitle = element_markdown(colour = "black",size = 11,
                                         hjust = 0.5, family = "Roboto Condensed"),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "top",
        legend.title.align = 0.5,
        legend.box.background = element_blank(),
        legend.title = element_text(family = "Roboto Condensed", face = "bold", size = 12),
        plot.caption = element_markdown(colour = 'black', hjust = 0.5, size = 9,
                                        family = 'Rosario', margin = margin(t = 20))) +
  labs(
    y = "",
    x = "",
    caption = cap,
    subtitle = "The Consumer Price Index (CPI) has been steadily rising over the past decade with a <br> sharp trend observed over recent months.  It is clear that the rate of increase is too high.<br> An indication that inflation is rising too quickly, which can have negative impacts on consumers and businesses.",
    title = "<br>KENYA CONSUMER PRICE INDEX") +
  guides(fill = guide_colourbar(direction = 'horizontal', ticks.linewidth=2,
                                barwidth = 14, barheight = 0.3, title.position = "top"))

ggsave("CPI.png", height=7, width=7, bg = "#f3f1ff")


