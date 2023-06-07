# DATI WORLD BANK POPOLAZIONE RURALE
# LINK VIDEO: https://www.youtube.com/watch?v=8kunLjakT0c

# CARICAMENTO LIBRERIE ----
library(tidyverse)
library(sf)
library(rnaturalearth)
library(wbstats)
library(gganimate)
library(classInt)
library(av)

# CARICAMENTO DATI ----
## WB DATA ----
kpi_df <- wbstats::wb_search(pattern = "rural population")
fix(kpi_df)

rural_df <- wbstats::wb_data(
  indicator = "SP.RUR.TOTL.ZS",
  country = "countries_only",
  start_date = 2001, end_date = 2022
)

rural_df

rural_world_df <- rural_df |>
  dplyr::select(, c(1, 4:5))

names(rural_world_df) <- c(
  "iso2", "Year", "% rural population"
)

rural_world_df

## WORLD SHAPEFILE ----

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

world_sf <- rnaturalearth::ne_countries(
  type = "countries", scale = "small"
) |>
  sf::st_as_sf() |>
  sf::st_transform(crsLONGLAT) |>
  sf::st_set_crs(crsLONGLAT)
head(world_sf)
names(world_sf)
plot(sf::st_geometry(world_sf))

world_sf_no_antartica <- world_sf |>
  dplyr::filter(region_un != "Antarctica") |>
  dplyr::select(iso_a2, name)

plot(sf::st_geometry(world_sf_no_antartica))

# JOIN DATA & SHAPEFILE ----
rural_world_sf <- dplyr::left_join(
  world_sf_no_antartica, rural_world_df,
  by = c("iso_a2" = "iso2")
)

rural_world_sf

# BREAKS ----
vmin <- min(rural_world_sf$`% rural population`, na.rm = TRUE, finite = TRUE)
vmax <- max(rural_world_sf$`% rural population`, na.rm = TRUE, finite = TRUE)
brk <- round(classInt::classIntervals(
  rural_world_sf$`% rural population`,
  n = 6,
  style = "fisher"
)$brks, 1) |>
  head(-1) |>
  tail(-1) |>
  append(vmax)
breaks <- c(vmin, brk)

cols <- rev(c('#f0951e', '#d59956', '#b39c81', '#859fa9', '#00a2d2'))

# ANIMATE ----
get_animated_world_map <- function() {
  world_map <- ggplot(
    data = rural_world_sf,
    aes(fill = `% rural population`)
  ) +
    geom_sf(color = "white", size = 0.05) +
    scale_fill_gradientn(
      name = "",
      colours = cols,
      breaks = breaks,
      labels = round(breaks, 1),
      limits = c(vmin, vmax),
      na.value = "grey70"
    ) +
    coord_sf(crs = crsLONGLAT) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = .5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = c(.5, -.015),
      legend.text = element_text(size = 11, color = "grey10"),
      panel.grid.major = element_line(color = "white", size = .2),
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        face = "bold", size = 20,
        color = "grey10", hjust = .5, vjust = -3
      ),
      plot.subtitle = element_text(
        size = 40, color = "#c43c4e",
        hjust = .5, vjust = -1
      ),
      plot.caption = element_text(
        size = 10, color = "grey10",
        hjust = .5, vjust = -10
      ),
      plot.margin = unit(c(t = -4, r = -4, b = -4, l = -4), "lines"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank()
    ) +
    labs(
      x = "",
      y = "",
      title = "Rural population (% of population)",
      subtitle = "Year: {as.integer(current_frame)}",
      caption = "©2023 Sport Business Lab Consultancy
        World Development Indicators, The World Bank"
    )
  
  return(world_map)
}

world_map <- get_animated_world_map()
print(world_map)

timelapse_world_map <- world_map +
  transition_manual(Year) +
  enter_grow() +
  ease_aes("quadratic-in-out", interval = .2)

animated_world <- gganimate::animate(
  timelapse_world_map,
  nframes = 120,
  duration = 20,
  start_pause = 3,
  end_pause = 30,
  height = 6,
  width = 7.15,
  res = 300,
  units = "in",
  fps = 15,
  renderer = gifski_renderer(loop = T)
)

av_video_convert(animated_world, output = "output.mp4", verbose = TRUE)

gganimate::anim_save(
  "rural_population_world_3.mp4", animation = animate(timelapse_world_map, renderer = av_renderer())
)



gganimate::anim_save(
  "rural_population_world.gif", animated_world
)

av_video_convert("rural_population_world.gif", output = "output.mp4", verbose = TRUE)



## save as mp4
anim_save(  filename = "animation.mp4",   animation = animate(animated_plot,                       renderer = av_renderer())
