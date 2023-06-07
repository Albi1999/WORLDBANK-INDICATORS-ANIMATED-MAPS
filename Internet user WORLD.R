# CARICAMENTO LIBRERIE ----
library(tidyverse)
library(sf)
library(rnaturalearth)
library(wbstats)
library(gganimate)
library(classInt)
# 1. WB DATA
#-----------
kpi_df <- wbstats::wb_search(pattern = "internet users")
fix(kpi_df)

internet_df <- wbstats::wb_data(
  indicator = "IT.NET.USER.ZS",
  country = "countries_only",
  start_date = 2001, end_date = 2022
)

internet_df

internet_world_df <- internet_df |>
  dplyr::select(, c(1, 4:5))

names(internet_world_df) <- c(
  "iso2", "Year", "users"
)

internet_world_df

# 2. WORLD SHAPEFILE
#-------------------
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

world_sf <- rnaturalearth::ne_countries(
  type = "countries", scale = "small"
) |>
  sf::st_as_sf() |>
  sf::st_transform(crsLONGLAT)

head(world_sf)
names(world_sf)
plot(sf::st_geometry(world_sf))

world_sf_no_antartica <- world_sf |>
  dplyr::filter(region_un != "Antarctica") |>
  dplyr::select(iso_a2, name)

plot(sf::st_geometry(world_sf_no_antartica))

# 3. JOIN DATA & SHAPEFILE
#-------------------------
internet_world_sf <- dplyr::left_join(
  world_sf_no_antartica, internet_world_df,
  by = c("iso_a2" = "iso2")
)

internet_world_sf

# 4. PROJECTION
#--------------
# Robinson
robinson_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
internet_world_sf_robinson <- internet_world_sf |>
  sf::st_transform(robinson_crs)
plot(sf::st_geometry(internet_world_sf_robinson))
# 5. BREAKS
#----------
vmin <- min(internet_world_sf$users, na.rm = T)
vmax <- max(internet_world_sf$users, na.rm = T)
brk <- round(classIntervals(
  internet_world_sf$users,
  n = 6,
  style = "fisher"
)
$brks, 1) |>
  head(-1) |>
  tail(-1) |>
  append(vmax)
breaks <- c(vmin, brk)
cols <- rev(c('#f0951e', '#d59956', '#b39c81', '#859fa9', '#00a2d2'))

# 6. ANIMATE
#-----------
get_animated_world_map <- function() {
  world_map <- ggplot(
    data = internet_world_sf,
    aes(fill = users)
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
    coord_sf(crs = robinson_crs) +
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
      title = "Internet users (% of population)",
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
  renderer = gifski_renderer(loop = TRUE)
)
gganimate::anim_save(
  "internet_users_world.gif", animated_world
)