# Rail lines (total route-km)

# LINK VIDEO: https://www.youtube.com/watch?v=8kunLjakT0c

# CARICAMENTO LIBRERIE ----

library(tidyverse)
library(sf)
library(rnaturalearth)
library(wbstats)
library(gganimate)
library(classInt)
library(transformr)

# CARICAMENTO DATI ----
## WB DATA ----
kpi_df <- wbstats::wb_search(pattern = "Rail lines")  ######################
fix(kpi_df)

ind_df <- wbstats::wb_data(
  indicator = "IS.RRS.TOTL.KM",  ######################
  country = "countries_only",
  start_date = 1995, end_date = 2021  ############ SELEZIONARE ANNI
)

ind_df

ind_world_df <- ind_df |>
  dplyr::select(, c(1, 4:5))
  
  names(ind_world_df) <- c(
    "iso2", "Year", "Rail lines"   ######################
  )
  
  ind_world_df ######################
  
  ## WORLD SHAPEFILE ----
  
  crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  world_sf <- rnaturalearth::ne_countries(
    type = "countries", scale = "small"
  ) |>
    sf::st_as_sf(wkt = "geometry") |>
    sf::st_transform(crsLONGLAT)|>
    sf::st_set_crs(crsLONGLAT)
  st_crs(world_sf)
  head(world_sf)
  names(world_sf)
  plot(sf::st_geometry(world_sf))
  
  world_sf_no_antartica <- world_sf |>
    dplyr::filter(region_un != "Antarctica") |>
    dplyr::select(iso_a2, name)
  
  plot(sf::st_geometry(world_sf_no_antartica))
  
  # JOIN DATA & SHAPEFILE ----
  ind_world_sf <- dplyr::left_join(
    world_sf_no_antartica, ind_world_df,
    by = c("iso_a2" = "iso2")
  )
  
  ind_world_sf
  
  # PROJECTION ----
  # Robinson
  robinson_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  ind_world_sf_robinson <- ind_world_sf |>
    sf::st_transform(robinson_crs)
  plot(ind_world_sf_robinson)
# BREAKS ----
vmin <- min(ind_world_sf$`Rail lines`, na.rm = TRUE, finite = TRUE) ################
vmax <- max(ind_world_sf$`Rail lines`, na.rm = TRUE, finite = TRUE) ################
brk <- round(classInt::classIntervals(
  ind_world_sf$`Rail lines`, ###############
  n = 6,
  style = "fisher"
)$brks, 1) |>
  head(-1) |>
  tail(-1) |>
  append(vmax)
breaks <- c(vmin, brk)

cols <- rev(c('#ff2800', '#e3342a', '#c73b42', '#aa3f57', '#89416c', '#604380', '#004395'))

# ANIMATE ----
get_animated_world_map <- function() {
  world_map <- ggplot(
    data = ind_world_sf,
    aes(fill = `Rail lines`) #################
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
      panel.grid.major = element_line(color = "white", linewidth = .2),
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        face = "bold", size = 25,
        color = "#f0951e", hjust = .5, vjust = 0
      ),
      plot.subtitle = element_text(
        size = 20, color = "#00a2d2",
        hjust = .5, vjust = -1
      ),
      plot.caption = element_text( face = "italic",
                                   size = 10, color = "#25465d",
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
      title = "Rail lines (total route-km)",  ################
      subtitle = "Year: {as.integer(current_frame)}",
      caption = "Sport Business Lab Consultancy\nWorld Development Indicators\nThe World Bank"
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
  duration = 35, ###### VARIA IN BASE AL NUMERO DI ANNI
  start_pause = 3,
  end_pause = 30,
  height = 6,
  width = 7.15,
  res = 300,
  units = "in",
  fps = 15,
  renderer = gifski_renderer(loop = T)
)

gganimate::anim_save(
  "RAIL_LINES_world.gif", animated_world ############
)



