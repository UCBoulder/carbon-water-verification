# Fig 1: locations of baseline water quality sampling.
# Sampling coordinates are not part of the public data. This script reads them from a
# non-public file (map_points.csv, held by the authors) and is included to document how Fig 1 was drawn.
# Base layer: Natural Earth 1:50m admin-0 countries (public domain), via the rnaturalearth package.
suppressMessages({
  library(data.table); library(ggplot2)
  library(sf); library(rnaturalearth); library(rnaturalearthdata); library(ggspatial)
})
map_data <- fread(Sys.getenv("MAP_POINTS", "map_points.csv"))
map_data[, Label := c("LifeStraw - Kenya" = "LifeStraw (Kenya)", "Asili - DRC" = "Asili (DRC)",
  "Amazi Meza - Rwanda" = "Amazi Meza (Rwanda)", "DRIP - Kenya" = "DRIP-FUNDI (Kenya)",
  "Helvetas - Madagascar" = "Ranovola (Madagascar)", "Amazi Water - Burundi" = "Amazi Water (Burundi)",
  "Water Mission - Tanzania" = "Water Mission (Tanzania)")[Label]]
africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
map_sf <- st_as_sf(map_data, coords = c("Longitude","Latitude"), crs = 4326)
cent <- suppressWarnings(st_point_on_surface(africa))
bb <- st_bbox(map_sf); pad <- 2

p <- ggplot(africa) +
  geom_sf(fill = "antiquewhite", color = "gray40") +
  geom_sf(data = map_sf, aes(color = Label), size = 2, alpha = 0.85) +
  geom_sf_text(data = cent, aes(label = name), size = 3, color = "gray20") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(bb["xmin"]-pad, bb["xmax"]+pad), ylim = c(bb["ymin"]-pad, bb["ymax"]+pad), expand = FALSE) +
  theme_minimal(base_size = 10) +
  labs(x = NULL, y = NULL, color = "Project")

dir.create("figures", showWarnings = FALSE)
out <- "figures/Fig1.pdf"
ggsave(out, plot = p, width = 7.5, height = 6)
ggsave("figures/Fig1.tif", plot = p, width = 7.5, height = 6, dpi = 300, compression = "lzw", bg = "white")
cat("WROTE", out, "\n")
