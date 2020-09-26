library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(ggspatial)
library(maps)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)
states <- cbind(states, st_coordinates(st_centroid(states)))
states$ID <- toTitleCase(states$ID)
head(states)

scty <- data.frame(state = rep("Texas", 4), 
                   cty.cent = c("San Augustine", "Sabine", "Shelby", "Trinity"), 
                   lat = c(31.4699, 31.2974, 31.8343, 31.0688), 
                   lng = c(-94.1514, -93.8248, -94.1514, -95.1432))

(scty <- st_as_sf(scty, coords = c("lng", "lat"), remove = FALSE, 
                  crs = 4326, agr = "constant"))

states$nudge_x <- -0.55
states$nudge_x[states$ID == "Mississippi"] <- -0.4
states$nudge_x[states$ID == "Alabama"] <- -0.15
states$nudge_x[states$ID == "New Mexico"] <- 1

states$nudge_y <- -0.01
states$nudge_y[states$ID == "Louisiana"] <- 0.25
states$nudge_y[states$ID == "Mississippi"] <- 0.25


ggplot(data = world) +
  geom_sf(fill = "#FFFFCC") +
  geom_sf(data = states, fill = NA) +
  geom_text(data = states, aes(X, Y, label = ID), nudge_x = states$nudge_x, 
            nudge_y = states$nudge_y, fontface = "italic", size = 3.5) +
  geom_sf(data = scty) +
  geom_label_repel(data = scty, aes(x = lng, y = lat, label = cty.cent), 
                   fontface = "bold", 
                   nudge_x = c(-3, -1, -2, -2), 
                   nudge_y = c(0.25, -0.8, 0.75, -0.5),
                   color = "#660000", size = 3) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.07, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-107, -89), ylim = c(25, 37), expand = FALSE) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue")) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Samples from Texas Counties", subtitle = "(archaeological site locations redacted)")