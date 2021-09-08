library(sf)
library(tidyverse)
library(mapview)

## Create the four corners from the grid area
a <- as.data.frame(matrix(c(109, 109, 162, 162, -8.5, -49, -49, -8.5), ncol = 2))
# view(a) ## check it

a2 <- a %>%
  sf::st_as_sf(coords = c("V1","V2"), crs = 4326)

# mapview::mapview(a2) ## check it spatially

## Squared grid
grid <- 
  sf::st_make_grid(a2, cellsize = c(1, 1), crs = 4326)

# mapview::mapview(grid)

## Hexagonal grid
grid_hex <- 
  sf::st_make_grid(a2, cellsize = c(1, 1), crs = 4326, square = FALSE)

mapview::mapview(grid_hex)

### ****************************************************************************
### ***************************** For trials  **********************************
### ****************************************************************************

a <- as.data.frame(matrix(c(-180, -180, 180, 180, 90, -90, 90, -90), ncol = 2))

a2 <- a %>%
  sf::st_as_sf(coords = c("V1","V2"), crs = 4326)

mapview::mapview(a2)

## Hexagonal grid -- 10
grid_hex10 <- 
  sf::st_make_grid(a2, cellsize = c(10, 10), crs = 4326, square = FALSE)

mapview::mapview(grid_hex10)

## Hexagonal grid -- 15
grid_hex15 <- 
  sf::st_make_grid(a2, cellsize = c(15, 15), crs = 4326, square = FALSE)

mapview::mapview(grid_hex15)

### Overlay in a ggplot Fig.
world <- ggplot2::map_data("world")

map <-
  ggplot2::ggplot() + 
  ggplot2::geom_sf(data = grid_hex15) +
  ggplot2::geom_map(data = world, map = world, 
                    aes(x = long, y = lat, map_id = region), 
                    color = "black", fill = "black", size = 0.1) +
  theme_bw()

