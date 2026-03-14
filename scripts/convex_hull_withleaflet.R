# -----------------------------------------------------------
# Load libraries
# -----------------------------------------------------------

library(sf)       # spatial data
library(ggplot2)  # data view
library(leaflet)  # intercative map


# -----------------------------------------------------------
# 1. Read record table
# -----------------------------------------------------------

dados <- read.delim("data/records_stryphnodendron_confertum.txt")

# -----------------------------------------------------------
# 2. Remove duplicates
# -----------------------------------------------------------

# Remove lines with same longitude and latitude
dados <- unique(dados[, c("lon","lat")])

# Other possible solution:
# dados <- dados[!duplicated(dados[,c("lon","lat")]), ]


# -----------------------------------------------------------
# 3. Convert to spatial object
# -----------------------------------------------------------

# EPSG 4326 = geographical system based on datum WGS84
# unit = graus (latitude/longitude)

sp_points <- st_as_sf(
  dados,
  coords = c("lon","lat"),
  crs = 4326
)


# -----------------------------------------------------------
# 4. Detect possible outliers
# -----------------------------------------------------------

# Transform to UTM so distances can be measured
# Zona 23S cobre grande parte do Brasil central

sp_points_utm <- st_transform(sp_points, 32723)

# Calculate distribution centroid
centro <- st_centroid(st_union(sp_points_utm))

# Distance to centroid
distancias <- st_distance(sp_points_utm, centro)

# Convert to km
dist_km <- as.numeric(distancias) / 1000

# Limite de outlier = média + 3 desvios padrão
limite <- mean(dist_km) + 3*sd(dist_km)

# identificar outliers
outliers <- dist_km > limite

# separar pontos normais e outliers
sp_points_clean <- sp_points_utm[!outliers, ]
sp_points_outliers <- sp_points_utm[outliers, ]


# -----------------------------------------------------------
# 5. Calcular EOO (Extent of Occurrence)
# -----------------------------------------------------------

# Convex hull com pontos SEM outliers

hull <- st_convex_hull(
  st_union(
    st_combine(sp_points_clean)
  )
)

# área em km2
eoo_km2 <- st_area(hull) / 1e6

print(eoo_km2)


# -----------------------------------------------------------
# 6. Visualização simples (base R)
# -----------------------------------------------------------

#plot(st_geometry(hull), col="lightblue")
#plot(st_geometry(sp_points_clean), add=TRUE, pch=16)
#plot(st_geometry(sp_points_outliers), add=TRUE, col="red", pch=17)

# -----------------------------------------------------------
# 7. Visualização com ggplot
# -----------------------------------------------------------

ggplot() +
   geom_sf(data = hull, fill = "lightblue", color = "blue", alpha = 0.4) +
   geom_sf(data = sp_points_clean, color = "darkgreen", size = 1) +
   geom_sf(data = sp_points_outliers, color = "red", size = 3) +
   scale_x_continuous(n.breaks = 2, labels = function(x) paste0(round(x,2),"°W")) +
   scale_y_continuous(n.breaks = 2, labels = function(y) paste0(round(y,2),"°S")) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   expand_limits(x = c(min(dados$lon) - 0.1,
                      max(dados$lon) + 0.1)) + labs(x = NULL, y = NULL) +
   labs(x = "Longitude", y = "Latitude") +
   labs(title = "Distribution of Stryphnodendron confertum") +
   theme(plot.title = element_text(size = 8))
   
# -----------------------------------------------------------
# Load libraries
# -----------------------------------------------------------

library(sf)       # spatial data
library(ggplot2)  # data visualization
library(leaflet)  # interactive map


# -----------------------------------------------------------
# 1. Read occurrence table
# -----------------------------------------------------------

records <- read.delim("data/records_stryphnodendron_confertum.txt")

# -----------------------------------------------------------
# 2. Remove duplicate coordinates
# -----------------------------------------------------------

# Remove rows with identical longitude and latitude
records <- unique(records[, c("lon","lat")])

# Alternative solution:
# records <- records[!duplicated(records[,c("lon","lat")]), ]


# -----------------------------------------------------------
# 3. Convert to spatial object
# -----------------------------------------------------------

# EPSG 4326 = geographic coordinate system based on WGS84 datum
# unit = degrees (latitude/longitude)

sp_points <- st_as_sf(
  records,
  coords = c("lon","lat"),
  crs = 4326
)


# -----------------------------------------------------------
# 4. Detect potential outliers
# -----------------------------------------------------------

# Transform to UTM so distances can be measured in meters
# Zone 23S covers a large portion of central Brazil

sp_points_utm <- st_transform(sp_points, 32723)

# Calculate distribution centroid
centroid <- st_centroid(st_union(sp_points_utm))

# Distance from each point to the centroid
distances <- st_distance(sp_points_utm, centroid)

# Convert to kilometers
dist_km <- as.numeric(distances) / 1000

# Outlier threshold = mean + 3 standard deviations
outlier_threshold <- mean(dist_km) + 3*sd(dist_km)

# Identify outliers
outliers <- dist_km > outlier_threshold

# Separate regular points and outliers
sp_points_clean <- sp_points_utm[!outliers, ]
sp_points_outliers <- sp_points_utm[outliers, ]


# -----------------------------------------------------------
# 5. Calculate EOO (Extent of Occurrence)
# -----------------------------------------------------------

# Convex hull calculated using points WITHOUT outliers

hull <- st_convex_hull(
  st_union(
    st_combine(sp_points_clean)
  )
)

# Area in km²
eoo_km2 <- st_area(hull) / 1e6

print(eoo_km2)


# -----------------------------------------------------------
# 6. Simple visualization (base R)
# -----------------------------------------------------------

#plot(st_geometry(hull), col="lightblue")
#plot(st_geometry(sp_points_clean), add=TRUE, pch=16)
#plot(st_geometry(sp_points_outliers), add=TRUE, col="red", pch=17)

# -----------------------------------------------------------
# 7. Visualization using ggplot2
# -----------------------------------------------------------

ggplot() +
   geom_sf(data = hull, fill = "lightblue", color = "blue", alpha = 0.4) +
   geom_sf(data = sp_points_clean, color = "darkgreen", size = 1) +
   geom_sf(data = sp_points_outliers, color = "red", size = 3) +
   scale_x_continuous(n.breaks = 2, labels = function(x) paste0(round(x,2),"°W")) +
   scale_y_continuous(n.breaks = 2, labels = function(y) paste0(round(y,2),"°S")) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   expand_limits(x = c(min(records$lon) - 0.1,
                      max(records$lon) + 0.1)) +
   labs(x = "Longitude", y = "Latitude") +
   labs(title = "Distribution of Stryphnodendron confertum") +
   theme(plot.title = element_text(size = 8))


# -----------------------------------------------------------
# 8. Interactive map without outliers
# -----------------------------------------------------------

# Calculate geographic centroid of the distribution
centroid_geo <- st_centroid(st_union(sp_points))

# Distance from each point to centroid
distances_geo <- st_distance(sp_points, centroid_geo)

# Convert to kilometers
dist_km_geo <- as.numeric(distances_geo) / 1000

# Define outlier threshold
threshold_geo <- mean(dist_km_geo) + 3*sd(dist_km_geo)

# Identify outliers
outliers_geo <- dist_km_geo > threshold_geo

# Separate regular points and outliers
sp_points_clean_geo <- sp_points[!outliers_geo, ]
sp_points_outliers_geo <- sp_points[outliers_geo, ]

# Convex hull of cleaned dataset
hull_geo <- st_convex_hull(
  st_union(
    st_combine(sp_points_clean_geo)
  )
)

leaflet() |>
  addTiles() |>
  addPolygons(data = hull_geo, color="blue", fillOpacity = 0.3) |>
  addCircleMarkers(data = sp_points_clean_geo, radius = 2, color = "green") |>
  addCircleMarkers(data = sp_points_outliers_geo, radius = 3, color = "red")