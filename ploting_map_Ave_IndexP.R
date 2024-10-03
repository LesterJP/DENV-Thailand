library(ggplot2)
library(sf)
library(dplyr)
library(raster)
library(gstat)
library(viridis)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sp)
library(lubridate)
library(rgdal)


# Load your data
data <- readr::read_csv("weather_data_LL_monthly.estimated_indexP.csv") # Ensure the file path is correct

# Convert the date field to Date object and extract year and month
data <- data %>%
  dplyr::mutate(date = as.Date(date, "%m/%d/%Y"),
                year = lubridate::year(date),
                month = lubridate::month(date)) %>%
  dplyr::select(date, month, year, indexP, Latitude, Longitude)



# Aggregate data by month and spatial coordinates (latitude and longitude), averaging indexP
monthly_avg <- data %>%
  group_by(month, Latitude, Longitude) %>%
  summarise(avg_indexP = mean(indexP, na.rm = TRUE), .groups = 'drop')

# Convert aggregated data to sf object for spatial operations
coordinates(monthly_avg) <- ~Longitude+Latitude
proj4string(monthly_avg) <- CRS("+proj=longlat +datum=WGS84")


# Get Thailand boundary data as an sf object
thailand <- ne_countries(country = "Thailand", scale = "medium", returnclass = "sf")
thailand_provinces <- ne_states(country = "Thailand", returnclass = "sf")

# Create a RasterLayer that covers the extent of Thailand with the desired resolution
thailand_extent <- st_bbox(thailand)
raster_template <- raster(xmn=thailand_extent$xmin, xmx=thailand_extent$xmax, ymn=thailand_extent$ymin, ymx=thailand_extent$ymax, resolution=0.1)
crs(raster_template) <- st_crs(thailand)$proj4string


# Define a function to perform IDW on aggregated data
perform_idw_avg <- function(spdf) {
  if (!inherits(spdf, "SpatialPointsDataFrame")) {
    stop("Input data must be a SpatialPointsDataFrame.")
  }
  
  grd <- rasterToPoints(raster_template)
  sp_grd <- SpatialPoints(grd[,1:2], proj4string=CRS(proj4string(spdf)))
  sp_grd_df <- SpatialPixelsDataFrame(points = sp_grd, data = data.frame(avg_indexP=rep(NA, length(sp_grd))))
  
  idw_model <- gstat(formula = avg_indexP ~ 1, locations = spdf, nmax = 30, set = list(idp = 0.5))
  idw_output <- predict(idw_model, newdata = sp_grd_df, na.action = na.pass)
  
  r <- raster(idw_output)
  return(r)
}

# Perform IDW for each month using the averaged data
for (month in 1:12) {
  month_data <- monthly_avg[monthly_avg$month == month, ]
  
  if (nrow(month_data) > 0) {
    interpolated_raster <- perform_idw_avg(month_data)
    
    # Mask the raster to Thailand's boundary
    thailand_sp <- as(st_geometry(thailand), "Spatial")
    raster_masked <- mask(interpolated_raster, thailand_sp)
    
    interpolated_df <- as.data.frame(raster_masked, xy = TRUE)
    gg <- ggplot() +
      geom_raster(data = interpolated_df, aes(x = x, y = y, fill = var1.pred), na.rm = TRUE) +
      geom_sf(data = thailand, fill = NA, color = "black") +
      geom_sf(data = thailand_provinces, fill = NA, color = "black", size = 1) +
      scale_fill_viridis_c(na.value = "gray95", guide = FALSE) + # guide = FALSE removes the legend
      labs(title = paste("Average monthly index P for", month.abb[month])) +
      theme_void() + # theme_void() for removing most plot elements including coordinates
      theme(plot.title = element_text(hjust = 0.5)) # Center the plot title
   ggsave(paste0("AvgIndexP_Map_", month.abb[month], ".png"), gg, width = 8, height = 6)
  }
}
