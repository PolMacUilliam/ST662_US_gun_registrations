setwd("C:\\Users\\PAUL\\Documents\\A_Maynooth HDip\\ST662 - Topics in Data Analytics\\Gun registration project")

#install.packages(c("choroplethr", "choroplethrMaps"))
#install.packages(c("rgeos", "maptools", "sp", "rgdal"))

#library(utils)
library(rgdal)
library(choroplethr)
library(choroplethrMaps)

library(rgeos)
library(maptools)

# MAP
us_states_map <- readOGR(dsn= getwd(),layer= "gadm36_USA_1")

par(mar=c(0,0,0,0))
plot(us_states_map, col="#f2f2f2", bg="paleturquoise4", lwd=0.25, mar=rep(0,4), border=0.5 )

# Next the shapefile has to be converted to a dataframe for use in ggplot2
us_states_map_df <- fortify(us_states_map)
state_choropleth(us_states_map_df, title = "my choropleth map", legend = "", num_colors = 7,
                 zoom = NULL, reference_map = FALSE)

map <- ggplot() +
  geom_path(data = us_states_map_df, 
            aes(x = long, y = lat, group = group),
            color = 'gray', size = .2)

print(map) 





# VERIFY IT LOADED PROPERLY
plot(us_states_map)
