library(tidyverse)
library(osmdata)
library(tmaptools)
library(XML)

##### Chicago Marathon
palette <- c(
	background = '#142068',
	water = '#00A4E4',
	streets = '#929292',
	small_streets = '#929292',
	rivers = '#00A4E4',
	route = '#E31D39',
	labels = '#FFFFFF',
	title = '#FFFFFF'
)

gpx.file <- 'course_data/2022-10-09-Chicago-Marathon.gpx'
title <- 'Chicago Marathon'
subtitle <- 'October 9, 2022'
title.hjust <- 0 # 0 = left align; 1 = right align
distance <- "mi" # Distance unit, one of: "m", "km", "mi", and "ft"



gpx <- XML::htmlTreeParse(gpx.file, 
						  error = function (...) {}, useInternalNodes = T)

coords <- xpathSApply(gpx, path = "//trkpt", xmlAttrs)
lats <- as.numeric(coords["lat",])
lons <- as.numeric(coords["lon",])
path <- data.frame(x = lons, y = lats)
bb.route <- matrix(c(min(path$x), min(path$y), max(path$x), max(path$y)),
				   nrow = 2, ncol = 2,
				   dimnames = list(c('x','y'), c('min', 'max')))


bb <- bb.route

# Get the data from OpenStreet Maps
streets <- bb %>%
	opq() %>%
	add_osm_feature(key = "highway", 
					value = c("motorway", "primary", "trunk",
							  "secondary", "tertiary")) %>%
	osmdata_sf()

small_streets <- bb %>%
	opq() %>%
	add_osm_feature(key = "highway",
					value = c("residential", "living_street",
							  "unclassified",
							  "service", "footway")) %>%
	osmdata_sf()

river <- bb %>%
	opq() %>%
	add_osm_feature(key = "waterway", value = "river") %>%
	osmdata_sf()

water <- bb %>%
	opq() %>%
	add_osm_feature(key = "natural", value = c('water')) %>%
	osmdata_sf()

# Mile markers
path$distance <- 0
for(i in 2:nrow(path)) { 
	# Probably shouldn't use a loop, this is slow. Not sure what to use instead.
	path[i,]$distance <- as.numeric(
		suppressMessages(
			approx_distances(unlist(path[i - 1,,drop=TRUE]), unlist(path[i,,drop=TRUE]), 
						 target = distance, projection = 4326)
		)
	)
}
path$cum_distance <- cumsum(path$distance)
# We could round up or down here. For NYC, my watch registered just under 26 miles
# so to ensure a 26 mile marker is shown, we will round up here. I presume the
# GPS lost accuracy when running on the lower level of bridges and/or through
# the buildings.
# markers <- path[!duplicated(floor(path$cum_distance)),][-1,]
markers <- path[!duplicated(ceiling(path$cum_distance), fromLast = TRUE),][-1,]

map <- ggplot() +
	geom_sf(data = water$osm_multipolygons,
			inherit.aes = FALSE,
			fill = palette['water'],
			color = NA,
			alpha = .3) +
	geom_sf(data = streets$osm_lines,
			inherit.aes = FALSE,
			color = palette['streets'],
			size = .3,
			alpha = .6) +
	geom_sf(data = small_streets$osm_lines,
			inherit.aes = FALSE,
			color = palette['small_streets'],
			size = .1,
			alpha = .5) +
	geom_sf(data = river$osm_lines,
			inherit.aes = FALSE,
			color = palette['rivers'],
			size = .2,
			alpha = .3) +
	geom_path(data = path, aes(x = x, y = y),
			  color = palette['route'],
			  size = 1) +
	geom_point(data = markers, aes(x = x, y = y),
			   inherit.aes = FALSE,
			   color = palette['labels'],
			   fill = palette['route'],
			   shape = 21, stroke = 1, size = 5) +
	geom_point(data = path[1,], aes(x = x, y = y),
			   inherit.aes = FALSE,
			   color = palette['labels'],
			   fill = 'green',
			   shape = 21, stroke = 1, size = 5) +
	geom_text(data = markers, aes(x = x, y = y, label = ceiling(cum_distance)),
			  inherit.aes = FALSE,
			  color = palette['labels'],
			  size = 2) +
	geom_point(data = path[nrow(path),], aes(x = x, y = y),
			   inherit.aes = FALSE,
			   color = palette['labels'],
			   fill = 'red',
			   shape = 21, stroke = 1, size = 5) +
	coord_sf(xlim = bb[1,],
			 ylim = bb[2,]) +
	theme_void() +
	theme(plot.background = element_rect(fill = palette['background']))

map + ggtitle(title,   
			  subtitle = subtitle) +
	theme(panel.background = element_rect(fill = palette['background'], color = palette['background']),
		  panel.spacing = margin(0,0,0,0),
		  plot.margin = margin(-45,10,10,10),
		  plot.title = element_text(color = palette['title'],
		  						  size = 20,
		  						  hjust = title.hjust, vjust = -10,
		  						  family = 'Helvetica'),
		  plot.subtitle = element_text(color = palette['title'],
		  							 size = 16,
		  							 hjust = title.hjust, vjust = -12,
		  							 family = 'Helvetica'))

ggsave(filename = paste0(gsub(' ', '_', title), '.png'))
