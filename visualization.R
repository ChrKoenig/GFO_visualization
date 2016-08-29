setwd("C://Users/Christian/Dropbox/PhD/Conferences/GfÖ2016")
#setwd("C://Users/KingKong/Dropbox/PhD/Conferences/Gf?2016")
data = read.csv("Visualization Award.csv", sep = ";", stringsAsFactors = F)

# prepare data
data_full = data
data = subset(data_full,! category %in% c("taxi", "pharmacy"))
data$category_raw = sapply(data$category, function(x){ 
  switch(as.character(x),
       accomodation = "accomodation",
       restaurant = "lunch & dinner",
       cafe = "lunch & dinner",
       imbiss = "lunch & dinner",
       bakery = "grocery & commodities",
       food = "grocery & commodities",
       supermarket = "grocery & commodities",
       other_essentials = "grocery & commodities",
       bar_pub_bistro = "nightlife")
})
data$category = NULL
data = data[-which(duplicated(data)),]

# distance
library(circlize)
library(geosphere)
library(plyr)

uni_coords = as.numeric(data[which(data$name == "Zentrales Hörsaalgebäude (venue)"), c("latitude", "longitude")])
data = data[!grepl("Zentrales Hörsaalgebäude|Zum Alten Schneider", data$name, useBytes = T),]
data$dist_to_university = distm(data[,c("latitude", "longitude")], uni_coords, fun = distHaversine) # calc distance to city university
data = data[with(data, order(category_raw, dist_to_university)),] # reorder

data$dist_grouprank = ave(data$dist_to_university, data$category_raw, FUN= rank ) # calculate ranks per group
data$scale = 1 - (data$dist_to_university / (max(data$dist_to_university)+1)) # relative proximity to university
  
# add colors
table(data$category_raw)
data$colors = c(colorRampPalette(colors = c(rgb(0,1,0, maxColorValue = 1), "white"))(table(data$category_raw)[1]),
                colorRampPalette(colors = c(rgb(1,0.4,0.3, maxColorValue = 1), "white"))(table(data$category_raw)[2]),
                colorRampPalette(colors = c(rgb(0.6,0.4,0.75, maxColorValue = 1), "white"))(table(data$category_raw)[3]),
                colorRampPalette(colors = c(rgb(0,0.7,1, maxColorValue = 1), "white"))(table(data$category_raw)[4]))
                               
draw_slices = function(data){ # we need columns for centrality
  slice_bounds = rev(seq(0,360, length.out = (nrow(data)+1))) # define width of slices
  slice_bounds = (slice_bounds + 90) %% 360 # rotate
  par(mar = c(0, 1, 4, 1))
  plot(c(-1.35, 1.4), c(-1.35, 1.35), type = "n", axes = FALSE, main = "After-conference guide to Marburg", cex.main = 2)
  draw.sector(0,360, border = "gray80", col = "gray95")
  for(i in 2:length(slice_bounds)){
    start_deg = slice_bounds[i]
    stop_deg = slice_bounds[i-1]
    scale = data$scale[i-1]
    color = data$colors[i-1]
    draw.sector(start.degree = start_deg, end.degree = stop_deg, rou1 = scale, clock.wise = F, 
                          col = color, border = NA)
    label = paste(data$name[i-1], ": ", data$street[i-1], " ", data$street_no[i-1], ", ", round(data$dist_to_university[i-1], 0) , " m", sep = "")
    text(x = cos(start_deg*0.01745329252) * scale, y = sin(start_deg*0.01745329252) * scale, 
         labels = label, cex = scale/2.5, srt = start_deg, adj = c(0,0))
  }
  legend(-1.5, -0.9, legend = c("accomodation", "lunch & dinner", "grocery & commodities", "nightlife"), title = "Legend", title.adj = 0.09,  fill = c(rgb(0.1,1,0, maxColorValue = 1), rgb(1,0.4,0.3, maxColorValue = 1), rgb(0.6,0.4,0.75, maxColorValue = 1), rgb(0,0.7,1, maxColorValue = 1)), bty = "n", cex = 1.3, border = NA, adj = 0)
  text(1.5, -.9, "Your destination too far?\nCall a taxi!", cex = 1.3, adj = c(1,1))
  text(1.5, -1.1, "CityTaxi: 06421-44411\nAyse's Taxi: 06421-44477\nTaxi Mitte: 06421-22222\nTaxi Kazim: 06421-9488877\nVIPcar: 06421-66699", cex = 0.9, adj = c(1,1))
  lines(matrix(c(-0.9659258,-1.2,0.258819,0.3), ncol = 2), col = "gray80")
  text(x = -1.2, y = 0.33, labels = "You are here", adj = c(0.5,0), cex = 1.3)
}

pdf(file = "viz.pdf", width = 10, height = 10)
draw_slices(data)
dev.off()


# Map
library(ggplot2)
library(ggmap)

marburg_map <- get_map(location = c(8.735, 50.766, 8.8, 50.850), maptype = "roadmap", 
                       source = "osm", zoom = 14, crop = F, force = T)
ggmap(marburg_map, extent = "device", scale = 4) + geom_point(aes(x = longitude, y = latitude), size = 2.5, data = data, colour = data$colors) + geom_point(aes(x = uni_coords[2], y = uni_coords[1]), size = 4, shape = 18)

