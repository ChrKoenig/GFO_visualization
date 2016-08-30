# This script produces a circular plot that visualizers the distance between the GFÖ conference centre 
# and a number of restaurants, bars and nightclubs in Marburg. 
# The 

#install.packages("circlize")
#install.packages("geosphere")
#install.packages("plyr")
library(circlize)
library(geosphere)
library(plyr)

data = read.csv("Visualization Award.csv", sep = ";", stringsAsFactors = F)

###############################################################
### prepare data
data_full = data
data = subset(data_full,! category %in% c("taxi", "pharmacy", "")) # remove useless stuff
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

uni_coords = as.numeric(data[which(data$name == "Zentrales Hörsaalgebäude (venue)"), c("latitude", "longitude")])
data = data[!grepl("Zentrales Hörsaalgebäude|Zum Alten Schneider", data$name, useBytes = T),]
data$dist_to_university = distm(data[,c("latitude", "longitude")], uni_coords, fun = distHaversine) # calc distance to city university
data = data[with(data, order(category_raw, dist_to_university)),] # reorder

data$scale = 1 - (data$dist_to_university / 2500)
# add colors
table(data$category_raw)
data$colors = c(rep(rgb(0,1,0, maxColorValue = 1), table(data$category_raw)[1]),
                rep(rgb(1,0.4,0.3, maxColorValue = 1), table(data$category_raw)[2]),
                rep(rgb(0.6,0.4,0.75, maxColorValue = 1), table(data$category_raw)[3]),
                rep(rgb(0,0.7,1, maxColorValue = 1), table(data$category_raw)[4]))
data = subset(data, dist_to_university < 2500)
data = data[order(data_new$dist_to_university),]

draw_slices = function(data){
  par(mar = c(1, 1, 4, 1))
  plot(c(-1.7, 1.7), c(-1.7, 1.7), type = "n", axes = FALSE, main = "Where (not) to get drunk tonight", cex.main = 2)
  
  slice_bounds = rev(seq(0,360, length.out = (nrow(data)+1))) # define width of slices
  slice_bounds = (slice_bounds + 90) %% 360 # rotate to start with shortest distance on top
  CF = 0.01745329252 # Conversion factor from degree to radians
  draw.sector(0, 360, col = "white", border = "black")
  for(i in 2:length(slice_bounds)){
    start_deg = slice_bounds[i]
    stop_deg = slice_bounds[i-1]
    lines(x = c(0, cos(start_deg * CF)), y = c(0,sin(start_deg * CF)), col = "gray50")
    scale = data$scale[i-1]
    color = data$colors[i-1]
    draw.sector(start.degree = start_deg, end.degree = stop_deg, rou1 = scale, clock.wise = F, 
                col = color, border = NA)
    label = paste(data$name[i-1], ": ", data$street[i-1], " ", data$street_no[i-1], sep = "")
    text(x = cos(start_deg * CF) *1.01, y = sin(start_deg * CF) * 1.01, 
         labels = label, cex = 0.6, srt = start_deg, adj = c(0,0))
  }
  draw.sector(0, 360, col = NA, rou1 = 0.8, border = "gray50")
  draw.sector(0, 360, col = NA, rou1 = 0.6, border = "gray50")
  draw.sector(0, 360, col = NA, rou1 = 0.4, border = "gray50")
  draw.sector(0, 360, col = NA, rou1 = 0.2, border = "gray50")
  text(1.5, -1.3, "Need a taxi?", cex = 1.3, adj = c(1,1))
  text(1.5, -1.5, "CityTaxi: 06421-44411\nAyse's Taxi: 06421-44477\nTaxi Mitte: 06421-22222\nTaxi Kazim: 06421-9488877\nVIPcar: 06421-66699", cex = 0.9, adj = c(1,1))
}

pdf(file = "viz.pdf", width = 15, height = 15)
draw_slices(data_new)
dev.off()
