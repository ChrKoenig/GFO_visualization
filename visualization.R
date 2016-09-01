# This script produces a circular plot that visualizes the distance between the GFÖ conference centre 
# and a number of restaurants, bars and nightclubs in Marburg. 
# 
# Written by Christian König
# Biodiversity, Macroecology and Biogeography Group
# University of Göttingen

#install.packages("circlize")
#install.packages("geosphere")
#install.packages("plyr")
library(circlize)
library(geosphere)

data_full = read.csv("Visualization Award.csv", sep = ";", stringsAsFactors = F)

###############################################################
#### Prepare data
# 1. Remove non-beer-serving venues!
data = subset(data_full, category %in% c("imbiss", "restaurant", "bar_pub_bistro"))

# 2. Remove duplicates
data = data[which(!duplicated(data$name)),]

# 3. Calculate distance to university
uni_coords = as.numeric(data_full[which(data_full$name == "Zentrales Hörsaalgebäude (venue)"), c("latitude", "longitude")])
data$dist_to_university = distm(data[,c("latitude", "longitude")], uni_coords, fun = distHaversine)

# 4. Use only rather proximate venues
data = subset(data, dist_to_university < 2500)
data$scale = 1 - (data$dist_to_university / 2500)
data = data[order(data$dist_to_university),]

# 5. add colors
legend_colors = c(rgb(0,1,0), rgb(1,0.6,0), rgb(0,0.7,1))
data$colors = legend_colors[as.factor(data$category)]

###############################################################
# Main plotting function
draw_slices = function(data){
  par(mar = c(1, 1, 4, 1))
  plot(c(-1.85, 1.85), c(-1.7, 1.7), type = "n", axes = FALSE, main = NA, 
       col.main = "firebrick1", cex.main = 2)
  slice_bounds = rev(seq(0,360, length.out = (nrow(data)+1))) # define width of slices
  slice_bounds = (slice_bounds + 90) %% 360 # rotate to start with shortest distance on top
  CF = 0.01745329252 # Conversion factor from degree to radians
  draw.sector(0, 360, col = "white", border = "black")
  
  # draw individual slices
  for(i in 2:length(slice_bounds)){
    start_deg = slice_bounds[i]
    stop_deg = slice_bounds[i-1]
    mean_deg = ifelse(stop_deg < start_deg, (start_deg + stop_deg ) / 2 + 180, (start_deg + stop_deg) / 2)
    lines(x = c(0, cos(start_deg * CF)), y = c(0,sin(start_deg * CF)), col = "gray50")
    scale = data$scale[i-1]
    color = data$colors[i-1]
    draw.sector(start.degree = start_deg, end.degree = stop_deg, rou1 = scale, clock.wise = F, 
                col = color, border = NA)
    label = paste(data$name[i-1], ": ", data$street[i-1], " ", data$street_no[i-1], sep = "")
    srt_tmp = ifelse(mean_deg > 90 & mean_deg < 270, mean_deg-180, mean_deg)
    adj_tmp = ifelse(mean_deg > 90 & mean_deg < 270, 1, 0)
    text(x = cos(mean_deg * CF) *1.01, y = sin(mean_deg * CF) * 1.01, 
         labels = label, cex = 0.65, srt = srt_tmp, adj = adj_tmp)
  }
  
  # add title, text and other stuff
  draw.sector(0, 360, col = NA, rou1 = 0.8, border = "gray50")
  draw.sector(0, 360, col = NA, rou1 = 0.6, border = "gray50")
  draw.sector(0, 360, col = NA, rou1 = 0.4, border = "gray50")
  draw.sector(0, 360, col = NA, rou1 = 0.2, border = "gray50")
  mtext("Where (not) to get drunk tonight", side = 3, line = 2, cex = 1.8, font = 2)
  mtext("- Distance to the next beer-serving venue. More whitespace means longer walkway -", side = 3, line = 0.5, cex = 1.3)
  text(-1.55, -1.4, "Legend", cex = 1.3, adj = c(1,1), font = 2)
  text(c(-1.55, -1.55, -1.55, -1.55), c(-1.56, -1.64, -1.72, -1.8), 
       labels = c("Bar/Pub/Bistro", "Restaurant", "Imbiss", "500 m distance from conference center"), adj = c(0,0.5))
  points(c(-1.65, -1.65, -1.65), c(-1.56, -1.64, -1.72), pch = 15, col = legend_colors[c(1,3,2)], cex = 1.7)
  polygon(x = c(-1.82, -1.82, -1.62, -1.62), y = c(-1.82, -1.78, -1.78, -1.82), border = "gray50", lwd = 1.4)
  text(1.75, -1.4, "Need a taxi?", cex = 1.3, adj = c(1,1), font = 2)
  text(c(1.75, 1.75, 1.75, 1.75), c(-1.56, -1.64, -1.72, -1.8), 
       labels = c("CityTaxi: 06421-44411", "Ayse's Taxi: 06421-44477" , "Taxi Mitte: 06421-22222", "Taxi Kazim: 06421-9488877"),
       adj = c(1,0.5))
}

pdf(file = "viz.pdf", width = 11, height = 11)
draw_slices(data)
dev.off()

