library(qtbase)
library(qtpaint)

alpha <- function (colour, alpha) {
  col <- col2rgb(colour, TRUE)/255
  col[4, ] <- alpha
  rgb(col[1, ], col[2, ], col[3, ], col[4, ])
}
rescale01 <- function(x) (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE))


# County boundaries
if (!exists("boundary")) {
  options(stringsAsFactors = FALSE)
  boundary <- read.csv("boundaries.csv.bz2")
  # Remove alaska and hawawii for now
  boundary <- subset(boundary, !(state %in% c(2, 15)))

  polys <- split(boundary, boundary$group)
  source("poly.r")
  centers <- plyr::ddply(boundary, c("state", "county"), info)
  
  flow <- read.csv("flow.csv")
  flow <- merge(flow, centers, by.x = c("state_to", "county_to"),
    by.y = c("state", "county"))
    
  fips <- read.csv("fips.csv")
}

# Draw county borders
render_borders <- function(item, painter, exposed) { 
  qstrokeColor(painter) <- "grey70" 
  for(poly in polys) {
    qdrawPolygon(painter, poly$long, poly$lat)
  }
}

# Draw highlighted and selected counties
render_highlight <- function(item, painter, exposed) {
  if (is.na(highlighted)) return()
   
  h_poly <- polys[[highlighted + 1]]
  qdrawPolygon(painter, h_poly$long, h_poly$lat, 
    stroke = "NA", fill = "grey70")
  
  loc <- as.list(h_poly[1, c("state", "county")])
  county <- with(fips, county[state_fips == loc$state & county_fips == loc$county])
  state <- with(fips, state[state_fips == loc$state & county_fips == loc$county])
  
  qstrokeColor(painter) <- "black"
  qdrawText(painter, paste(county, state), 
    min(boundary$long), min(boundary$lat), "left", "bottom")

  s_poly <- polys[[selected + 1]]
  qdrawPolygon(painter, s_poly$long, s_poly$lat, 
    stroke = "black", fill = "grey50")
}

highlighted <<- NA
selected <<- NA

# Figure out which county is currently under the mouse
hover_county <- function(layer, event) {
  mat <- layer$deviceTransform(event)$inverted()

  rect <- qrect(-1, -1, 1, 1)
  rect <- mat$mapRect(rect) # now in data space
  pos <- event$pos()
  rect$moveCenter(pos) # centered on the pointer data pos
  highlighted <<- layer$locate(rect)[1] # get indices in rectangle
  
  qupdate(highlight)
}

# On click, select highlighted county
select_county <- function(layer, event) {
  selected <<- highlighted
  qupdate(flow_layer)
}

# Render movement between counties
render_flow <- function(item, painter, exposed) {
  if (is.na(selected)) return()
  county <- as.list(polys[[selected + 1]][1, 1:2])
  
  movement <- subset(flow, state_from == county$state & 
    county_from == county$county)
  movement$size <- sqrt(abs(movement$change) / max(abs(movement$change)))
  
  flow_in <- subset(movement, change > 0)
  flow_out <- subset(movement, change < 0)
  
  circle <- qglyphCircle(5)
  
  qdrawGlyph(painter, circle, flow_in$long, flow_in$lat, 
    stroke = "NA", fill = alpha("black", 0.5), cex = flow_in$size + 0.5)
  qdrawGlyph(painter, circle, flow_out$long, flow_out$lat, 
    stroke = "NA", fill = alpha("red", 0.5), cex = flow_out$size + 0.5)
}

if (exists("view")) view$close()

scene <- Qt$QGraphicsScene()
root <- qlayer(scene)
view <- qplotView(scene = scene)

borders <- qlayer(root, render_borders, 
  hoverMove = hover_county, mousePress = select_county, cache = T)
borders$setLimits(qrect(range(boundary$long), range(boundary$lat)))

highlight <- qlayer(root, render_highlight, clip = FALSE)
highlight$setLimits(borders$limits())

flow_layer <- qlayer(root, render_flow, cache = T)
flow_layer$setLimits(borders$limits())

print(view)

# How might we describe this with a grammar?
#
# geom_poly(boundaries, aes(long, lat, id = id), 
#   colour = "grey50", fill = NA) + brush("closest", "hover", "transient") +
#   geom_poly(subset = .(id == parent$id), fill = "grey80", colour = NA) +
#   geom_point(subset = .(id == parent$id), data = migration, 
#     aes(size = abs(n), colour = sign(n))) + 
#   scale_size(dynamic = TRUE)
#    
# Thoughts:
#   * need full inheritance tree, a la protovis
#   * brush needs to be associated with a layer?
#   * dynamic scales would not be constant over time, but would adapt to 
#     displayed data
