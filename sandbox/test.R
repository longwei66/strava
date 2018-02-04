## =============================================================================
## 	Load libraries
## =============================================================================
library(httr)
library(httpuv)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(data.table)
library(rgbif)
library(plotly)


## =============================================================================
## 	Id, Tokens and API keys
## =============================================================================
source("./conf/keys.R")


## =============================================================================
## 	Load useful functions
## =============================================================================
source("./conf/streams-type.R")
source("./R/decode-polyline.R")
source("./R/make-stream-path.R")
source("./R/get-stream.R")
source("./R/get-streams.R")





## =============================================================================
## 	Authentification
## =============================================================================
my_app <- oauth_app("strava", key = myId, secret = mySecret)

my_endpoint <- oauth_endpoint(
	request = NULL,
	authorize = "https://www.strava.com/oauth/authorize",
	access = "https://www.strava.com/oauth/token"
)

sig <- oauth2.0_token(my_endpoint, my_app, scope = "view_private",
		      type = NULL, use_oob = FALSE, as_header = FALSE,
		      use_basic_auth = FALSE, cache = FALSE)



## =============================================================================
## 	Get Activities
## =============================================================================
activities <- GET("https://www.strava.com/", path = "api/v3/activities",
		  query = list(access_token = myToken, per_page = 200))
activities <- content(activities, "text")
activities <- fromJSON(activities)
## Only rides since last october
activities <- activities[ activities$type == "Ride" &
			  	activities$start_date_local > "2017-10-01",]





## =============================================================================
## 	Get Streams
## =============================================================================
## Get all streams paths from all activities
allStreams <- lapply(X = unique(activities$id),FUN = getStreams)
allStreams <- rbindlist(allStreams, use.names = TRUE)


allStreams.long <- melt(data = allStreams,
			id.vars = c("activity.id", "point.id","distance") )


w <- ggplot(data = allStreams.long)
w <- w + geom_path(aes(x = distance, y = value))
w <- w + facet_grid(facets =  variable ~ activity.id, scales = "free_y")
w


w <- ggplot(data = allStreams)
w <- w + geom_path(aes(x = lat, y = lng, col = velocity_smooth * 3.6), size = 1.8)
w <- w + scale_colour_gradient2(low = "blue", mid = "green", high = "red",
				midpoint = 10)
w <- w + scale_size_area() + coord_map() + facet_grid(facets = . ~ activity.id)
w


## Plotly trial
p <- plot_ly(data = streamsDT,
	     x = ~lat, y = ~lng, z = ~heartrate,
	     type = 'scatter3d', mode = 'lines',
	     opacity = 1,
	     line = list(width = 6, color = ~velocity_smooth, reverscale = FALSE))




## =============================================================================
## 	Plot activities from summary
## =============================================================================


## Function to convert a google path to coordinnates
getPath <- function(maps,id){
	s <- maps[maps$id == id,]$summary_polyline
	r <- decodePolyline(s)
	r$id <- id
	r$coord_id <- 1:nrow(r)
	return(r)
}

myList <- lapply(X = activities$map$id, FUN = , getPath, maps = activities$map)
myData <- rbindlist(myList)

myData[ , elevation := rgbif::elevation(latitude = lat,
					longitude = lon,
					key = ggElevationKey)$elevation ]


h <- ggplot(data = myData)
h <- h + geom_path(aes(x = lon, y = lat, group = id, col = elevation),
		   alpha = 0.7, size = 0.7, lineend = "round")
h <- h + scale_colour_gradient2(low = "blue", mid = "green", high = "red",
				midpoint = 120)
h <- h + coord_map()
h <- h + facet_wrap( ~ id)
h


p <- ggplot(data = myData, aes( x = coord_id, y = elevation))
p <- p + geom_path(aes(col = elevation, group = id))
p <- p + scale_colour_gradient2(low = "blue", mid = "green", high = "red",
				midpoint = 120)
p <- p + facet_wrap( ~ id)
p










## =============================================================================
## 	Get Atlethe
## =============================================================================
athlete <- GET("https://www.strava.com/", path = "api/v3/athlete",
	       query = list(access_token = myToken, per_page = 200))
athlete <- content(athlete, "text")
athlete <- fromJSON(athlete)





activity <- GET("https://www.strava.com/", path = "api/v3/activities/1377378518",
		query = list(access_token = myToken, per_page = 200))
activity <- content(activity, "text")
activity <- fromJSON(activity)
map <- decodePolyline(activity$map$polyline)
