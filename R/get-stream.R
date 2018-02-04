getStream <- function(path2stream){
	myStreams <- GET("https://www.strava.com/", path = path2stream,
			 query = list(access_token = myToken, per_page = 200))
	myStreams <- content(myStreams, "text")
	myStreams <- fromJSON(myStreams)

	message(paste0("... get stream: ",path2stream))

	myList <- myStreams$data

	return(setNames(myList, myStreams$type))
}
