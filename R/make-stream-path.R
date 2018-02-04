makeStreamPath <- function(activity.id,stream.type){
	my.path = paste0("api/v3/activities/", activity.id, "/streams/", stream.type)
	return(my.path)
}
