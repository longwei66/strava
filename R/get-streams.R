
getStreams <- function(activity.id, stream.type = streams.type){

	## debug
	#activity.id = "1377378518"
	#stream.type = streams.type

	## generate the path
	myPath <- makeStreamPath(activity.id = activity.id, stream.type = streams.type)
	message(paste0("Get streams for activity: ", activity.id))

	## Get all the associated streams
	activity.streams <- setNames(object = lapply(myPath, getStream),
				     nm = streams.type)


	activity.streamsDT <- data.table(
		time = activity.streams$time$time,
		distance = activity.streams$distance$distance,
		lat = activity.streams$latlng$latlng[,2],
		lng = activity.streams$latlng$latlng[,1],
		altitude = activity.streams$altitude$altitude,
		velocity_smooth = activity.streams$velocity_smooth$velocity_smooth,
		heartrate = activity.streams$heartrate$heartrate,
		temp = activity.streams$temp$temp,
		moving = activity.streams$moving$moving,
		grade_smooth = activity.streams$grade_smooth$grade_smooth
	)

	activity.streamsDT[ , point.id := 1:nrow(activity.streamsDT)]
	activity.streamsDT[ , activity.id := activity.id]

	return(activity.streamsDT)
}
