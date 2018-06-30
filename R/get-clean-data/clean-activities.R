#' cleanActivities
#'
#' a function to clean strava actities
#'
#'
#' @param activities json activities returnd by strava
#' @param myGearList
#'
#' @return
#' @export
#'
#' @examples
cleanActivities <- function(activities, myGearList){

	## Convert json file to data.frame
	## --------------------------------
	activities <- fromJSON(activities)

	## clean activities.
	## ------------------
	## deal with nested data.frame start_latlng
	start_latlng <- rbindlist(lapply(activities$start_latlng, as.data.frame.list))
	end_latlng <- rbindlist(lapply(activities$end_latlng, as.data.frame.list))
	map <- rbindlist(lapply(activities$map, as.data.frame.list))

	## Remove non necessary features
	activities <- select(activities,
			     -start_latlng, -end_latlng, -map, -resource_state, -athlete,
			     -location_city, -location_state, -location_country, -from_accepted_tag)

	## Convert to data.table
	activities <- as.data.table(activities)

	## Some renames
	activities <- activities %>% rename(workout.type.id = workout_type,
					    moving.time.sec = moving_time,
					    elapsed.time.sec = elapsed_time,
					    total.elevation.gain.m = total_elevation_gain,
					    activity.id = id,
					    external.activity.id = external_id,
					    upload.id = upload_id,
					    start.date = start_date,
					    start.date.local = start_date_local,
					    utc.offset = utc_offset,
					    achievement.count = achievement_count,
					    kudos.count = kudos_count,
					    comment.count = comment_count,
					    athlete.count = athlete_count,
					    photo.count = photo_count,
					    gear.id = gear_id,
					    average.speed.m.per.s = average_speed,
					    max.speed.m.per.s = max_speed,
					    average.temperature = average_temp,
					    average.power.watts = average_watts,
					    average.power.kilojoules = kilojoules,
					    device.watts = device_watts,
					    has.heartrate.data = has_heartrate,
					    average.heartrate = average_heartrate,
					    max.heartrate = max_heartrate,
					    elevation.high.m = elev_high,
					    elevation.low.m = elev_low,
					    personal.records.count = pr_count,
					    total.photo.count = total_photo_count,
					    has.kudoed = has_kudoed,
					    suffer.score = suffer_score,
					    average.cadence = average_cadence
	)


	## Add back the data only for activit with start lat long
	activities[ !is.na(start_latitude) & !is.na(start_longitude), ':=' (
		start.latitude = unlist(start_latlng[ , 1]),
		start.longitude = unlist(start_latlng[ , 2]),
		end.latitude = unlist(end_latlng[ , 1]),
		end.longitude = unlist(end_latlng[ , 2])
	)]
	## remove duplicate
	activities <- select(activities, -start_latitude, -start_longitude)


	## Get names of workout types
	## Run / Ride supported
	activities[ , workout.type := factor(levels = c("Ride.Relax","Ride.Workout","Ride.Race",
							"Run.Relax","Run.Workout","Run.Long.Run","Run.Race",
							NA))]
	activities[ workout.type.id == 10, workout.type := "Ride.Relax"]
	activities[ workout.type.id == 11, workout.type := "Ride.Race"]
	activities[ workout.type.id == 12, workout.type := "Ride.Workout"]
	activities[ workout.type.id == 0, workout.type := "Run.Relax"]
	activities[ workout.type.id == 1, workout.type := "Run.Race"]
	activities[ workout.type.id == 2, workout.type := "Run.Long.Run"]
	activities[ workout.type.id == 3, workout.type := "Run.Workout"]
	activities[ is.na(workout.type.id), workout.type := NA]

	## Add gear name
	activities[ , gear.name := unlist(lapply(activities$gear.id, getGearName, myGearList))]

	## Convert Dates
	activities[ , start.date := strptime(x = start.date, format = "%Y-%m-%dT%H:%M:%SZ")]
	activities[ , start.date.local := strptime(x = start.date.local, format = "%Y-%m-%dT%H:%M:%SZ")]



	return(activities)
}
