g <- ggplot(data = my.activities[type == "Ride" & start.date > "2017-03-01"])
g <- g + geom_point(aes(x = average.speed.m.per.s * 3.6,
			y = distance,
			size = total.elevation.gain.m,
			shape = gear.id,
			col = average.heartrate), alpha = 0.6)
g



g <- ggplot(data = my.activities[type == "Ride" & start.date > "2017-03-01"])
g <- g + geom_point(aes(x = as.Date(start.date.local),
			y = average.heartrate,
			size = distance,
			#shape = ,
			col = total.elevation.gain.m), alpha = 0.6)
g <- g + scale_colour_gradient2(low = "blue", mid = "green",
				high = "red", midpoint = 300, space = "Lab",
				na.value = "grey50", guide = "colourbar")
g <- g + facet_grid(facets = as.factor(workout.type) ~ commute)
g





g <- ggplot(data = my.activities[type == "Ride" & start.date > "2017-03-01"])
g <- g + geom_point(aes(x = average.power.watts,
			y =  distance,#average_heartrate,
			size = moving.time.sec,
			#shape = ,
			col = as.numeric(as.Date(start.date.local))), alpha = 0.6)
g <- g + scale_colour_gradient2(low = "blue", mid = "green",
				high = "red", midpoint = 17575, space = "Lab",
				na.value = "grey50", guide = "colourbar")
g <- g + facet_grid(facets = as.factor(workout.type) ~ commute)
g






