#' getGearName
#'
#' using a gear list database
#' return gear name from a list of gear id
#'
#' @param x
#' @param gearList
#'
#' @return
#' @export
#'
#' @examples
getGearName <- function(x,gearList){
	if(!is.na(x)){
		test <- (gearList$id == x)
		if(sum(test) > 0) {
			return(myGear$gear.name[test])
		} else {
			return(NA)
		}} else {
			return(NA)
		}
}
