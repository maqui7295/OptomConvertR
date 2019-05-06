source("R/metricConversions.R")
# devtools::load_all()

MIN_ANGLE_RESOLUTION <- 5
DEFAULT_FEET <- 20
DEFAULT_METERS <- 6


#' @title visual_demand
#' @description Calculates visual demand or threshold for a letter
#' @details visual_demand calculates the minimum va required to view a letter of a given size at a given distance
#' @aliases visual_demand
#' @author Mark Edosa
#' @export visual_demand
#' @param letter_height height or size of the letter. This should be in the same unit as the distance
#' @param distance distance from the viewer to the letter, should be in the same unit as the letter.
#' @param meter if TRUE, return the visual demand (as a string) in snellen meters
#' @param feet if TRUE, return the visual demand (as a string) in snellen feet
#' @return visual acuity in log MAR or snellen or both
#' @examples
#' visual_demand(1.5, 110) # visual demand in log MAR for a letter size of 1.5 inches at a distance of 110 inches
#' visual_demand(0.005, 6, all = TRUE) # visual demand (in logMAR and snellen) for a 5mm letter viewed at 6 meters
#' visual_demand(0.005, 6, meter = TRUE) # visual demand (in snellen meters) for a 5mm letter viewed at 6 meters
#' visual_demand(0.005, 6, feet = TRUE) # visual demand (in snellen feet) for a 5mm letter viewed at 6 meters
#' visual_demand(0.005, 6, meter= TRUE, feet = TRUE) # visual demand (in both snellen meter and feet) for a 5mm letter viewed at 6 meters
visual_demand <- function(letter_height, distance, meter = FALSE,
                          feet = FALSE, all = FALSE){

  arc <- min_arc(letter_height, distance)

  va_ft <- (DEFAULT_FEET * arc) / MIN_ANGLE_RESOLUTION
  va_mt <- (DEFAULT_METERS * arc) / MIN_ANGLE_RESOLUTION

  snellen_ft <- paste0(DEFAULT_FEET, "/", round(va_ft))
  snellen_mt <- paste0(DEFAULT_METERS, "/", round(va_mt))

  logmar <- log10(va_ft/DEFAULT_FEET)

  if(meter && !feet) return(snellen_mt)

  if(!meter && feet) return(snellen_ft)

  if(meter && feet) return(list(snellen_ft = snellen_ft, snellen_mt = snellen_mt))

  if(all) return(list(logmar = logmar, snellen_ft = snellen_ft, snellen_mt = snellen_mt))

  return(logmar)

}


#' @title min_arc
#' @description  Determine the minimum angle of resolution
#' @details  A function that calculates the minimum angle of resolution of a letter at a particular distance
#' @aliases min_arc
#' @author Mark Edosa
#' @export min_arc
#' @param letter_height height or size of the letter. This should be in the same unit as the distance
#' @param distance distance from the viewer to the letter. This should be in the same unit as the letter.
#' @return Minimum angle of resolution
#' @examples
#' min_arc(1.5, 110)
#' min_arc(0.005, 6)
min_arc <- function(letter_height, distance){

  message("To get accurate results letter height and distance should be of same unit")

  if(missing(letter_height) || missing(distance)){
    stop("Please provide the letter height/size and the distance")
  }

  if(is.null(letter_height) || is.null(distance)){
    stop("Letter and the distance should not be null")
  }

  if(!is.numeric(letter_height) && !is.numeric(distance)){
    stop("Letter height and distance should be numeric values")
  }

  deg_angle <-  rad_to_deg(atan2(letter_height, distance))
  min_of_arc <- deg_angle * 60
  return(min_of_arc)

}



#' @title logmar_to_snellen
#' @description Convert log MAR visual acuity to Snellen Visual acuity
#' @aliases logmar_to_snellen
#' @author Mark Edosa
#' @export logmar_to_snellen
#' @param logmar visual acuity in log MAR (should be a numeric value)
#' @param ft if TRUE, return the visual demand (as a string) in snellen feet
#' @return visual acuity in snellen meters (default) or feet
#' @examples
#' logmar_to_snellen(1) # 6/60
#' logmar_to_snellen(1, ft = TRUE) # 20/200
logmar_to_snellen <- function(logmar, ft=FALSE){

  if(!is.numeric(logmar)){
    stop("logmar must be a numeric value")
  }

  snell <- 10^logmar

  if(ft){
    return(paste0(DEFAULT_FEET, '/', round(snell * DEFAULT_FEET)))
  }

  return(paste0(DEFAULT_METERS, '/', round(snell* DEFAULT_METERS)))

}


#' @title snellen_to_logmar
#' @description  Convert Snellen visual acuity to log MAR Visual acuity
#' @details This functions log MAR acuity (numeric) from snellen visual acuity (string or numeric)
#' @aliases snellen_to_logmar
#' @author Mark Edosa
#' @import stringr
#' @export logmar_to_snellen
#' @param snellen snellen visual acuity in meters or feet
#' @return log MAR visual acuity
#' @examples
#' snellen_to_logmar("6/60") # 1
#' snellen_to_logmar("6/9") # 0.1760913
#' snellen_to_logmar("20/40") # 0.30103
#' snellen_to_logmar(1) # 0
snellen_to_logmar <- function(snellen){

  UseMethod("snellen_to_logmar")

}


snellen_to_logmar.default <- function(snellen){

  if(missing(snellen) || is.null(snellen)) {
    warning("Please provide a valid value")
    return(NA)
  }

  if(snellen == 0 || snellen < 0) {
    warning("Is there a minus sign in any of the snellen number?")
    warning("The snellen numbers when divided should not equal zero?")

    message("NA is returned")
    return(NA)
  }

  return(snellen)
}


snellen_to_logmar.numeric <- function(snellen){

  val <- snellen_to_logmar.default(snellen)

  if(is.na(val)) return(val)

  return(log10(val))

}


snellen_to_logmar.character <- function(snellen){
  if(snellen == ""){
    warning("an empty string was supplied, returning NA")
    return(NA)
  }

  # the string matches snellen va format
  if(grepl("\\d{1,}/\\d{1,}", snellen)){

    va <- as.integer(strsplit(snellen, "/")[[1]])

    # reciprocal of the visual acuity
    recP <- va[2]/va[1]

    val <- snellen_to_logmar.default(recP)

    if(is.na(val)) return(val)

    return(log10(recP))

  } else {

    warning("The snellen value should be in the following format: x/y e.g 6/6. NA is returned")

    return(NA)
  }


}



# TODO: move this function to a different file
sphe_equiv <- function(sph, cyl){
    se <- sph + (cyl/2)
    return(se)
}


