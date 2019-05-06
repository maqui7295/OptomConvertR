cm_to_inches <- function(cm) {
  # 30.48cm = 12inches = 1ft
  inches <- cm/2.54
  return(inches)
}

rad_to_deg <- function(rad){
  deg <- (180 * rad) / pi
  return(deg)
}
