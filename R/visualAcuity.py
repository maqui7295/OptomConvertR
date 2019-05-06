from math import pi

def cm_to_inches(cm):
  # 30.48cm = 12inches = 1ft
  inches <- cm/2.54
  return inches


def rad_to_deg(rad):
  deg = (180 * rad) / pi
  return deg

def get_va_dd(letter_height, distance):
  value =  letter_height/distance
  deg_angle = rad_to_deg(atan(value))
  min_of_arc = deg_angle * 60
  logMAR =log10(min_of_arc)
  return logMAR

