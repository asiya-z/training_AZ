#functions
#' Convert temperature values from Fahrenheit to Celsius
#'
#' @param fahr Numeric or numeric vector in degrees Fahrenheit
#' 
#' @return Numeric or numeric vector in degrees Celsius
#' @export
#' 
#' @examples
#' fahr_to_celsius(32)
#' fahr_to_celsius(c(32, 212, 72))


airtemps <- c(212, 30.3, 78, 32)
airtemps_c <- c(100,-0.94, 25.6, 0)


celsius1 <- (airtemps[1] - 32) * 5/9
celsius2 <- (airtemps[2] - 32) * 5/9
celsius3 <- (airtemps[3] - 32) * 5/9

fahr_to_celsius <- function(fahr) {
  celsius <- (fahr - 32) * 5/9
  return(celsius)
}


if (is.null(celsius)){print("No num")}

celsius4 <- fahr_to_celsius(airtemps)
celsius4

celsius_to_fahr <- function(celsius) {
  fahr <-  celsius * 9/5 + 32
  return(fahr)
}

fahr1 <- celsius_to_fahr(airtempsF)
fahr1

airtemps_fahr <- celsius_to_fahr(celsius = airtemps_c)
airtemps == airtemps_fahr

############################

convert_temps <- function(fahr) {
  celsius <- (fahr - 32) * 5/9
  kelvin <- celsius + 273.15
  return(data.frame(fahr = fahr, celsius = celsius, kelvin = kelvin))
}

temps_df <- data.frame(convert_temps(seq(-100,100,10)))

##### custom theme #######################
library(ggplot2)
source("custom_theme.R")

ggplot(temps_df, mapping = aes(x = fahr, y = celsius, color = kelvin)) +
  geom_point() +
  custom_theme(10)

scatterplot <- function(df, point_size = 2, font_size = 9) {
  ggplot(df, mapping = aes(x = fahr, y = celsius, color = kelvin)) +
    geom_point(size = point_size) +
    custom_theme(font_size)
}

scatterplot(temps_df, point_size = 3, font_size = 16)


