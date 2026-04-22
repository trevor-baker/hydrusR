#' Generates hourly PET inputs from provided daily value.
#'
#' @param pet.daily numeric vector of daily PET values in same length units as project. i.e., must convert mm PET to cm if your project is in cm.
#' @param hours hours of day that will have non-zero ET. numeric vector, not a range. i.e., 6:20 not c(6,20)
#' @export
#' @examples
#' pet.val <- pet.hourly(pet.daily = c(1,2,4), hours = 6:20)
#' plot(pet.val) #show the daily totoals broken into hourly with diurnal variation
#' sum(pet.val$pet[1:24]) #sum = 1 on day 1
#' sum(pet.val$pet[25:48]) #sum = 2 on day 2
#' sum(pet.val$pet[49:72]) #sum = 4 on day 3
pet.hourly<- function(pet.daily, hours = 6:20) {

  cat("pet.hourly() assigns same day length for every days - not seasonal.\n")

  et_hours = length(hours)
  thours = 1:et_hours
  zero_hours = 24 - length(hours)
  morning = min(hours) - 1
  eve = 24 - max(hours)
  morn_hours = numeric(morning)
  eve_hours = numeric(eve)
  Tcycle = length(thours)

  pet.hourly<- as.vector(rep(0, length(pet.daily)*Tcycle))
  Tday = as.vector(rep(thours, length(pet.daily)))
  X<- rep(Tcycle, length(pet.daily))
  pet.daily.ext<- rep(pet.daily, X)

  for(i in 1: length(pet.hourly)) {
        pet.hourly[i] = pet.daily.ext[i]/Tcycle*(1 + sin(2*pi*Tday[i]/Tcycle - pi/2))
  }
  et_hourly = data.frame(hour = rep(hours, length(pet.daily)),
                         pet = pet.hourly)

  et_24hour = data.frame(hour = rep(0:23, length(pet.daily)),
                         pet = numeric(length(pet.daily)))
  hour_ind = which(et_24hour$hour %in% et_hourly$hour)
  et_24hour[hour_ind, "pet"] = pet.hourly

  return(et_24hour)
}
