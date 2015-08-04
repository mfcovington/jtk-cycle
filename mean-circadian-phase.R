circtime.to.radians <- function(phases, periods) {
  2 * pi * phases / periods
}

radians.to.circtime <- function(angles, periods=NULL) {
  hours <- angles * 12 / pi
  hours <- hours %% 24
  ifelse(is.null(periods), hours, hours * mean(periods) / 24)
}

circular.mean <- function(angles) {
  atan2(mean(sin(angles)), mean(cos(angles)))
}

radius <- function(angles, radians=TRUE) {
  if (radians == FALSE) {
    angles <- angles * 2 * pi / 360
  }
  sqrt(mean(sin(angles)) ^ 2 + mean(cos(angles)) ^ 2)
}

mean.circadian.phase <- function(phases, periods, min.radius=0.5, circ.time=TRUE) {
  angles <- circtime.to.radians(phases, periods)
  mean.angle <- circular.mean(angles)

  if (radius(angles) > min.radius) {
    radians.to.circtime(mean.angle, if (circ.time == FALSE) periods)
  } else {
    NA
  }
}
