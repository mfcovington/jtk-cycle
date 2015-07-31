circtime.to.radians <- function(phases, periods) {
  2 * pi * phases / periods
}

radians.to.circtime <- function(angles) {
  hours <- angles * 12 / pi
  hours %% 24
}

circular.mean <- function(angles) {
  atan2(mean(sin(angles)), mean(cos(angles)))
}

radius <- function(angles, radians=TRUE) {
  sqrt(mean(sin(angles)) ^ 2 + mean(cos(angles)) ^ 2)
}

mean.circadian.phase <- function(phases, periods, min.radius=0.5) {
  angles <- circtime.to.radians(phases, periods)
  mean.angle <- circular.mean(angles)
  ifelse(radius(angles) > min.radius, radians.to.circtime(mean.angle), NA)
}
