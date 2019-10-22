# -------------------------------------------------------------------------
# GOAL: find the moment of the day based on the hour
# DESCRIPTION: help us to find the moment of the day the observation is placed:
#               - Winter: 0:00 to 6:00
#               - Morning: 6:00 to 12:00
#               - Midday: 12:00 to 18:00 
#               - Evening: 18:00 to 0:00
# -------------------------------------------------------------------------

getPeriodTime <- function(time) {
  time <- hour(time)
  ifelse (time >= 0 & time < 6, "Night",
          ifelse (time >= 6 & time < 12, "Morning",
                  ifelse(time >= 12 & time < 18, "Midday","Evening")))
}