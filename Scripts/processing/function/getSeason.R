# -------------------------------------------------------------------------
# GOAL: find seasons in the data
# DESCRIPTION: based on dates, help us to extract the seasons:
#               - 2008-12-21 Winter Solstice
#               - 2008-3-20 Spring Equinox
#               - 2008-6-21 Summer Solstice
#               - 2008-9-22 Fall Equinox
# -------------------------------------------------------------------------
getSeason <- function(date) {
  WS <- as.Date("2008-12-21", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2008-3-20",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2008-6-21",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2008-9-22",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(date, format="2008-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}


