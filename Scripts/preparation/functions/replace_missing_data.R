# -------------------------------------------------------------------------
# GOAL: function to deal with missing values
# DESCRIPTION: replace values from the ones of one week before 
# -------------------------------------------------------------------------

na_imputation <- function(vector) {
  for (i in seq_along(vector)) {
    if (is.na(vector[i])) {
      vector[i] <- vector[i-60*24*7] # 60 minutes * 24 hours * 7 days
    }
  }
  return(vector)
}
