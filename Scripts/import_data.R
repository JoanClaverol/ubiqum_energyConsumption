
# libraries ----

if (!require(pacman)) {
  install.packages("pacman")  
}

pacman::p_load(RMySQL)


# connect to database & import database ----

# establishing the connection parameters
con <-  dbConnect(drv = MySQL(),
                  user = 'deepAnalytics',
                  password = 'Sqltask1234!',
                  dbname = 'dataanalytics2018',
                  host = 'data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# list the tables conected in the databsae
dbListTables(con)

# check the attributes inside a table
dbListFields(con, 'iris')

## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(conn = con, "SELECT * FROM iris")

## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(conn = con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

# storing the dataset information
energy_2006 <- dbGetQuery(conn = con, 'SELECT * FROM yr_2006')
energy_2007 <- dbGetQuery(conn = con, 'SELECT * FROM yr_2007')
energy_2008 <- dbGetQuery(conn = con, 'SELECT * FROM yr_2008')
energy_2009 <- dbGetQuery(conn = con, 'SELECT * FROM yr_2009')
energy_2010 <- dbGetQuery(conn = con, 'SELECT * FROM yr_2010')

# Unifying all the datasets  ----

# binding data
newDF <- bind_rows(energy_2006, energy_2007, energy_2008,
                   energy_2009, energy_2010)
