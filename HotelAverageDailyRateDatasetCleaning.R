# File to Clean data for Milestone 3

##################################################################################
# New Variable: season
# Description Seasons based on (https://www.calendardate.com/) in format (MM-DD-YYYY)
##################################################################################

# 2015 - Winter (12-21-2014) - (03-20-2015)"2007-06-22"
winter2015Start <- as.Date("2014-12-21")
winter2015End   <- as.Date("2015-03-20")

# 2015 - Spring (03-20-2015) - (06-21-2015)
spring2015Start <- as.Date("2015-03-20")
spring2015End   <- as.Date("2015-06-21")

# 2015 - Summer (06-21-2015) - (09-23-2015)
summer2015Start <- as.Date("2015-06-21")
summer2015End   <- as.Date("2015-09-23")

# 2015 - Fall   (09-23-2015) - (12-22-2015)
fall2015Start <- as.Date("2015-09-23")
fall2015End   <- as.Date("2015-12-22")

# 2016 - Winter (12-22-2015) - (03-20-2016)
winter2016Start <- as.Date("2015-12-22")
winter2016End   <- as.Date("2016-03-20")

# 2016 - Spring (03-20-2016) - (06-20-2016)
spring2016Start <- as.Date("2016-03-20")
spring2016End   <- as.Date("2016-06-20")

# 2016 - Summer (06-20-2016) - (09-22-2016)
summer2016Start <- as.Date("2016-06-20")
summer2016End  <- as.Date("2016-09-22")

# 2016 - Fall   (09-22-2016) - (12-21-2016)
fall2016Start <- as.Date("2016-09-22")
fall2016End  <- as.Date("2016-12-21")

# 2017 - Winter (12-21-2016) - (03-20-2017)
winter2017Start <- as.Date("2016-12-21")
winter2017End   <- as.Date("2017-03-20")

# 2017 - Spring (03-20-2017) - (06-21-2017)
spring2017Start <- as.Date("2017-03-20")
spring2017End   <- as.Date("2017-06-21")

# 2017 - Summer (06-21-2017) - (09-22-2017)
summer2017Start <- as.Date("2017-06-21")
summer2017End   <- as.Date("2017-09-22")

# 2017 - Fall   (09-22-2017) - (12-21-2017)
fall2017Start <- as.Date("2017-09-22")
fall2017End   <- as.Date("2017-12-21")

# convertDate(Date) returns the Season in which the Date falls falls based on the above variables
convertDateToSeason <- function(providedDate) {
  if((providedDate >= winter2015Start & providedDate <= winter2015End) | (providedDate >= winter2016Start & providedDate <= winter2016End) | (providedDate >= winter2017Start & providedDate <= winter2017End) ) {
    result <- 'winter'
  }
  else if((providedDate >= spring2015Start & providedDate <= spring2015End) | (providedDate >= spring2016Start & providedDate <= spring2016End) | (providedDate >= spring2017Start & providedDate <= spring2017End)  ){
    result <- 'spring'
  }
  else if((providedDate >= summer2015Start & providedDate <= summer2015End) | (providedDate >= summer2016Start & providedDate <= summer2016End) | (providedDate >= summer2017Start & providedDate <= summer2017End)) {
    result <- 'summer'
  }
  else if((providedDate >= fall2015Start & providedDate <= fall2015End) | (providedDate >= fall2016Start & providedDate <= fall2016End) | (providedDate >= fall2017Start & providedDate <= fall2017End) ) {
    result <- 'fall'
  }
  else {
    result <- 'UNDEFINED'
  }
  return(result)
}

# Step 1 - Convert Values (arrival_date_month, arrival_date_day_of_month, arrival_date_year) to DATE FORMAT
MonthDay <- paste(hotel_bookings$arrival_date_month, hotel_bookings$arrival_date_day_of_month, sep=" ", collapse = NULL) # concat ("Month Day")
MonthDayYear <- paste(MonthDay, hotel_bookings$arrival_date_year, sep=", ", collapse=NULL) # concat ("Month Day, Year")
convertedDate <- as.Date(MonthDayYear,format='%B %d, %Y') # Convert to date format

# Step 2 - create new column on hotel_bookings called 'Season' and input 'UNDEFINED' for the value
hotel_bookings$season <- "UNDEFINED"
hotel_bookings$season

# Step 3 - loop through each Element and Assign the hotel
x <- 1
for(i in convertedDate) {
  hotel_bookings$season[[x]] <- convertDateToSeason(i)
  x <- x + 1
}

hotel_bookings$season

##################################################################################
# New Variable: countryOfOriginInPortugal
# Represents if the traveler's country of Origin is in Portugal (1) or outside of Portugal (0)
##################################################################################
# Step 1 - Create new column on hotel_bookings called 'CountryOfOriginInPortugal' and input 'UNDEFINED' for the value
hotel_bookings$countryOfOriginInPortugal <- 0

# Step 2 - Conditional to set the value of 1 to Travelers whose country of Origin is Portugal and 0 if they are outside of Portugal
hotel_bookings$countryOfOriginInPortugal <- ifelse(hotel_bookings$country == 'PRT', 1, 0)
hotel_bookings$countryOfOriginInPortugal


##################################################################################
# New Variable: reservedRoomAndAssignedRoomMatch
# Reprents if the Reserved Room and the AssigendRoom fields contain the same value
##################################################################################
# Step 1 - Create new column on hotel_bookings called 'CountryOfOriginInPortugal' and input 'UNDEFINED' for the value
hotel_bookings$reservedRoomAndAssignedRoomMatch <- 0

# Step 2 - Conditional to set the value of 1 to Travelers whose country of Origin is Portugal and 0 if they are outside of Portugal
hotel_bookings$reservedRoomAndAssignedRoomMatch <- ifelse(hotel_bookings$reserved_room_type == hotel_bookings$assigned_room_type, 1, 0)

hotel_bookings$reservedRoomAndAssignedRoomMatch


################################################################################
# Clean Existing Meals
# Combine SC and UNDEFINED as they are refer to the same value
################################################################################
hotel_bookings$meal <- ifelse(hotel_bookings$meal == 'UNDEFINED', 'SC', hotel_bookings$meal)

# Validating the update
#for(i in hotel_bookings$meal ) {
#  if(i == 'UNDEFINED') {
#    print(i)
#  }
#}
summary(hotel_bookings$meal)

hotel_bookings$meal
##################################################################################
# Data Omitting
# market_segment. Complementary and Undefined values
# TEST

##################################################################################
summary(hotel_bookings$market_segment)
