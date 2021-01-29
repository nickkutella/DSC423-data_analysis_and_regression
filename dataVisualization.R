### Paste plots and data visualzations here ###

#Randomly take 1000 samples of the hotel booking dataset to streamline ploting function
hotel_bookings_sample <- hotel_bookings[sample(1:nrow(hotel_bookings), 1000, replace=FALSE),]

#Plot of Hotel Average Daily Rate by Season
plot(hotel_bookings_sample$season, hotel_bookings_sample$adr, main="Hotel Bookings ADR by Season", xlab="Season", ylab="ADR")


# Separate blocks for easy reading and please ensure you comment your code
