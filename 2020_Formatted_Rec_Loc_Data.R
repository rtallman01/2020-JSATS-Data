                                      # convert Receiver Location data to correct format #




library(tidyverse)

# Import 2020 Receiver Location Data --------------------------------------

Loc_dat <- read.csv ("2020_Receiver_Locations.csv") 


# Select specific 2020 Receiver Locations ---------------------------------


select_loc <- Loc_dat %>% select(
  ï..GeneralLocation,
  Location,
  Lat,
  Lon,
  Region,
  rkm,
  GenLat,
  GenLon,
  Genrkm,
  BankSide,
  DistNextUp,
  ChannelWidth,
  NearestBoatRamp,
  LocationNotes)


# Rename Columns with Appropriate Access Database Names -------------------

colnames(select_loc)

names(select_loc)[1] <- "General Location"
names(select_loc)[2] <- "GPS Names"
names(select_loc)[11] <- "Dist to next upstream location"
names(select_loc)[12] <- "Channel Width (m)"
names(select_loc)[13] <- "Nearest Boat Ramp"
names(select_loc)[14] <- "Location Notes"

# re-checking names to ensure they are the same as the database column labels

colnames(select_loc)



# Add additional columns required by access database ----------------------

# make a copy of the database you are working with

select_loc_1 <- select_loc

# add columns to the data frame using the mutate function
select_loc_1 <- mutate(select_loc_1, Attachment = NA)
select_loc_1 <- mutate(select_loc_1, Cable = NA)
select_loc_1 <- mutate(select_loc_1, Depth = NA)
select_loc_1 <- mutate(select_loc_1, RecDepth = NA)
select_loc_1 <- mutate(select_loc_1, CableStatus = NA)


# change new column names so that they reflect the acess database labels. The reason I could not rename them properly in the mutate function is because [R] got confused when I included parenthesis that had metrics. For example Weight (g)

colnames(select_loc_1)

names(select_loc_1)[16] <- "Cable(m)"
names(select_loc_1)[17] <- "Depth(m)"
names(select_loc_1)[17] <- "Depth(m)"
names(select_loc_1)[18] <- "RecDepth(m)"

str(select_loc_1)


# Write data frame to a csv to import into access --------------------------

write.csv(select_loc_1,"C:/Users/Rachelle Tallman/Documents/RProjects/2020_JSATS_Data\\2020_JSATS_Rec_Loc_Mod.csv", row.names = FALSE)

