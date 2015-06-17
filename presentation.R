# Load neccessary library
library(data.table)
library(ggplot2)
library(rCharts)
library(ggvis)
library(reshape2)
library(dplyr)
library(mapproj)
library(maps)
library(ggthemes)

# Read the Weather dataset after aggregating
weatherData <- fread('data/StormData_to2015_after_aggregating.csv') %>% mutate(EVENT_TYPE = tolower(EVENT_TYPE))
weatherData <- data.table(weatherData)
weatherData$STATE_NAME <- tolower(weatherData$STATE_NAME)

# Remove unsupported states
dt <- weatherData[!weatherData$STATE_NAME %in% c("american samoa", "atlantic north", "atlantic south", "district of columbia",
                                     "e pacific", "guam", "gulf of alaska", "gulf of mexico", "hawaii waters", "lake erie", "lake huron", 
                                     "lake michigan", "lake ontario", "lake st clair", "lake superior", "puerto rico", "st lawrence r", 
                                     "virgin islands"),]
event_types <- sort(unique(dt$EVENT_TYPE))

aggregate_by_state <- function(dt, year_min, year_max, evtypes) {
  replace_na <- function(x) ifelse(is.na(x), 0, x)
  round_2 <- function(x) round(x, 2)
  
  states <- data.table(STATE_NAME=sort(unique(dt$STATE_NAME)))
  
  aggregated <- dt %>% filter(YEAR >= year_min, YEAR <= year_max, EVENT_TYPE %in% evtypes) %>%
    group_by(STATE_NAME) %>%
    summarise_each(funs(sum), COUNT:CROPS_DAMAGE)
  
  # We want all states to be present even if nothing happened
  left_join(states,  aggregated, by = "STATE_NAME") %>%
    mutate_each(funs(replace_na), FATALITIES:CROPS_DAMAGE) %>%
    mutate_each(funs(round_2), PROPERTY_DAMAGE, CROPS_DAMAGE)    
}

# Get damage of weather events by states
data <- aggregate_by_state(weatherData, 1950, 2015, event_types)
data$Affected <- data$FATALITIES + data$INJURIES

# Draw Map
states_map <- map_data("state")
fill <- "Affected"
year_min <- 1950
year_max <- 2015
title <- "Population Impact %d - %d (Number of Affected)"
title <- sprintf(title, year_min, year_max)
p <- ggplot(data, aes(map_id = STATE_NAME))
p <- p + geom_map(aes_string(fill = fill), map = states_map, colour='black')
p <- p + expand_limits(x = states_map$long, y = states_map$lat)
p <- p + coord_map() + theme_bw(base_size = 14)
p <- p + labs(x = "Long", y = "Lat", title = title)
p <- p + geom_point(x=5, y=5) + coord_fixed()
p + scale_fill_gradient(low = "#d3e992", high = "#669900")




