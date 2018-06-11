library(monzor)
library(ggmap)
library(dplyr)

transactions <- getTransactions(expand = "merchant")
locations = transactions$merchant$address
locations$amount = transactions$amount

mapsZoom <- 15

googMap <- get_googlemap("London, United Kingdom", zoom = mapsZoom)
bb <- attr(googMap, "bb")

locations <- locations %>%
    filter(!is.na(latitude)) %>%
    filter(
        latitude < bb$ur.lat,
        latitude > bb$ll.lat,
        longitude < bb$ur.lon,
        longitude > bb$ll.lon,
    )

transactionsMap <- get_stamenmap(bb2bbox(bb), maptype = "toner-lite", zoom = mapsZoom)
ggmap(transactionsMap) +
    geom_density2d(data = locations,
                   aes(x = longitude, y = latitude), size = 0.1) +
    stat_density2d(data = locations,
                   aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
                   size = 0.001, bins = 5, geom = "polygon") +
    scale_fill_gradient(low = "green", high = "red") +
    scale_alpha(range = c(0, 0.5), guide = FALSE)
