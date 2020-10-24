install.packages("maps")
library(maps)


map(database = "state")
c=c("wisconsin", "pennsylvania","nevada","oregon","new york","Massachusetts","Ohio", "Washington",
    "minnesota", "Tennessee", "Kentucky", "Florida", "vermont", "california")

map(database = "state",
    regions = c,
    col = "mediumpurple",
    fill=T,
    add=TRUE)
save.image()

