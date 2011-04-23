options(stringsAsFactors = FALSE)

inflow <- read.csv("inflow.csv.bz2")
outflow <- read.csv("outflow.csv.bz2")

clean_flow <- function(flow) {
  names(flow) <- c("state_from", "county_from", "state_to", "county_to", "state_name", "county_name", "returns", "exempt", "income")
  flow <- subset(flow, county_from != 0 & state_from != 0 & state_to <= 56)
  flow$n <- flow$returns + flow$exempt
  flow <- flow[c("state_from", "county_from", "state_to", "county_to", "n")]
  rownames(flow) <- NULL
  flow
}

inflow <- clean_flow(inflow)
names(inflow)[5] <- "in"
outflow <- clean_flow(outflow)
names(outflow)[5] <- "out"

flow <- merge(inflow, outflow, all = T, 
  by = c("state_from", "county_from", "state_to", "county_to"))
flow$`in`[is.na(flow$`in`)] <- 0
flow$out[is.na(flow$out)] <- 0
flow$change <- flow$`in` - flow$out

total <- subset(flow, state_from == state_to & county_from == county_to)
total <- total[c("state_from", "county_from", "in")]
names(total) <- c("state", "county", "pop")

flow <- subset(flow, !(state_from == state_to & county_from == county_to))

write.table(flow, "flow.csv", sep = ",", row = F)
write.table(total, "total.csv", sep = ",", row = F)