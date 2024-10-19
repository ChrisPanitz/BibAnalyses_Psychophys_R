# 1986
year <- 1986
results$TCperYear[results$Years == year]
nCrit <- 30
cbind(
  results$FirstAuthors[results$Years == year & results$TCperYear > nCrit],
  results$Years[results$Years == year & results$TCperYear > nCrit],
  df$TI[results$Years == year & results$TCperYear > nCrit],
  results$TCperYear[results$Years == year & results$TCperYear > nCrit]
)

# 1993
year <- 1993
results$TCperYear[results$Years == year]
nCrit <- 30
cbind(
  results$FirstAuthors[results$Years == year & results$TCperYear > nCrit],
  results$Years[results$Years == year & results$TCperYear > nCrit],
  df$TI[results$Years == year & results$TCperYear > nCrit],
  results$TCperYear[results$Years == year & results$TCperYear > nCrit]
)

# 2000
year <- 2000
results$TCperYear[results$Years == year]
nCrit <- 30
cbind(
  results$FirstAuthors[results$Years == year & results$TCperYear > nCrit],
  results$Years[results$Years == year & results$TCperYear > nCrit],
  df$TI[results$Years == year & results$TCperYear > nCrit],
  results$TCperYear[results$Years == year & results$TCperYear > nCrit]
)

# 2008
year <- 2008
results$TCperYear[results$Years == year]
nCrit <- 30
cbind(
  results$FirstAuthors[results$Years == year & results$TCperYear > nCrit],
  results$Years[results$Years == year & results$TCperYear > nCrit],
  df$TI[results$Years == year & results$TCperYear > nCrit],
  results$TCperYear[results$Years == year & results$TCperYear > nCrit]
)
