# Load package
library(dplyr)

# Functions to simulate games
computeLog5 <- function(wp.x, wp.y){
  (wp.x - wp.x * wp.y) / (wp.x + wp.y - 2 * wp.x * wp.y)
}
safeifelse <- function(test, yes, no){
  no[test] <- yes[test]
  return(no)
}
simulateGames <- function(wp.x, wp.y, team.x, team.y){
  odds.x <- computeLog5(wp.x, wp.y)
  random.numbers <- runif(length(odds.x))
  winner <- safeifelse(random.numbers < odds.x, team.x, team.y)
  return(winner)
}
simulateSeason <- function(fixed.wp, teams = teams.sim, schedule = schedule.sim){
  # Set teams winning percentages
  if (fixed.wp){
    teams$wp <- teams$exp.wp
  } else {
    num.teams <- nrow(teams)
    teams$wp <- rnorm(n = num.teams, mean = teams$exp.wp, teams$wp.sd)
  }
  
  # Add winning percentage to schedule
  schedule.info <- inner_join(schedule, teams, by = c("team.x" = "team"))
  schedule.info <- inner_join(schedule.info, teams, by = c("team.y" = "team"))
  
  # Simulate games
  with(schedule.info, simulateGames(wp.x, wp.y, team.x, team.y))
}

# Function to summarize simulations
summarizeCounts <- function(x){
  c(mean = mean(x), sd = sd(x))
}
summarizeSimulations <- function(simulations){
  win.counts <- sapply(simulations, table)
  win.summaries <- apply(win.counts, 1, summarizeCounts)
  most.wins.count <- table(apply(win.counts, 2, which.max))
  most.wins.perc <- as.numeric(most.wins.count / length(simulations))
  return(data.frame(t(win.summaries), most.wins.perc))
}

# Set teams expected win percentage and variance
teams.sim <- data.frame(team = c("CHC", "DET", "PHI"),
                    exp.wp = c(0.55, 0.5, 0.45),
                    wp.sd = c(0.05, 0.15, 0.05))
# Set schedules
schedule.sim <- data.frame(team.x = teams.sim$team,
                           team.y = teams.sim$team[c(2:(nrow(teams.sim)), 1)])
schedule.sim <- schedule.sim[rep(1:nrow(schedule.sim), 10), ]

# Run simulations
fixed.simulations <- replicate(10000, simulateSeason(fixed.wp = TRUE), simplify = FALSE)
non.fixed.simulations <- replicate(10000, simulateSeason(fixed.wp = FALSE), simplify = FALSE)

# Summarize simulations
summarizeSimulations(fixed.simulations)
summarizeSimulations(non.fixed.simulations)
