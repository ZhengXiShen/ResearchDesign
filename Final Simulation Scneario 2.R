#Scenario 2: An Expected Effect for the study

set.seed(seed = 329)

n <- 200
rc.dat2 <- data.table(Group = c(rep.int(x = "Treatment",
                                        times = n/2), rep.int(x = "Control", 
                                                              times = n/2)))
rc.dat2[Group == "Control", rc :=round(x = rnorm(n = n/2, mean = 3,
                                                 sd = 1))]
rc.dat2[Group == "Treatment", rc :=round(x = rnorm(n = n/2,
                                                   mean = 7, sd = 1))]
data = rc.dat2
# data1 = rc.dat1 %>%
#   group_by(Group) %>%
#   summarise(count0 = sum(CAR == 0), count1 = sum(CAR ==
#                                                    1))
# data1
?rnorm
analyze.experiment <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  the.test <- t.test(x = the.dat[Group == "Treatment", 
                                 rc], y = the.dat[Group == "Control", rc], alternative = "greater")
  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  upper.bound <- the.test$conf.int[2]
  lower.bound <- the.test$conf.int[1]
  p <- the.test$p.value
  result <- data.table(effect = the.effect, upper_ci = upper.bound,
                       lower_ci = lower.bound, p = p)
  return(result)
}

analyze.experiment(the.dat = rc.dat2)


# Repeating the Scenario 1000 times
B <- 1000
n <- 200
RNGversion(vstr = 3.6)
set.seed(seed = 4172)

Experiment <- 1:B
Group <- c(rep.int(x = "Treatment", times = n/2), rep.int(x = "Control",
                                                          times = n/2))
sim.dat  <- as.data.table(expand.grid(Experiment = Experiment,
                                      Group = Group))
setorderv(x = sim.dat, cols = c("Experiment", "Group"), order = c(1,1))
sim.dat[Group == "Control", rc := round(x = rnorm(n = .N, mean = 16, sd = 7))]
sim.dat[Group == "Treatment", rc := round(x = rnorm(n = .N, mean = 20, sd =6))]
dim(sim.dat)

exp.results <- sim.dat[, analyze.experiment(the.dat = .SD), 
                       keyby = "Experiment"]

scenario2_1000 <- DT::datatable(data = round(x = exp.results[1:100, ], digits = 3), 
                                rownames = F)

# Percentage of True positive
exp.results[, mean(p < 0.05)]

# Percentage of False negative
1 - exp.results[, mean(p < 0.05)]

# Summary of P-Value
exp.results[, summary(p)]

# Summary of difference for prop-test
exp.results[, summary(effect)]

# Mean effect of the simulated data
exp.results[, mean(effect)]

# Summary of upper confidence interval of mean effect
exp.results[, summary(upper_ci)]

# Summary of lower confidence interval of mean effect
exp.results[, summary(lower_ci)]
