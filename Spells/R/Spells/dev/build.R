# build file
library(here)
library(devtools)
pkg_path <- here("Spells","R","Spells")

document(pkg_path)
check(pkg_path)
vaccinate(pkg_path)

devtools::install_local(here::here("Spells","R","Spells"), force = TRUE)


x <- rep(c("Employed", "Inactive", "Retired", "Employed", "Retired", 
           "Dead"), c(7,  8,  3,  3, 25,  5))
# a single 8-year spell of inactivity, counting up
clock(x, "Inactive", clock_type = "step", increasing = TRUE)
# a single 8-year spell of inactivity, counting down
clock(x, "Inactive", clock_type = "step", increasing = FALSE)
# two, employment spells, each with its own clock
clock(x, "Employed", clock_type = "step", increasing = TRUE)
# two, employment spells, throw out first because left censored
clock(x, "Employed", clock_type = "step", increasing = TRUE, not_first = TRUE)
# but no need to throw out first if counting down:
clock(x, "Employed", clock_type = "step", increasing = FALSE, not_first = FALSE)
# for total durations we do want to throw out the first spell if left censored
clock(x, "Employed", clock_type = "duration", not_first = TRUE)
# TODO: pathological case: inactivity spells not left censored. Need better ID.
clock(x, "Inactivity", clock_type = "duration", not_first = TRUE)#

# total duration at entry or exit of spell:
clock(x, "Inactivity", clock_type = "duration", dur_condition = "entry", not_first = TRUE)
clock(x, "Inactivity", clock_type = "duration", dur_condition = "exit", not_first = TRUE)
# merges first consecutive employment and inactivity spells into a single spell,
# also catches second employment after retirement
clock(x, c("Inactive","Employed"), clock_type = "step", increasing = FALSE, not_first = FALSE)
# count down spell order
clock(x, c("Employed"), clock_type = "order", increasing = FALSE, not_first = FALSE)
# again w merged states
clock(x, c("Inactive","Employed"), clock_type = "order", increasing = FALSE, not_first = FALS)
        clock(x, c("Employed"), clock_type = "order", increasing = FALSE, not_first = FALSE)
      # total lifespan after 50
      clock(x, 
            state = c("Inactive","Employed","Retired"), 
            clock_type = "duration", 
            not_first = FALSE)
      # shortcut for the same:
      clock(x, "ALL", clock_type = "duration", increasing = FALSE, not_first = FALSE)
      # remaining lifespan
      clock(x, "ALL", clock_type = "step", increasing = FALSE, not_first = FALSE)