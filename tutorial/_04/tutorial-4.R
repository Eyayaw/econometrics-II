
# load packages  ----------------------------------------------------------
library(ggplot2) # plotting workhorse in R
library(data.table) # fast data manipulation
library(dplyr) # simple but slower than data.table api for data manipulation

path <- "~/Documents/PhD Archive/semester-II/Econometrics-2/tutorial/otc_regulation.dta"

otc <- haven::read_dta(path)

otc <- data.table::as.data.table(otc)

# rows with empty event_date values; state FL
with(otc, which(event_date == ""))
otc[event_date == "", event_date := NA]

otc <- na.omit(otc) # remove rows with missing vals: on all cols


# let's convert the date vars into date format
in_cols <- c("any_law", "event_date")

otc[, (in_cols) := .(
  as.Date(any_law, "%m/%d/%Y"),
  as.Date(event_date, "%d%b%Y")
)]

# # month of law enactment and records
# otc[, paste0("month_", in_cols) := .(
#   format(any_law, "%b"),
#   format(event_date, "%b")
# )]
# # year of law enactment and records
# otc[, paste0("year_", in_cols) := .(
#   year(any_law),
#   year(event_date)
# )]


# create otc variable -----------------------------------------------------

##  helper functions ----------------------------
### while the first day of the month is undoubtedly clear,
###  the last day of the month depends on the underlying month. However, it's easy to compute. The last day of the month is the day before the first day of the next month.

last_date <- function(dat) {
  f <- function(x) {
    mm <- as.integer(format(x, "%m"))
    dd <- format(x, "%d")
    yy <- as.integer(format(x, "%Y"))
    if (mm <= 11) {
      nxt_mm <- mm + 1
    } else if (mm == 12) {
      nxt_mm <- 1
      yy <- yy + 1
    }
    first_day_next_month <-
      as.Date(sprintf("%s-%s-%s", yy, nxt_mm, "01"))
    first_day_next_month - 1
  }
  out <- vapply(dat, f, 1)
  as.Date(out, origin = "1970-01-01")
}

# gets the last day of the month
last_day <- function(x) as.numeric(format(last_date(x), "%d"))

# gets the first date of the month
first_date <- function(x) {
  mm <- as.integer(format(x, "%m"))
  yy <- as.integer(format(x, "%Y"))
  # first_day_of_the_month
  as.Date(sprintf("%s-%s-%s", yy, mm, "01"))
}


# The variable otc is an indicator set to 0 in the months prior to implementation of a state's regulation, 1 afterwards, and the fraction of the month a regulation was implemented in months a regulation was enacted.  If a state law was enacted on the 15th of February of 2005 otc would be coded as (28−14)/28 = 0.5 during that time period for that state, for example.

frac_otc_month <- function(x) {
  # gets the day from the date
  day <- function(x) {
    as.integer(format(x, "%d"))
  }
  1 - ((day(x) - 1) / last_day(x))
}
# ----------------------------------------------------------

otc[, "days_from.until_otc" := event_date - any_law]

otc[, OTC := fcase(
  event_date < first_date(any_law), 0,
  event_date > last_date(any_law), 1,
  event_date >= first_date(any_law) & event_date <= last_date(any_law), frac_otc_month(event_date)
), by = state_ab]

# labs producing 9 and more oz of methamphetamine
otc[, cap_above_9_oz := tot_labs - (cap_under_2_oz + cap_2_8_oz)]


## US pop covered by the OTC law
otc[, pop_covered := OTC * pop_all_fitted]


# compute new vars by averaging

in_cols <- c(
  "event_date", "pop_all_fitted", "pop_covered",
  "cap_under_2_oz", "cap_2_8_oz", "cap_above_9_oz"
)

nms <- c(
  paste0("mean_", in_cols[-1]),
  paste0("tot_", in_cols[-1])
)

otc_summary <- otc[, ..in_cols][,
  {
    c(
      lapply(.SD, mean),
      lapply(.SD, sum)
    )
  },
  by = .(event_date),
  .SDcols = in_cols[-1]
]

names(otc_summary)[-1] <- nms

in_cols <- c("event_date", "tot_pop_all_fitted", "tot_pop_covered", "mean_cap_under_2_oz", "mean_cap_2_8_oz", "mean_cap_above_9_oz")

otc_summary <- otc_summary[, ..in_cols]

# proportion of the US pop covered by the OTC law
otc_summary[, pop_covered := (tot_pop_covered / tot_pop_all_fitted)]
