# packages to load
library(httr)
library(jsonlite)
library(tidyverse)
library(stringi)

#Get data from API
url <- "https://opendata.socrata.com"
path <- "resource/n2rk-fwkj.json"
raw.data <- GET(url = url, path = path)

#Convert content to characters to make it readable
char.raw.data <- rawToChar(raw.data$content)

#Convert from JSON to df to tibble
data <- fromJSON(char.raw.data)
data <- as.tibble(data)
View(data)


# Data Prep ---------------------------------------------------------------

# a bit of tidying classes
data$balance <- as.numeric(data$balance) 
data$last_transaction <- as_datetime(data$last_transaction)

# regex mayhem! Let's clean up all those erroneous spaces in first_names
data$first_name <- stri_trim_right(data$first_name)

# let's also replace NAs and blanks in first_name so we can use it for count/tally if needed
data$first_name[is.na(data$first_name)] <- "NO NAME"
data$first_name[data$first_name == ""] <- "NO NAME"

# Get a list of bank names to ensure there are no missing names or anything weird (typos, etc)
bank_list <- unique(data$bank_name)
View(bank_list)

# Nope, it's dirty. Let's first fix the two dupes and we can deal with the NAs later

data$bank_name[data$bank_name == "CANADIAN IMPERIAL BANK OF COMMERCEMMERCE"] <- "CANADIAN IMPERIAL BANK OF COMMERCE"
data$bank_name[data$bank_name == "MONTREAL TRUST COMPANY OF CA"] <- "MONTREAL TRUST COMPANY "
data$bank_name[data$bank_name == "MONTREAL TRUST COMPANY "] <- "MONTREAL TRUST COMPANY"
bank_list <- unique(data$bank_name)
View(bank_list)


# Question 1 --------
# Find which bank registered the highest amount of unclaimed money

highest_bank <- data %>%
  group_by(bank_name) %>%
  summarize(top_bank_balance = max(sum(balance))) %>%
  arrange(desc(top_bank_balance)) %>%
  slice(1:1)
View(highest_bank)

  # there are accounts with missing bank names (NAs), let's see if it impacts our findings above

nobank <- data %>%
  filter(is.na(bank_name)) %>%
  summarize(nobank_balance = sum(balance))
View(nobank)

if (nobank$nobank_balance > (Bank_balances$bank_balance_total[1] - Bank_balances$bank_balance_total[2])) {
  print("DAMN, account balances without a bank name exceeds the ∆ between the top two banks... Bad data.") & stop()
} else {print("Cool, even though some accounts don't have a bank name, they won't change the outcome")}



# Question 2 --------------------------------------------------------------
# Which bank registered the highest amount of unclaimed CAD$ relative to its number of unclaimed bank accounts?

#get number of accounts per bank then summarise amount/accounts, sort, and print

accounts <- data %>%
  group_by(bank_name) %>%
  summarise(amt_over_accts = sum(balance)/n()) %>%
  arrange(desc(amt_over_accts)) %>%
  slice(1:1)
View(accounts)


# Question 3 --------------------------------------------------------------
# Did the total number of unclaimed bank accounts increased or decreased over time?

# not sure if I understood this question, so I'll assume it means number of unclaimed accts per year?

# separate year from last_transaction
data <- data %>%
  mutate(year = year(last_transaction))

# visualize it
by_year <- data %>%
  group_by(year) %>%
  summarise(annual_total = n())
ggplot(by_year, aes(x = year, y = annual_total)) +
  geom_col() +
  geom_smooth(method = "auto")


# Answers -----------------------------------------------------------------

# Question 1: Find which bank registered the highest amount of unclaimed money
print(highest_bank)

# Question 2: Which bank registered the highest amount of unclaimed CAD$ relative to its number of
#unclaimed bank accounts?
print(accounts)

# Question 3: Did the total number of unclaimed bank accounts increased or decreased over time?
print("increased")
 



