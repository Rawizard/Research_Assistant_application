#' ---
#' title: "Research Assistant application (072127) - Coding task"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---


# Install and load package to access database + load dplyr 
# install.packages("RPostgreSQL")
library(RPostgreSQL)
library(dplyr)

# Connect to AACT database
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname = "aact", host = "aact-db.ctti-clinicaltrials.org", port = 5432, 
                 user = ("jamiec"), password = "Bayesianinf12") # Connection profile

# Query AACT using dplyr 
query.result1 <- tbl(src = con, from = "studies") %>%       # Database connection, access studies table
  filter(phase %in% c("Phase 2", "Phase 3"),                # Only include phase 2 and 3 trials
         overall_status %in% "Completed") %>%               # That are completed
  left_join(y = tbl(src = con, from = "calculated_values"), # Join calculated values table to studies table 
            by = "nct_id") %>%                              # Using unique trial ID
  filter(registered_in_calendar_year == 2019) %>%           # Only including trials registered in 2019
  collect()                                                 # Store query 

# Create summary comparison between phase 2 and 3 clinical trials 
query.summary1 <- query.result1 %>%                                                      # Act on query result object
  group_by(phase) %>%                                                                    # Group by trial phase
  summarise(Number_of_trials = n(),                                                      # Number of Phase 2 & 3 trials 
            Total_enrollment = sum(enrollment),                                          # Total number of enrolled participants   
            Median_enrollment = median(enrollment),                                      # Median number of enrolled participants p/trial
            Median_number_of_arms = median(number_of_arms),                              # Median number of arms p/trial
            Total_FDA_regulated_drugs = sum(is_fda_regulated_drug == "TRUE", na.rm = T), # Total number of FDA regulated drugs used 
            Average_duration (days) = mean(completion_date - start_date))                # Average trial duration 

# Save summary as csv file
write.csv(query.summary1, file = "summary_table.csv")



##  Alternative query method using SQL  ##

# Set up SQL query
select.q <- "SELECT *" 
from     <- "FROM studies" 
l.join   <- "LEFT JOIN calculated_values ON calculated_values.nct_id = studies.nct_id"
where    <- "WHERE studies.overall_status = 'Completed' AND studies.phase IN ('Phase 2', 'Phase 3') AND calculated_values.registered_in_calendar_year = 2019"
query    <- paste(select.q, from, l.join, where)

# Query AACT database using DBI 
query.result2 <- dbGetQuery(conn = con, statement = query)

# Check result  
head(query.result2)


#' *NOTE:*
#' 
#' query.result1 has 82 columns while query.result2 has 83 columns due to nct_id being 
#' duplicated (from studies and calculated_values tables).
#' 
#' "Select" can also be used in both query methods to return only specified columns.
#' 
#' I have only produced one summary table using the dplyr query method to minimise
#' repetitive code. 
#' 
#' Query terms from AACT Data Dictionary: https://aact.ctti-clinicaltrials.org/data_dictionary
#'   


# Dependencies 
sessionInfo()
