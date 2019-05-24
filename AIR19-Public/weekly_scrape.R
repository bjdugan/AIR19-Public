# Scraping AIR Jobs page https://www.airweb.org/Careers/Pages/Jobs.aspx
# This script calls a function, update_jobs_data, which itself calls another
# (jobs_board_scrape) that together pull data from the jobs board, make minor edits,
# and append to existing data.
# This script will be run on a regular basis (despite my best efforts, I could not
# get TaskScheduler to work appropriately) on the 5th, 12th, 19th, and 26th of
#  every month from Nov 2018 to May 2019 (perhaps longer).

# Git version control was initiated mid-January 2019.

# AIR began launching a redesigned website in late January 2019, and hosted a temporary,
# non-interactive Jobs Board starting then. The interactive version and thus job
# descriptions were last known to be available around  2019-01-19.


library(rvest)
library(tidyverse)

source("scrape_job_board.R")
source("update_job_data.R")


# Paths prior to AIR website update (~2019-01-25)
old_paths <- list(
  addr = "https://www.airweb.org/Careers/Pages/Jobs.aspx",
  # job board table
  node_xpath1 = '//*[@id="ctl00_m_g_740dac4c_352f_4793_ab77_eb689a86f2da"]/span/table',
  # job descriptions; this appears to be constant across job pages
  node_xpath2 = '//*[@id="ctl00_m_g_358a82e0_9654_4b59_8311_21ee8381e00f"]/span/div/table'
)
# Paths during transition to new AIR website ~2019-01-25 to...
temp_paths <- list(
  addr = "https://www.airweb.org/resources-tools/job-board",
  node_xpath1 = '//*[@id="Main_TE7036BE6003_Col00"]/div[2]/table',
  node_xpath2 = NA # these were not included in non-interactive version available during website update.
)

t1 <- Sys.time()
update_job_data(jobs_file = "./data/jobs.rds",
                 jobs_desc = "./data/job_desc.rds",
                 addr = "https://www.airweb.org/resources-tools/job-board",
                 node_xpath1 = '//*[@id="Main_TE7036BE6003_Col00"]/div[2]/table',
                 node_xpath2 = NA,
                 ndays = 5)
t2 <- Sys.time()
print(t2 - t1)
closeAllConnections()
print(t2 - t1)
cat(tail(readLines("update_jobs_data_log.txt"), n = 7), sep = "\n")

# sink("update_jobs_data_log.txt", append = T)
# cat(paste0("\n", Sys.time(), "\n"))
# cat(paste0("'The interactive Job Board is temporarily unavailable due to the transition to an updated website and user portal, however, this feature should be available soon. In the interim, the current list of jobs is available below.', from https://www.airweb.org/resources-tools/job-board."))
# sink()
