# Data cleaning and preparation
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(readr)
library(readxl)
library(purrr)
library(tidyr)
library(tidytext)


# Job Board ####
jb <- readRDS("data/jobs.rds")
# import IPEDS 2017 Institutional Characterstics  and dictionary
#https://nces.ed.gov/ipeds/datacenter/data/IC2017.zip
ic <- read_csv("data/ic2017.csv") # includes 8 imputed variables not in ic_dict$varlist
# for all coded values, may need to also merge Frequencies (one:many)
ic_dict <- full_join(read_xlsx("data/ic2017.xlsx", "varlist"),
                     read_xlsx("data/ic2017.xlsx", "Description"),
                     by = c("varnumber", "varname")
)
# import IPEDS 2017 directory information and dictionary
# https://nces.ed.gov/ipeds/datacenter/data/HD2017.zip
hd <- read_csv("data/hd2017.csv")
hd_dict <- full_join(read_xlsx("data/hd2017.xlsx", "varlist"),
                     read_xlsx("data/hd2017.xlsx", "Description"),
                     by = c("varnumber", "varname")
)

# tidy variable names, replacing ws with '_', no caps
(colnames(jb) <- tolower(colnames(jb)) %>%
  str_replace_all(" ", "_"))

# create factor for pay ranges
jb$salary_range <- factor(jb$salary_range,
                          levels = c("Dependent on qualifications and experience",
                                     "$20,000 - $40,000",
                                     "$40,000 - $60,000",
                                     "$60,000 - $80,000",
                                     "$80,000 - $100,000",
                                     "$100,000 and Higher"),
                          ordered = TRUE
)
plot(jb$salary_range)
# there end up being so few in the lowest category, 20k-40k, so I will merge this upward.
jb[jb$salary_range == "$20,000 - $40,000", "salary_range"] <- "$40,000 - $60,000"
jb$salary_range <- droplevels(jb$salary_range)


# Create cleaned application due date (due_date)
jb$due_date <- parse_date_time(jb$application_due_date,
                               c("%m/%d/%Y", "%m/%d%y", "%B %d, %Y", "%B %d", "%d %B %Y"),
                               tz = "EST")
jb[is.na(jb$due_date) | jb$due_date > ymd("2019-05-01"), "due_date"] <- ymd("2019-05-01")

# examine locations (not all are in US), fix duplicate D.C., and fix "" missings.
nonStates <- unique(jb$location)[!unique(jb$location) %in% state.name]
jb <- jb %>%
  mutate(location = case_when(location == "Washington D.C." | location == "District of Columbia" ~ "District of Columbia",
                              location == "" ~ NA_character_,
                              TRUE ~ location))
table(jb$location, exclude = FALSE)


# Merge IPEDS data in hd2016.csv, using unitid
# #   UNTID, ZIP, SECTOR, ICLELVEL, CONTROL, HBCU, TRIBAL, LOCALE, C15BASIC,
#   C15SZSET or INSTSIZE, LONGITUDE, LATITUDE, OBEREG.

# (fuzzy) matching to IPEDS data (hd2017.csv) on INSTNM or IALIAS to merge unitid
#   start with base::agrep (Approximate matching via regex's),
cat("There are", n_distinct(jb$institution),
    "unique institutions listed in job board data, of which",
    length(unique(jb$institution)[unique(jb$institution) %in% hd$INSTNM]) +
      length(unique(jb$institution)[unique(jb$institution) %in% hd$IALIAS]) ,
    "exist as-is in IPEDS directory info.\nWe'll see how many more we find through matching."
    )
# what will be joined...
matchedINSTNM <- jb %>%
  mutate(INSTNM = institution) %>%
  select(institution, INSTNM) %>%
  semi_join(hd, by = "INSTNM") %>%
  rename(instnm_matched = INSTNM) %>%
  distinct()
matchedIALISAS <- jb %>%
  mutate(IALIAS = institution) %>%
  select(institution, IALIAS) %>%
  semi_join(hd, by = "IALIAS") %>%
  rename(instnm_matched = IALIAS) %>%
  distinct()

# what won't be joined...
nomatchINSTNM <- jb %>%
  rename(INSTNM = institution) %>%
  select(INSTNM, location) %>%
  anti_join(hd, by = "INSTNM") %>%
  distinct() %>%
  glimpse()

# generate list of possible matches. Some get LOTS of matches (e.g., Doane University) because of common words.
possibleMatches <- apply(nomatchINSTNM, 1, agrep, hd$INSTNM, value = TRUE)
names(possibleMatches) <- nomatchINSTNM$INSTNM

sum(lapply(possibleMatches, length) == 0) # 51 failed to match (50%), but not all may reasonably exist in IPEDS data (e.g. private companies like SAS institute)

# a function to help correct data. Prompts user to determine which, if any, fuzzy-matched vectors are "true" matches.
chooseMatch <- function(x, y, z){
  # x - index of item of interest in y, a list
  # z - data frame containing INSTNM and location values for x
  #
  if (length(y[[x]]) > 0) {
    msg <- paste(y[[x]], "-", 1:length(y[[x]]),  collapse = "\n")

    cat("Select matches for", names(y[x]), "in", z[x, 2], "(0 for none):\n", msg, collapse = "\n")
    response <- as.numeric(readline(""))
    if (response == 0) {
      return(NA)
    } else {
      return(y[[x]][response])
    }

  } else {
    cat("Nothing fuzzy matched for", names(y[x]), ".\n")
    return(NA)
  }
}

# nomatchINSTNM$instnm_matched <- NA
# for (i in 1:length(possibleMatches)) {
#   nomatchINSTNM[i, "instnm_matched"] <- chooseMatch(i, possibleMatches, nomatchINSTNM)
# }

# save nomatches and possible matches for the record and reporting. Load these instead of doing all that work again.
# saveRDS(nomatchINSTNM, "data/nomatches_matched.rds")
# saveRDS(possibleMatches, "data/fuzzymatches.rds")
nomatchINSTNM <- readRDS("data/nomatches_matched.rds")


# add fuzzy-matched instnames where they exist.
jb <- jb %>%
  rename(INSTNM = institution) %>%
  left_join(nomatchINSTNM[, -(grep("location", colnames(nomatchINSTNM)))], by = "INSTNM") %>%
  rename(institution = INSTNM)

jb <- jb %>%
  left_join(matchedINSTNM, by = "institution") %>%
  left_join(matchedIALISAS, by = "institution") %>%
  rename(instnm_matched.z = instnm_matched)

nrow(jb[!is.na(jb$instnm_matched.z) & !is.na(jb$instnm_matched.x),])
jb[!is.na(jb$instnm_matched.z) & !is.na(jb$instnm_matched.x), "instnm_matched.z"] <- NA # don't want to overwrite valid names

jb$instnm_matched <- jb %>%
  select(instnm_matched.x, instnm_matched.y, instnm_matched.z) %>%
  replace_na(list(instnm_matched.x = "", instnm_matched.y = "", instnm_matched.z = "")) %>%
  unite("instnm_matched", sep = "") %>%
  pull()

jb[, c("instnm_matched.x", "instnm_matched.y", "instnm_matched.z")] <- NULL

# merge institutional data (hd) on INSTNM or IALIAS where there are matches, and instnm_matched where fuzzy matched.
hd <- select(hd, INSTNM, IALIAS, UNITID, ICLEVEL, CONTROL, C15BASIC, INSTSIZE) %>%
  mutate(instnm_matched = INSTNM)

jb <- left_join(jb, hd, by = "instnm_matched") %>%
  mutate(INSTNM = NULL,
         IALIAS = NULL)

# recode the institution data...
jb$ICLEVEL <- factor(jb$ICLEVEL,
                     labels = c("Four or more years",
                                "Less than four years"))
jb$CONTROL <- factor(jb$CONTROL,
                     labels = c("Public", "Private not-for-profit", "Private for-profit"))
jb <- jb %>% mutate(
  C15BASIC = case_when(
    C15BASIC < 1 ~ NA_character_,
    C15BASIC >= 1 & C15BASIC <= 14 ~ "Associates or Special focus two-year",
    C15BASIC >= 15 & C15BASIC <= 17 ~ "Doctoral",
    C15BASIC >= 18 & C15BASIC <= 20 ~ "Master's",
    C15BASIC > 19 ~ "Baccalaureate"),
  INSTSIZE = case_when(
    INSTSIZE == -2 ~ NA_character_,
    INSTSIZE == 1 ~ "Under 1,000",
    INSTSIZE == 2 ~ "1,000 - 4,999",
    INSTSIZE == 3 ~ "5,000 - 9,999",
    INSTSIZE == 4 ~ "10,000 - 19,999",
    INSTSIZE == 5 ~ "20,000 and above")
  )

saveRDS(jb, "data/jobs_clean.rds")

# Job Descriptions ####
jd <- readRDS("data/job_desc.rds")
# each entry contains two data frames with two column each and a variable number of rows (11-16). These  df's will be  reduced or expanded to a common size before merging to jb.

unique(map(jd, colnames)) # X2 col has data, X1 field names
# determine common rows from X1 column. There are 5 unique sets of variables.

common <- jd %>%
  map(pluck, "X1") %>%
  discard(is.null) %>% # to account for those w/o job desc
  unique() %>%
  unlist() %>%
  unique()

(common <- tibble(X1 = common))

# Some of these fields I already have in jb: State/Province, application due date,
# and some I likely won't need: Application procedure, Other location, Institution description, Job start date, City
# I will want to retain salary range and benefits, reporting information, Qualifcations and Job Duties (most importantly).
# First I will need to address NULLs in jd list, which arose from no pages being available during site reconstruction.
filler <- cbind(common, X2 = "")
jd <- jd %>%
  modify_if(is.null, ~filler) %>% # impute filler table, which has common fields in col1, but NA in col2
  modify_if(is.logical, ~filler)
some(jd, is.null)
some(jd, is.logical)
rm(filler)

common <- filter(common,  X1 %in% c("Posted on", "Job type", "Reports to", "Reports in office",
                           "Job duties", "Qualifications", "Salary range", "Salary benefits"))

# See which fields will be excluded...
jd %>%
  map(anti_join, common, by = "X1") %>%
  head()

jd <- jd %>%
  map(semi_join, common, by  = "X1")

jd %>% keep(map(jd, nrow) < 8) %>% length() # 14 cases don't have all 8 required fields; this turns out to be just two cases
jd %>% keep(map(jd, nrow) < 8) %>% unique()

only5 <- jd %>% keep(map(jd, nrow) == 5) %>%
  unique() %>%
  map(pluck, "X1") %>%
  unlist()
only5 <- cbind(common[!pull(common) %in% only5, ],
               X2 = "")

only6 <- jd %>% keep(map(jd, nrow) == 6) %>%
  unique() %>%
  map(pluck, "X1") %>%
  unlist()
only6 <- cbind(common[!pull(common) %in% only6, ],
               X2 = "")

# Tried getting modify_if to work correctly with bind_rows, but couldnt...instead, segregating data to work on it before putting it back together.
jd8 <- keep(jd, map(jd, nrow) == 8)
jd5 <- keep(jd, map(jd, nrow) == 5) %>%
  map(bind_rows, only5)
jd6 <- keep(jd, map(jd, nrow) == 6) %>%
  map(bind_rows, only6)

jd <- splice(jd8, jd5, jd6)
rm(list = c("jd8", "jd5", "jd6", "only5", "only6"))

# check that each entry has the desired 6 rows
jd %>% map(nrow) %>% unlist() %>% mean()

id <- names(jd)
# now render as data frame.
jd <- jd %>%
  map(t) %>%
  map(as_tibble) %>%
  map(slice, 2) %>% # as only row 2 (formerly col 2) contains data elements
  map(`colnames<-`, common[[1]]) %>%
  bind_rows() %>%
  tibble::add_column(id, .before = TRUE) # add ID for merging

# Merge job board data with job description ####
allJobs <- left_join(as_tibble(jb), jd, by = "id")

all(allJobs$salary_range == allJobs$`Salary range`) # There are some missing values in Salary range coming from job descriptions, so this will be dropped.
allJobs$`Salary range` <- NULL
allJobs$`Posted on` <- NULL # had this already

# Those with links unavailable were scraped during site reconstruction, and so don't have descriptive information and will be excluded from any analysis involving that. However they still have salary and geographic data, so they are here retained.
jobs <- allJobs %>% filter(!duplicated(job_link) | job_link == "unavailable")

# Filter those where link is "unavailable". This is 288 of the 556 currently semi-filtered cases, of which 89 appear to be unique based on several dinstinct() combinations.
jobs <- jobs %>%
  filter(job_link != "unavailable") %>%
  distinct(job_title, institution, .keep_all = TRUE) %>%
  bind_rows(distinct(
    filter(
      jobs, job_link == "unavailable"), job_title, institution, .keep_all = TRUE)
    )

cat("Of the", nrow(allJobs), "jobs originally scraped", nrow(jobs), "were unique across",
    length(unique(jobs$institution)), "organizations.")


# tidy variable names, replacing ws with '_', no caps
(colnames(jobs) <- tolower(colnames(jobs)) %>%
    str_replace_all(" ", "_"))

# job duties and qualificiations text ####
# tokenize the text contained in jobs$job_duties and jobs$qualifications,
# then remove stop_words, group and nest at the id (individual job) level so they can be merged with other jobs data.

duties <- jobs %>%
  select(id, job_duties) %>%
  unnest_tokens(word, job_duties) %>%
  anti_join(stop_words) %>%
  group_by(id) %>%
  nest() %>%
  rename(job_duties_tkns = data)

qualifications <- jobs %>%
  select(id, qualifications) %>%
  unnest_tokens(word, qualifications) %>%
  anti_join(stop_words) %>%
  group_by(id) %>%
  nest() %>%
  rename(qualifications_tkns = data)

jobs <- jobs %>%
  mutate(job_duties = NULL, # won't need the original fields anymore
         qualifications = NULL) %>%
  left_join(duties) %>%
  left_join(qualifications)

jobs <- jobs %>%
  select(id, reports_to) %>%
  unnest_tokens(word, reports_to) %>%
  anti_join(stop_words) %>%
  group_by(id) %>%
  nest() %>%
  rename(reports_to_tkns = data) %>%
  right_join(jobs) # Right_join to ensure all original data are retained

jobs <- jobs %>%
  select(id, reports_in_office) %>%
  unnest_tokens(word, reports_in_office) %>%
  anti_join(stop_words) %>%
  group_by(id) %>%
  nest() %>%
  rename(reports_in_office_tkns = data) %>%
  right_join(jobs) # Right_join to ensure all original data are retained

jobs <- jobs[ , c(1, 4:ncol(jobs), 2:3 ) ] # shift cols to end


# Add explicit 'missing', and collapse private for-profit into all private (only 6)
jobs  <- jobs %>%
  mutate(
    iclevel = case_when(
      is.na(iclevel) ~ "Other",
      iclevel == "Four or more years" ~ "Four or more years",
      iclevel == "Less than four years" ~ "Less than four years"),
    control = case_when(
      is.na(control) ~ "Other",
      control == "Public" ~  "Public",
      control == "Private not-for-profit" ~  "Private",
      control == "Private for-profit" ~  "Private")
  )

duties_nstd_salary <- jobs %>%
  select(salary_range, job_duties_tkns) %>%
  filter(!map_lgl(job_duties_tkns, is.null)) %>% # drop nulls
  unnest() %>%
  # drop non-word characters, and NAs, as these will clutter results.
  mutate(word = str_extract(word, "[A-z]+")) %>%
  filter(!is.na(word)) %>%
  # add proportion of word occurence in salary ranges
  count(salary_range, word) %>%
  group_by(salary_range) %>%
  mutate(prop_salary = n / sum(n)) %>%
  rename(n_salary = n)

qualifications_nstd_salary <- jobs %>%
  select(salary_range, qualifications_tkns) %>%
  filter(!map_lgl(qualifications_tkns, is.null)) %>% # drop nulls
  unnest() %>%
  # drop non-word characters, and NAs, as these will clutter results.
  mutate(word = str_extract(word, "[A-z]+")) %>%
  filter(!is.na(word)) %>%
  # add proportion of word occurence in salary ranges
  count(salary_range, word) %>%
  group_by(salary_range) %>%
  mutate(prop_qual = n / sum(n)) %>%
  rename(n_qual = n)

reporting_office_nstd_salary <- jobs %>%
  select(salary_range, reports_in_office_tkns) %>%
  filter(!map_lgl(reports_in_office_tkns, is.null)) %>% # drop nulls
  unnest() %>%
  # drop non-word characters, and NAs, as these will clutter results.
  mutate(word = str_extract(word, "[A-z]+")) %>%
  filter(!is.na(word)) %>%
  # add proportion of word occurence in salary ranges
  count(salary_range, word) %>%
  group_by(salary_range) %>%
  mutate(prop_office = n / sum(n)) %>%
  rename(n_office = n) %>%
  nest() %>%
  rename(data_office = data)

salary <- full_join(duties_nstd_salary, qualifications_nstd_salary, by = c("word", "salary_range")) %>%
  nest() %>%
  left_join(reporting_office_nstd_salary, by = "salary_range")

saveRDS(salary, "data/salary.rds")
saveRDS(jobs, "data/jobs_clean_merged.rds")
