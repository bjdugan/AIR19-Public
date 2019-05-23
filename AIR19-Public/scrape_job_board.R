scrape_job_board <- function(addr, node_xpath) {
  # addr should be html address, e.g., https://www.airweb.org/Careers/Pages/Jobs.aspx
  # node_xpath can be identified with Chrome Inspect feature
  if (is.character(addr) & is.character(node_xpath)) {
    jobsURL <- read_html(addr)
    jobsDF <- jobsURL %>%
      html_node(xpath = node_xpath) %>%
      html_table(header = TRUE,
                 trim = TRUE,
                 fill = TRUE)
    # identify date of extraction
    jobsDF$extract_date <- as.Date(Sys.Date())
    # extract and add links on job title to job description
    job_links <- html_attr(html_nodes(jobsURL, "td a"), "href") # hypertext refs in table cells
    job_links <- job_links[!is.na(job_links)]
    job_links <- job_links[grep("jobdetails", job_links)]
    jobsDF$job_link <- paste0("https://www.airweb.org/Careers/Pages/", job_links)
    # to account for absence of links during website update
    if (length(job_links) == 0) {
      jobsDF$job_link <- "unavailable"
    }

    # some formatting the table
    jobsDF$`Posted On` <- as.Date(format(jobsDF$`Posted On`), "%m/%d/%Y")
    # reorganize columns
    jobsDF <- jobsDF[, c("Job Title", "Institution", "Location",
                         "Salary Range", "extract_date", "Application Due Date",
                         "Posted On", "job_link")]
    return(jobsDF)
  } else {
    return(cat("Both arguments must be character vectors: html address and node xpath."))
  }
}
