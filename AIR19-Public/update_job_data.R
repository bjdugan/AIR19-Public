update_job_data <- function(jobs_file, jobs_desc, addr, node_xpath1, node_xpath2, ndays = 5) {
  # file - character string poitning to rds.
  # new data - data.frame object of jobs to add
  # addr - URL address passed to job_board_update
  # node_xpath1 - xpath identified with Chrome Inspect, passed to job_board_update, refers to table at addr
  # node_xpath2 - xpath identified with Chrome Inspect, refers to individual job description tables at addr
  # ndays is the number of days allowed between udpates. Default is 5. ndays = 1 would permit same-day appending and is advised only in the case of revisions.

  sink("update_jobs_data_log.txt", append = T)
  cat(paste0("\n", Sys.time(), "\n"))
  if (file.exists(jobs_file)) {
    # append if data exist, create otherwise.
    if (Sys.Date() >= as.Date(file.info(jobs_file)$mtime) + ndays) {
      x <- readRDS(jobs_file)
      y <- scrape_job_board(addr, node_xpath1)
      y$ID <- sprintf("%04d", (nrow(x) + 1):(nrow(x) + nrow(y))) # ID starts with n+1th number
      y <- y[,c("ID", "Job Title", "Institution", "Location",
                "Salary Range", "extract_date", "Application Due Date",
                "Posted On", "job_link")]
      x <- rbind(x, y)
      saveRDS(x, jobs_file)
      # update job description data, beginning with last appended ending with first appended
      d <- readRDS(jobs_desc)
      d_n <- length(d)
      for (i in (nrow(x) - nrow(y) + 1):nrow(x)) {

        if (x[i, "job_link"] != "unavailable") {
          jobURL <- read_html(x[i, "job_link"])

          z <- jobURL  %>%
            html_node(xpath = node_xpath2) %>%
            html_table()
          d[[i]] <- z[-1, ]
          names(d)[i] <- x[i, "ID"]
        } else {
          d[[i]] <- NA
          names(d)[i] <- x[i, "ID"]
        }

      }
      saveRDS(d, "./data/job_desc.rds")
      d_n <- length(d) - d_n


      return(c(cat(paste(nrow(y), "new rows added to", jobs_file, "\nModified:",
                         as.POSIXct(file.info(jobs_file)$mtime), "\nCreated:",
                         as.POSIXct(file.info(jobs_file)$ctime), "\n",
                         d_n, "job descriptions added to", jobs_desc, "\nModified:",
                         as.POSIXct(file.info(jobs_desc)$mtime), "\nCreated:",
                         as.POSIXct(file.info(jobs_desc)$ctime)
                   )))
      )
    } else {
      return(cat(paste("New and old data collected within", ndays ,"days:",
                       "\nModified:",  as.POSIXct(file.info(jobs_file)$mtime),
                       "\nCreated:", as.POSIXct(file.info(jobs_file)$ctime),
                       "\nNo action taken. Consider reducing ndays parameter."
      ))
      )
    }
  } else {
    y <- scrape_job_board(addr, node_xpath1)
    y$ID <- sprintf("%04d", 1:nrow(y)) # add ID, rearrange.
    y <- y[,c("ID", "Job Title", "Institution", "Location",
              "Salary Range", "extract_date", "Application Due Date",
              "Posted On", "job_link")]
    d <- list()
    for (i in (1:nrow(y))) {

      jobURL <- read_html(y[i, "job_link"])

      z <- jobURL  %>%
        html_node(xpath = node_xpath2) %>%
        html_table()
      d[[i]] <- z[-1, ]
      names(d)[i] <- y[i, "ID"]
    }
    saveRDS(y, jobs_file)
    saveRDS(d, jobs_desc)
    return(c(cat("No such file found at", jobs_file,"\nSaving new data there instead.",
                 "\nSaved job descriptions as well at", jobs_desc)
             ))
  }

  sink()

}

