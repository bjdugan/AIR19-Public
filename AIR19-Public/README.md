## Contents
 * data: dir containing AIR job board data, job descriptions, other data files.
 * plots: dir containing AIR presentation images.
 * literature: dir containing supporting literature for project.
 * scrape_job_board.r R function to fetch table from AIR Job Board
 * update_job_data.r R function to update (or create, if nonexistent) data fetched from AIR Job Board; does a little bookkeeping in update_jobs_data_log.txt as well.
 * weekly_scrape.r R script that calls update_job_data.r, and in turn scrape_board_data, and provides details about process. This is the code run roughly every week between Nov. 5 2018 to Feb. 12, 2019. Originally data collection was intended to span until May 2019, but the launch of the retooled jobs page in mid-February rendered the existing process null.
 * prepare.r R script to clean and prepare data for presentation. 
 * analysis.rmd Rmd to do generate output for poster. 

 
