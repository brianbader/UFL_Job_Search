library(shiny)
library(rvest)
library(googlesheets)
library(DT)


shinyServer(function(input, output) {
  
  ## Function to find rows in dataframe 1 not in dataframe 2
  matchRows <- function(x.1, x.2, ...) {
    x.1p <- do.call("paste", x.1)
    x.2p <- do.call("paste", x.2)
    x.1[! x.1p %in% x.2p, ]
  }
  
  ## If you are running this on a local machine, need to get authorization first
  ## Use function 'gs_auth()' in googlesheets package
  GAP_KEY <- "1rd03X7AZYUH5Y6zsoKxADdRIpQtm4wz1pjHvM9lIWGM"
  
  gap <- GAP_KEY %>%
    gs_key()
  
  jobs.stored <- gap %>% 
    gs_read(ws = "UFLjobs")
  
  jobs.stored$Date <- as.Date(jobs.stored$Date, format = "%m/%d/%Y")
  
  ## Check against main page first
  main.page <- read_html("http://www.stat.ufl.edu/jobs/")
  
  ## Get page numbers
  pagenums <- main.page %>%
    html_nodes("footer ul li") %>%
    html_text()
  
  pagenums <- as.numeric(pagenums)[!is.na(as.numeric(pagenums))]
  
  withProgress(message = 'Loading...', detail = paste("page ", 1, " / ", max(pagenums)), 
               min = 0, max = 1, value = 1 / max(pagenums), {
                 
                 # Get main page URLs
                 urls.main <- main.page %>%
                   html_nodes(".joblist a") %>%
                   html_attr("href")
                 
                 ## Get job names, titles, and dates on main page
                 links.main <- main.page %>%
                   html_nodes(".joblist tr td") %>%
                   html_text()
                 
                 ## Make table from main page
                 jobs.temp <- as.data.frame(t(matrix(links.main, nrow = 3)))
                 colnames(jobs.temp) <- c("University/Company", "Position Title", "Date")
                 jobs.temp$`University/Company` <- paste('<a href="http://www.stat.ufl.edu/jobs/', urls.main, 
                                                         '" target="_blank">', jobs.temp$`University/Company`, '</a>', sep = "")
                 jobs.temp$`Position Title` <- as.character(jobs.temp$`Position Title`)
                 jobs.temp$Date <- as.Date(jobs.temp$Date, format = "%m/%d/%Y")
                 
                 ## Check if there are any new listings
                 newListings <- matchRows(jobs.temp, jobs.stored)
                 check <- matchRows(cbind.data.frame(substr(jobs.temp$`University/Company`, 50, 53), jobs.temp$Date), 
                                    cbind.data.frame(substr(jobs.stored$`University/Company`, 50, 53), jobs.stored$Date))
                 if(nrow(check) > 0) {
                   jobs <- rbind.data.frame(newListings, jobs.stored)
                   gs_add_row(gap, ws = "UFLjobs",
                              input = newListings)
                 } else {
                   jobs <- jobs.stored
                 }
                 
                 i <- 2
                 
                 while((nrow(check) > 0) & (i <= max(pagenums))) {
                   temp.page <- read_html(paste("http://www.stat.ufl.edu/jobs/?page=", i, sep = ""))
                   
                   # Get page URLs
                   urls.temp <- temp.page %>%
                     html_nodes(".joblist a") %>%
                     html_attr("href")
                   
                   ## Get job names, titles, and dates on each page
                   links.temp <- temp.page %>%
                     html_nodes(".joblist tr td") %>%
                      html_text()
                   
                   jobs.temp <- as.data.frame(t(matrix(links.temp, nrow = 3)))
                   colnames(jobs.temp) <- colnames(jobs)
                   jobs.temp$`University/Company` <- paste('<a href="http://www.stat.ufl.edu/jobs/', urls.temp, 
                                                           '" target="_blank">', jobs.temp$`University/Company`, '</a>', sep = "")
                   jobs.temp$`Position Title` <- as.character(jobs.temp$`Position Title`)
                   jobs.temp$Date <- as.Date(jobs.temp$Date, format = "%m/%d/%Y")
                   
                   newListings <- matchRows(jobs.temp, jobs.stored)
                   check <- matchRows(cbind.data.frame(substr(jobs.temp$`University/Company`, 50, 53), jobs.temp$Date), 
                                      cbind.data.frame(substr(jobs.stored$`University/Company`, 50, 53), jobs.stored$Date))
                   if(nrow(check) > 0) {
                     jobs <- rbind.data.frame(newListings, jobs)
                     gs_add_row(gap, ws = "UFLjobs",
                                input = newListings)
                   }
                   
                   # Increment the progress bar, and update the detail text.
                   incProgress(1/max(pagenums), detail = paste("page ", i, " / ", max(pagenums)))
                   
                   # Pause for 0.1 seconds to simulate a long computation.
                   Sys.sleep(0.1)
                   
                   i <- i + 1
                 }
                 
                 jobs <- jobs[!duplicated(jobs),]
                 jobs <- jobs[rev(order(jobs$Date)),]
                 
               })
  
  output$mytable <- renderDataTable({
    datatable(jobs, escape = FALSE)
  }, options = list(orderClasses = TRUE))
  
})