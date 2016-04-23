library(shiny)
library(rvest)
library(RCurl)

## Function to find rows in dataframe 1 not in dataframe 2
matchRows <- function(x.1, x.2, ...) {
  x.1p <- do.call("paste", x.1)
  x.2p <- do.call("paste", x.2)
  x.1[! x.1p %in% x.2p, ]
}

## Read in from github (can change this to local file)

x <- getURL("https://raw.githubusercontent.com/geekman1/UFL_Job_Search/master/jobs.csv")
jobs.stored <- read.csv(text = x, header = TRUE)[, -1]
colnames(jobs.stored) <- c("University/Company", "Position Title", "Date", "Link")

## Check against main page first
main.page <- html("http://www.stat.ufl.edu/jobs/")

## Get page numbers
pagenums <- main.page %>%
  html_nodes("footer ul li") %>%
  html_text()

pagenums <- as.numeric(pagenums)[!is.na(as.numeric(pagenums))]

# Get main page URLs
urls.main <- main.page %>%
  html_nodes(".joblist a") %>%
  html_attr("href")

## Get job names, titles, and dates on main page
links.main <- main.page %>%
  html_nodes(".joblist tr td") %>%
  html_text()

## Make table from main page
jobs.temp <- cbind.data.frame(t(matrix(links.main, nrow = 3)), paste("http://www.stat.ufl.edu/jobs/", urls.main, sep = ""))
colnames(jobs.temp) <- c("University/Company", "Position Title", "Date", "Link")

## Check if there are any new listings
newListings <- matchRows(jobs.temp, jobs.stored)
jobs <- rbind.data.frame(newListings, jobs.stored)

i <- 2

while((nrow(newListings) > 0) & (i <= max(pagenums))) {
  temp.page <- html(paste("http://www.stat.ufl.edu/jobs/?page=", i, sep = ""))
  
  # Get page URLs
  urls.temp <- temp.page %>%
    html_nodes(".joblist a") %>%
    html_attr("href")
  
  ## Get job names, titles, and dates on each page
  links.temp <- temp.page %>%
    html_nodes(".joblist tr td") %>%
    html_text()
  
  jobs.temp <- cbind.data.frame(t(matrix(links.temp, nrow = 3)), paste("http://www.stat.ufl.edu/jobs/", urls.temp, sep = ""))
  colnames(jobs.temp) <- names(jobs)
  
  newListings <- matchRows(jobs.temp, jobs.stored)
  jobs <- rbind.data.frame(newListings, jobs)
  
  i <- i + 1
}



shinyServer(function(input, output) {

  output$mytable = renderDataTable({
    jobs
  }, options = list(orderClasses = TRUE))
  
})
