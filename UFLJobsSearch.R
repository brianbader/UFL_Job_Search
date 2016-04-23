usePackage <- function(p) {
  newPackages <- p[!(p %in% installed.packages()[, "Package"])]
  if(length(newPackages))
    install.packages(newPackages, dependencies = TRUE)
  cat("Packages successfully loaded:\n")
  sapply(p, require, character.only = TRUE)
}

packageList <- c("rvest", "R2HTML")

usePackage(packageList)

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
jobs <- cbind.data.frame(t(matrix(links.main, nrow = 3)), paste("http://www.stat.ufl.edu/jobs/", urls.main, sep = ""))
colnames(jobs) <- c("University/Company", "Position Title", "Date", "Link")

for(i in 2:max(pagenums)) {

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
  jobs <- rbind.data.frame(jobs, jobs.temp)

}

head(jobs)

