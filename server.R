library(shiny)
library(rvest)
library(DT)


shinyServer(function(input, output) {

  output$mytable <- renderDataTable({

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
    jobs <- as.data.frame(t(matrix(links.main, nrow = 3)))
    colnames(jobs) <- c("University/Company", "Position Title", "Date")
    jobs$`University/Company` <- paste('<a href="http://www.stat.ufl.edu/jobs/', urls.main, 
                                       '" target="_blank">', jobs$`University/Company`, '</a>', sep = '')
    
    withProgress(message = 'Loading...', value = 0, {
    
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
      
      jobs.temp <- as.data.frame(t(matrix(links.temp, nrow = 3)))
      colnames(jobs.temp) <- names(jobs)
      jobs.temp$`University/Company` <- paste('<a href="http://www.stat.ufl.edu/jobs/', urls.temp, 
                                              '" target="_blank">', jobs.temp$`University/Company`, '</a>', sep = '')
      jobs <- rbind.data.frame(jobs, jobs.temp)
      
      # Increment the progress bar, and update the detail text.
      incProgress(1/max(pagenums), detail = paste("page ", i, " / ", max(pagenums)))
      
      # Pause for 0.1 seconds to simulate a long computation.
      Sys.sleep(0.1)
      
    }
  })
    
    datatable(jobs, escape = FALSE)
  }, options = list(orderClasses = TRUE))
  
})
