# Load packages
library(dplyr)
library(rvest)

# Function to scrape url
scrapeUrl <- function(num){
  
  message(sprintf("Scraping page %d... ", num), appendLF = FALSE)
  
  tryCatch({
    # Scrape tables
    url <- sprintf("http://www.baseballprospectus.com/odds/highcharts/simsearch.php?simNum=%d", num)
    html <- read_html(url)
    nodes <- html_nodes(html, xpath = "body/table")
    tables <- html_table(nodes, fill = TRUE, header = FALSE)
    
    # Format tables
    tables.combined <- rbind_all(tables)
    colnames(tables.combined) <- c("team", "wins")
    table.cleaned <- filter(tables.combined, !is.na(wins))
    
    # Return table
    message("suceeded")
    return(table.cleaned)
    
  }, error = function(e){
    message("failed")
    warning(sprintf("Page %d failed", num))
    return(NULL)
  })
}

# Scrape urls
projections <- vector("list", 1e6)
for (i in 1:1e6){
  projections[[i]] <- scrapeUrl(i)
}

# Combine projections into one data frame
projections.combined <- as.data.frame(data.table::rbindlist(projections))
