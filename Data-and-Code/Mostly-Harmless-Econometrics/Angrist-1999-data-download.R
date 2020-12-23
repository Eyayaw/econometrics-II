library(rvest)
library(dplyr)

# download Angrist (1990) data archive 

url <- "https://economics.mit.edu/faculty/angrist/data1/data/angrist90"
links <- rvest::read_html(url) %>% rvest::html_nodes("#subnavinfotype li a")
files <- html_attr(links, "href")
urls <- paste0("https://economics.mit.edu", files)
nms <- stringr::str_extract(links, "\\w+\\.(dta|do|xls)")
nms <- paste0("Data-and-Code/Mostly-Harmless-Econometrics/Angrist (1990)/", nms)
Map(urls, nms, f = function(x, nm) download.file(x, nm, quiet = TRUE))
