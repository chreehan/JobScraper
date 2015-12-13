
#Job Site Scraaaper

library(rvest)
library(stringr)
library(plyr)
library(dplyr)
library(ggvis)
library(knitr)
library(XML)
library(data.table)
options(digits = 4)
setwd('/Users/christophersheehan/Desktop/Data_Science/Working directory R')
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
#First run through with jobScrapeScriptbegin.js to grab all posted
#After use jobScrapeScriptcontinue.js to pick up new ones as posted
system("./phantomjs jobScrapeScriptcontinue.js")
#VPN has to be on in China
#Uses javascript adapted from http://blog.datacamp.com/scraping-javascript-generated-data-with-r/ 
# for getting past the Java on the inital site
# & utilizies phantom.js
page <- html("reed.html")
y = html_nodes(page, xpath = "//@href")
yop = as.character(y)
yop = yop[grepl('/jobs/', yop)]
#/jobs/ still containts duplicates and links to agencies
yop = unique(yop)

JobList = yop[grepl("^.+100$",yop)]
JobList = JobList[grepl("#",JobList)]
JobList = paste ('http://www.reed.co.uk', sep = '', JobList, collapse = NULL)

waitLength = runif(length(JobList), 0.5, 4)

t = length(table(JobData$RefNum))
if (exists('JobData') == FALSE) {
JobData = data.table(entryNum = 1:5000, title = ' ', location = ' ', 
                     text1 = ' ', text2 = ' ', keywords = ' ',
                     RefNum = '0', pay = ' ', time = ' ')
}


t = (length(table(JobData$RefNum)))-1
v = 0
d = 0
for (x in 1:length(JobList)) {
i = html(JobList[x])
x = x + t
refNum = html_text(html_nodes(i, xpath = '//li')[14], trim = TRUE)
refNum = substrRight(refNum, 8)
if (length((grep(refNum, JobData$RefNum, fixed = TRUE))) == 0) {
JobData$RefNum[x] = refNum
d = d + 1
jobtext = html_node(i, xpath = '//*[(@id = "jobdescription")]')
text2 = unlist(html_text(html_nodes(jobtext, 'p')))
text2 = paste(text2, collapse = ". ")
text1 = paste(html_text(html_nodes(jobtext, 'li')), collapse = '. ')
JobData$text1[x] = text1
JobData$text2[x] = text2
  # last one might be null coz not every site uses
time = html_node(i, xpath = '//time')
time = html_attr(time, 'datetime')
JobData$time[x] = time
title = html_node(i, xpath = '//h1')
title = html_text(title)
JobData$title[x] = title
location = html_node(i, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "detailLists", " " ))]//ul[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]//li[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]')
location = html_text(location, trim = TRUE)
JobData$location[x] = location
JobData$pay[x] = html_text(html_nodes(i, xpath = '//li')[10], trim = TRUE)
print(paste("Added", d, "New Jobs To A Total Of", (d+t)))

keywords = html_text(html_nodes(i, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "skill-name", " " ))]'))
if (length(keywords) == 0) {
  JobData$keywords[x] = NA
} else {
  JobData$keywords[x] = paste(keywords, collapse = ' ')
}

Sys.sleep(waitLength[d])
if (d %% 7 == 0) {
  Sys.sleep(3)
}
# Random system sleeps to avoid any chance of being picked up as a crawler
} else if (v == 0) {print('Already in table')
  v = v + 1
  }
}


olLvls = data.table(read.csv('JobData.csv'))
olLvls = length(levels(as.factor(olLvls$RefNum)))
setwd(yourwdhere)
if (length(levels(as.factor(JobData$RefNum))) > olLvls) {
  write.csv(JobData, 'JobData.csv') 
}
