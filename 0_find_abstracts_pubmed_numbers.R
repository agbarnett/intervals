# 0_find_abstracts_pubmed_numbers.R
# sample of abstracts over time (2009 to 2018 --- last decade) using pubmed numbers
# Nov 2018
library(dplyr)
library(stringr)
library(XML)

## Section 1, read in pubmed numbers
# these files come from web search of pubmed, see screen shot in data area
# search for all papers in one year, restricted to those with an abstract available (downloads took a while)
setwd('data') # move to data folder
to.load = dir(pattern='^pubmed_result') # find all the files
pubmed.numbers = NULL
for (file in to.load){
  year = as.numeric(str_remove_all(string=file, pattern = 'pubmed_result|.txt')) # extract year from filename
  numbers = read.table(file, header = FALSE)
  frame = data.frame(year=year, pubmed=numbers$V1)
  pubmed.numbers = rbind(pubmed.numbers, frame)
  remove(numbers, frame) # tidy up
}
setwd('..') # move back from data folder

# record numbers per year for table
n.stats = group_by(pubmed.numbers, year) %>%
  summarise(count = n())

# randomly sample PUBMED numbers
n = 1000 # number to sample per year
n = n * 10 # inflate sample size to account for abstracts with no intervals (based on early work)
set.seed(1234)
meta = group_by(pubmed.numbers, year) %>%
  sample_n(size=n, replace = FALSE)

# quick check for duplicates
table(table(meta$pubmed))
# remove duplicates (not expected; just in case)
index = duplicated(meta$pubmed)==FALSE # removed 5
meta = meta[index,] 
meta$year = as.numeric(as.character(meta$year))
# quick check
with(meta, table(year))

# save (used by 1_find.intervals.R)
date.searched = Sys.Date()
save(meta, n.stats, date.searched, file='data/meta.numbers.RData') 
