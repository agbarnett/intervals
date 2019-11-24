# 1_data_from_Georgescu_Wren.R
# get data from paper by Georgescu and Wren DOI:10.1093/bioinformatics/btx811
# used to validate my extraction algorithm
# May 2019
library(readxl)
library(dplyr)

## a) Get the numbers for the lower and upper intervals (from abstracts)
# get the raw data
asheet = read_excel('data/MEDLINE Ratio-CI errors trim.xlsx', sheet=1)
# reduce the variables; keep and rename variables to match my formats
abstract = dplyr::select(asheet, PMID, Journal, `lower CI`, `rep. R`,`upper CI`, `CI%`) %>%
  dplyr::rename('pubmed'='PMID', 'journal'='Journal', ci.level=`CI%`, lower=`lower CI`, mean=`rep. R`, upper=`upper CI`) %>%
  mutate(Year = NA, 
         source = 'Abstract',
         ci.level = ifelse(ci.level<=0 | ci.level>=1, NA, ci.level), # must be in (0,1)
                  mistake = mean<lower | mean>upper) # make mistake variable
# add pubmed year
for (year in 1976:2019){
  file = paste('data/pubmed_result', year, '.txt', sep='') # files that just contain PMIDs for each year; must have an abstract
  pyear = read.table(file, header=FALSE)
  abstract$Year[abstract$pubmed %in% pyear$V1] = year
}
plot(table(abstract$Year))
table(is.na(abstract$Year)) # check for any empty years; very small number

# version with just randomly selected unique pubmed IDs (one result per abstract for sensitivity analysis)
meta.abstract = dplyr::select(abstract, pubmed) %>%
  distinct() %>% # 
  slice(sample(1:n())) # randomly sample rows so that testing in order covers a variety of dates

## b) Get additional data from main paper
fsheet = read_excel('data/PMC Ratio-CI-pvals.xlsx', sheet=1)
fulltext = dplyr::select(fsheet, PMID, Journal, `lower CI`, `rep. R`,`upper CI`, `CI%`) %>%
  dplyr::rename('pubmed'='PMID', 'journal'='Journal', ci.level=`CI%`, lower=`lower CI`, mean=`rep. R`, upper=`upper CI`) %>%
  mutate(lower = stringr::str_replace_all(string=lower, pattern='^\\+', replacement = ''), # remove plus at start
         mean = stringr::str_replace_all(string=mean, pattern='^\\+', replacement = ''), # 
         upper = stringr::str_replace_all(string=upper, pattern='^\\+', replacement = ''), # 
         lower = stringr::str_replace_all(string=lower, pattern='\\+$', replacement = ''), # remove plus at end
         mean = stringr::str_replace_all(string=mean, pattern='\\+$', replacement = ''), # 
         upper = stringr::str_replace_all(string=upper, pattern='\\+$', replacement = ''), # 
         lower = as.numeric(lower), # convert to number
         upper = as.numeric(upper), # convert to number
         mean = as.numeric(mean), # convert to number
         ci.level = stringr::str_replace_all(string=ci.level, pattern='[a-zA-Z]|-|\\%|=|`', replacement = ''), # remove characters
         ci.level = as.numeric(ci.level),
         ci.level = ifelse(ci.level<=0 | ci.level>=1, NA, ci.level), # must be in (0,1)
         Year = NA, 
         source = 'Full-text',
         mistake = mean<lower | mean>upper) # make mistake variable
# add pubmed year
for (year in 1976:2019){
  file = paste('data/pubmed_result', year, '.txt', sep='') # files that just contain PMIDs for each year; must have an abstract
  pyear = read.table(file, header=FALSE)
  fulltext$Year[fulltext$pubmed %in% pyear$V1] = year
}
plot(table(fulltext$Year))
table(is.na(fulltext$Year)) # check for any empty years; very small number

# version with just randomly selected unique pubmed IDs (one result per fulltext for sensitivity analysis)
meta.fulltext = dplyr::select(fulltext, pubmed) %>%
  distinct() %>% # 
  slice(sample(1:n())) # randomly sample rows so that testing in order covers a variety of dates

## c) concatenate data
complete = bind_rows(abstract, fulltext)

# save
save(meta.abstract, meta.fulltext, complete, file='data/Georgescu.Wren.RData')

