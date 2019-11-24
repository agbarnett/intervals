# 0_find_abstracts.R
# sample of abstracts over time from specific journals
# search details here https://www.nlm.nih.gov/bsd/mms/medlineelements.html
# Nov 2018
library(dplyr)
library(ggplot2)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
library(rentrez)
library(stringr)
library(XML)
pp.year = 50 # papers per journal per year

# get Pubmed IDs from selected journals for last 10 years
# restrict to journal articles and reviews
# this does include articles with no abstract
types = c('Journal Article','Clinical Trial','Meta-Analysis','Review','Randomized Controlled Trial','Multicenter Study') # article types to include
types.search = paste(paste(types, '[PT]', sep=''), collapse=' OR ', sep='')
journals = c('Ann Intern Med','BMJ','Lancet','JAMA','N Engl J Med','Environ Health Perspect','Epidemiology','PLoS Med') # first list of journals 
journals = c('Am J Epidemiol','PLoS ONE','BMC Health Serv Res') # second list of journals
numbers = ids = NULL
for (j in journals){
  for (year in 2009:2018){
    query = paste('"', j, '"[SO] AND ', year, '[PDAT] AND (', types.search , ')', sep='')
    journal.search = entrez_search(db='pubmed', term=query, retmax=50000)
    # frame of numbers, store for flow diagram
    nframe = data.frame(journal=j, year=year, count=journal.search$count) 
    numbers = rbind(numbers, nframe)
    # frame of IDS
    frame = data.frame(journal=j, year=year, pubmed=journal.search$ids)
    ids = rbind(ids, frame)
  }
}

# quick plot
pplot = ggplot(data=numbers, aes(x=year, y=count, col=factor(journal)))+
  geom_point()+
  geom_line()+
  scale_color_manual('Journal', values=cbPalette)+
  ylab('Number of abstracts')+
  xlab('Year')+
  guides(color=guide_legend(ncol=2))+
  theme_bw()+
  theme(legend.position=c(0.3,0.84), panel.grid.minor = element_blank())
pplot
jpeg('figures/Available.Abstracts.Time2.jpg', width=6, height=5, units='in', res=500)
print(pplot)
dev.off()

# loop through journals and years again
n.truncated = 0
meta = NULL
for (j in journals){
  for (y in 2009:2018){
    # filter pubmed ids to a single journal and year
    to.search = dplyr::filter(ids, journal==j, year==y) %>%
      dplyr::mutate(rand = runif(n=n())) %>%
      arrange(rand) %>% # randomly order rows so that included articles are a random sample and spread over the year
      select(-rand)
    filled = 0
    for (i in 1:nrow(to.search)){
      rec = parse_pubmed_xml(entrez_fetch(db="pubmed", id=to.search$pubmed[i], rettype="xml")) # fetch and parse
      if(length(names(rec))==0){next} # empty result, so skip to next in for loop
      abstract = paste(rec$abstract, collapse=' ') # one bunch of text for abstract
      if(nchar(abstract)==0){next} # empty abstract (e.g., news article), so skip to next in for loop
      if(str_detect(pattern='ABSTRACT TRUNCATED', string=abstract) == TRUE){n.truncated = n.truncated +1 ; cat('pmid=', to.search$pubmed[i], '\n'); next} # abstract was truncated, so exclude
      # store because it has an abstract
      frame = data.frame(journal=j, year=y, pubmed=to.search$pubmed[i])
      meta = rbind(meta, frame)
      filled = filled + 1 # increase count
      if(filled == pp.year){break} # next loop as journal/year quota is filled
    }
  } # end of year loop
  cat('Up to journal ', j, '.\n', sep='')
} # end of journal loop
cat('There were ', n.truncated, ' truncated abstract.\n', sep='')

# remove duplicates
meta = dplyr::filter(meta, duplicated(pubmed)==F)
# quick check
with(meta, table(journal, year))

# save
date.searched = Sys.Date()
save(meta, date.searched, file='data/meta.CI.2.RData')
