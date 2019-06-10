# 1_find.intervals.R
# get data from abstracts that have confidence intervals
# Dec 2018
library(rentrez)
source('1_my_rentrez_key.R') # not shared on github
set_entrez_key(my.api.key) 
library(XML)
library(stringr)
library(stringi) # for str_locate_last

#######################################
### Section 0: functions used later ###

# a) function used later to remove numbers after the word "per" as in '10 per 10,000' ...
# ... also removes two numbers after range or IQR
remove.text = function(intext, w, remove.range){
  if(length(w)>0){
    to.remove = NULL
    for(s in 1:remove.range){
      to.remove = c(to.remove, w + s)
    }
    intext = intext[1:length(intext) %in% to.remove ==F] # remove after per
  }
  return(intext)
}
# test.text = c('range','1','2','3','iqr','4','5','6','per','10000','9','','range','7','8') # text to test function below
remove.per = function(intext){
  intext = intext[intext!='']
  w = which(intext=='per')
  intext = remove.text(intext, w=w, remove.range=1)
  w = which(intext=='range')
  intext = remove.text(intext, w=w, remove.range=2)
  w = which(intext=='iqr')
  intext = remove.text(intext, w=w, remove.range=2)
  return(intext)
}
# b) function to make word-search combinations
make.combs = function(words){
  # different ways the key-words can appear in the text
  text.patterns = c('\\[x\\]','\\(x\\)','-x ',' x ',' x,',' x=',' x:',' x\\.','\\(x ','\\(x:','\\[x ','\\[x:') 
  # re-order words from long to short phrases (helps with substrings after searching)
  order = order(-nchar(words))
  words = words[order]
  #
  pattern = NULL
  for (w in words){
    single = str_replace_all(text.patterns, pattern='x', replacement = w)
    plural = str_replace_all(text.patterns, pattern='x', replacement = paste(w, 's', sep='')) # add 's' to make plural version
    pattern = c(pattern, single, plural)
  }
  pattern = paste(pattern, sep='', collapse='|')
  return(pattern)
}

######################################################
### Section 1: create search patterns to find confidence intervals and other in abstracts ###

## all possible patterns of percents to use in later search
number.pattern = NULL
max.integers = 8
for (pre in 0:max.integers){ # pre-decimal place integers
  min.post = 0
  if(pre==0){min.post==1}
  for(post in min.post:max.integers){ # post-decimal place integers
    if(pre>0){pre.nums = paste(rep('[0-9]', pre), collapse = '')}
    if(pre==0){pre.nums=' '} # space to capture, for example 'was .9%' 
    if(post>0){post.nums = paste(rep('[0-9]', post), collapse = '')}
    if(post==0){post.nums=''}
    if(post==0){
      this.pattern = paste(c(pre.nums, '%'), collapse = '') # version wihtout space
    }
    if(post>0){
      this.pattern = paste(c(pre.nums, '\\.', post.nums), collapse = '')
    }
    if(pre+post>0){number.pattern = c(number.pattern, this.pattern)} 
  }
}
string.order = order(-nchar(number.pattern)) # large to small
number.pattern = number.pattern[string.order]
number.pattern = paste(number.pattern, collapse='|') # add to patterns using 'OR'

### commas in large numbers, used later to remove commas to make numbers easier to recognise
in.front = c(' ','-','\\(','\\[',',','\\.') # what's in front of large number, space, negative or brackets
commas.in.large.numbers = NULL
options(scipen = 999) # avoid scientific numbers
large.numbers = format(10^(3:9), big.mark=',') # all potential large numbers with commas
for (m in rev(large.numbers)){ # longest character length to shortest
  to.add = gsub(' ','', m) # remove spaces
  to.add = gsub('[0-9]','\\[0-9\\]', to.add) # change actual numbers to ranges
  for (i in in.front){
    this.add = paste(i, to.add, sep='') # version with negative sign 
    commas.in.large.numbers = c(commas.in.large.numbers, this.add) # version without decimals (shouldn't need decimals)
  }
}
commas.in.large.numbers = paste(commas.in.large.numbers, collapse='|') # turn into one string of or's
# spaces in large numbers, only with thousands on the ends
# two examples with spaces: 29282272, 29249478
spaces.in.large.numbers = NULL
for (m in rev(large.numbers)){ # longest character length to shortest
  to.add = gsub(' ','', m) # remove spaces
  to.add = gsub('[0-9]','\\[0-9\\]', to.add) # change actual numbers to ranges
  to.add = gsub('\\[0-9\\]\\[0-9\\]\\[0-9\\]$', '000', to.add) # replace last three numbers with thousands
  to.add = gsub(',', ' ', to.add) # replace comma with space
  for (i in in.front){
    this.add = paste(i, to.add, sep='') # also add negative sign
    spaces.in.large.numbers = c(spaces.in.large.numbers, this.add) # version without decimals (shouldn't need decimals)
  }
}
spaces.in.large.numbers = paste(spaces.in.large.numbers, collapse='|') # turn into one string of or's
### commas in small numbers, used later to replace commas with decimals to make numbers easier to recognise
startings = c('-','\\(',' ')
endings = c(' ',',','\\.',';','\\)','\\%','-') # endings after the number
small.numbers = c('[0-9],[0-9]', '[0-9],[0-9][0-9]') # with space at start
commas.in.small.numbers = NULL
for (m in rev(small.numbers)){ # longest character length to shortest
  for (s in startings){
    this = paste(s, m, endings, sep='')
    commas.in.small.numbers = c(commas.in.small.numbers, this) # version without decimals (shouldn't need decimals)
  }
}
commas.in.small.numbers = paste(commas.in.small.numbers, collapse='|') # turn into one string of or's

# Confidence interval patterns (with spaces and lower/upper cases)
# 11 is a dummy code for a missing number
levels = c(11,80,90,95,99) # currently commmon numbers, could replace with two integers [0-9][0-9]?
# ci and pi with space after (plus plurals) to avoid picking up words that start with these two letters
# typo 'uncertainly' from PMID:29171811
# 'uncertainty range' from PMID:28045934
ci.phrases = c(
  'UR','CrI','Cl','CL','CI','UI','UR','PI','CIs','UIs','PIs','CI:','UI:','PI:','CI,','UI,','PI,','CL\\.','CI\\.','UI\\.','PI\\.','CLs\\.','CIs\\.','UIs\\.','PIs\\.',
  'cl','ci','ic','ui','pi','cls','cis','uis','pis','cl:','ci:','ui:','ur:','pi:','ci,','ui,','ur,','pi,','cl\\.','ci\\.','ui\\.','ur\\.','pi\\.','cls\\.','cis\\.','uis\\.','urs\\.','pis\\.',
  'Confidence Interval','Posterior Interval','Credibility Interval','Prediction Interval','Credible Interval','Bayesian Credible Interval','Bayesian Interval','Confidence Limit','Uncertainty Interval','Uncertainly Interval','Uncertainty Range', # both capitals
  'Range','Confidence interval','Posterior interval','Credibility interval','Prediction interval','Credible interval','Bayesian credible interval','Bayesian interval','Confidence limit','Uncertainty interval','Uncertainly interval', 'Uncertainty range', # first capital
  'range','confidence interval','posterior interval','credibility interval','prediction interval','credible interval','bayesian credible interval','bayesian interval','confidence limit','uncertainty interval','uncertainly interval', 'uncertainty range') # no capitals
ci.phrases = unique(tolower(ci.phrases)) # only need lower case for this project
# re-order from long to short strings
string.order = order(-nchar(ci.phrases))
ci.phrases = ci.phrases[string.order]
ci.pattern.spaces = NULL
for (p in ci.phrases){ # 
  for (l in levels){ # 
    this.pattern = paste(l, '%', p, collapse = '', sep=' ') # space before and after percent
    ci.pattern.spaces = paste(c(ci.pattern.spaces, this.pattern), collapse='|') #
    this.pattern = paste(l, '% ', p, collapse = ' ', sep='')  # space after percent
    ci.pattern.spaces = paste(c(ci.pattern.spaces, this.pattern), collapse='|') # add to patterns using 'OR'
    this.pattern = paste(l, '%', p, collapse = ' ', sep='')  # no space after percent
    ci.pattern.spaces = paste(c(ci.pattern.spaces, this.pattern), collapse='|') # add to patterns using 'OR'
  }
}
# add a few more (found in abstracts)
annals.patterns = c('\\(ci,', '\\(ci ', '\\[ci ', '\\[ci,',' ci ',' ci,',',ci ')
ci.pattern.spaces = paste(c(ci.pattern.spaces, annals.patterns), collapse='|') # for Annals
ci.pattern.spaces = paste(c(ci.pattern.spaces, 'ci 95%', 'ci95%'), collapse='|') # reversed wording from 28228447 and 29176802
ci.pattern.spaces = paste(c(ci.pattern.spaces, '95% cl'), collapse='|') # L instead of I from PMID:28665786 
ci.pattern.spaces = paste(c(ci.pattern.spaces, '95%-ci'), collapse='|') # with dash from PMID:29155891 
ci.pattern.spaces = paste(c(ci.pattern.spaces, 'ic 95%', 'ic95%', '95% ic'), collapse='|') # ! from PMID:28222175 and PMID:28245252
ci.pattern.spaces = paste(c('95 percent confidence interval', '95 percent ci'), ci.pattern.spaces , collapse='|') # from 2821396 and 20442430
  
######################################################
### Section 2: create search patterns and other text things used by the later formating and search (only need lower case) ###
## i) Odds ratios
or.words = c('OR','AOR','aOR','odds ratio','Odds ratio','Odds Ratio', # different words for odds ratio
             'Adjusted odds ratio', 'Adjusted odds ratios', 'Adjusted-OR', 'Adj OR',
             'adjusted odds ratio', 'adjusted odds ratios', 'adjusted-OR', 'adj OR')
or.words = unique(tolower(or.words)) # only need lower case because we first convert abstract text to lower case
or.pattern = make.combs(or.words)
## ii) Relative risks
rr.words = c('RR','RRR','risk ratio','Risk ratio','Risk Ratio', # different words for risk ratio
             'relative risk', 'Relative risk', 'Relative Risk')
rr.words = unique(tolower(rr.words))
risk.pattern = make.combs(rr.words)
## iii) Hazard ratios
hazard.words = c('HR','hazard ratio','Hazard ratio','Hazard Ratio','aHRR') # different words for hazard ratio; aHRR from 28813444
hazard.words = unique(tolower(hazard.words))
hazard.pattern = make.combs(hazard.words)
## iv) Area under curve
auc.words = c('AUC','area under curve','Area Under Curve','Area under curve','AUROC') # different words for AUC; AUROCs from 28813448
auc.words = unique(tolower(auc.words))
auc.pattern = make.combs(auc.words)

# b) units of measurement
units.list = c('million','billion','thousand','hundred','mmol/L','kg/m2','pg/ml','g/ml','g2/m','mmhg','mmHg','cm3','cm2','pm','mm','cm','in','m','h','s','g','x') # longest to shortest character; 'x' from 28813424
post.punc = c(' ',',',';','\\.') # punctuation after units
for (u in units.list){
  this = paste('[0-9]', u, post.punc, sep='', collapse='|') # number followed by units with no space, but space,comma or semi-colon after unit
  units = c(units, this)
}
units = paste(units, sep='', collapse='|') 

# two other things
what.to.split = ",|=|\\%|,|;|:| |~|\\)|\\(|\\]|\\[|\\{|\\}" # whole load of characters for what to split numbers on 
words.to.search = 10 # number of words to search before and after confidence interval

# c) hyphen patterns (longest to shortest character)
hyphen.patterns = '[0-9]\\% - [0-9]|[0-9] - [0-9]|[0-9]\\%-[0-9]|-fold|[0-9]-[0-9]|[0-9]-\\.'

# d) n-patterns where people have given sample size (gets in the way, e.g., 29149211); no need for commas in numbers because they should have been replaced
n.patterns = NULL
for (number in 6:1){ # order from largest to smallest number so that first match is largest character
    this = paste(c(',n=',',n =',',n= ',',n = ', # four spacing patterns with comma
                   ';n=',';n =',';n= ',';n = ', # four spacing patterns with semi-colon
                   '\\.n=','\\.n =','\\.n= ','\\.n = ', # four spacing patterns with full stop
                   ' n=',' n =',' n= ',' n = ' # four spacing patterns with space
                   ), paste(rep('[0-9]', number), collapse=''), sep='') # combinations of spacing and numbers
    n.patterns = c(n.patterns, this)
}
n.patterns = paste(n.patterns, collapse='|') # turn into one string of or's

# e) P-value patterns where people have the p-value (gets in the way)
p.patterns = NULL
p.wording = c('p-value','pvalue','p value','pval','p')
equalities = c(' = ', ' =','= ','=',' < ', ' <','< ','<') # two equality types with spacing arrangements
for (word in p.wording){
  for (number in 4:1){ # order from largest to smallest number so that first match is largest character
    for (e in equalities){
      this = paste(word, e, '[0-9].', paste(rep('[0-9]', number), collapse='', sep = ''), sep = '') 
      p.patterns = c(p.patterns, this)
    }
  }
}
p.patterns = paste(p.patterns, collapse='|') # turn into one string of or's

# f) plus/minus patterns with SEMs or SDs, gets in the way of mean and CI
pm.patterns = NULL
pm.pattern = c(' Â± ',' Â±','Â± ','Â±') # combinations of pm-symbol with spaces
for (word in pm.pattern){
  for (pre.decimal in 4:0){ # order from largest to smallest number so that first match is largest
    for (post.decimal in 4:0){ # order from largest to smallest number so that first match is largest
      decimal = ifelse(post.decimal==0, '', '.')
      this = paste(word, paste(rep('[0-9]', pre.decimal), collapse='', sep = ''), decimal, paste(rep('[0-9]', post.decimal), collapse='', sep = ''), sep = '') 
      pm.patterns = c(pm.patterns, this)
    }
  }
}
order = order(-nchar(pm.patterns))
pm.patterns = pm.patterns[order] # order from largest to smallest
pm.patterns = paste(pm.patterns, collapse='|') # turn into one string of or's

# g) SEMs or SDs, gets in the way of mean and CI, e.g. 29175276
sem.patterns = NULL
sem.patterns = c('\\[se ','\\[sem ','\\[sd ','\\(se ','\\(sem ','\\(sd ','\\.se ','\\.sem ','\\.sd ','\\,se ','\\,sem ','\\,sd ') # 
for (word in sem.patterns){
  for (pre.decimal in 4:0){ # order from largest to smallest number so that first match is largest
    for (post.decimal in 4:0){ # order from largest to smallest number so that first match is largest
      decimal = ifelse(post.decimal==0, '', '.')
      this = paste(word, paste(rep('[0-9]', pre.decimal), collapse='', sep = ''), decimal, paste(rep('[0-9]', post.decimal), collapse='', sep = ''), sep = '') 
      sem.patterns = c(sem.patterns, this)
    }
  }
}
order = order(-nchar(sem.patterns))
sem.patterns = sem.patterns[order] # order from largest to smallest
sem.patterns = paste(sem.patterns, collapse='|') # turn into one string of or's

# h) remove abbreviation of "confidence interval" as double-wording causes confusion
long = c('confidence intervals','confidence interval')
short = c('cis','ci')
punctuation = list(c(' \\(','\\)'), c(' \\[','\\]'), c(' ,',''), # with space
                   c('\\(','\\)'), c('\\[','\\]'), c(',',''), c(', ','') ) # variants of punctuation
ci.abbreviation.patterns = NULL
for (l in long){
  for (s in short){
    for (p in 1:length(punctuation)){
      this.p = punctuation[[p]]
      to.add = paste(l, this.p[1], s, this.p[2], sep='')
      ci.abbreviation.patterns = c(ci.abbreviation.patterns, to.add)
    }
  }
}
order = order(-nchar(ci.abbreviation.patterns))
ci.abbreviation.patterns = ci.abbreviation.patterns[order] # order from largest to smallest
ci.abbreviation.patterns = paste(ci.abbreviation.patterns, collapse='|') # turn into one string of or's

# i) currencies without spaces to numbers, e.g., 29233795
currencies = c('aud','gbp','usd','eur','jpy','chf') # six major currencies
currency.patterns = paste(' ', currencies, '[0-9]', sep='', collapse='|') # turn into one string of or's

######################################################
### Section 3: find eligible abstracts from pubmed ###

# get Pubmed IDs from selected journals in 2017; restrict to journal articles and reviews
# i) 50 per year from selected health/medical journals over time:
#load('data/meta.CI.RData') # from 0_find_abstracts.R (first batch)
#load('data/meta.CI.2.RData') # from 0_find_abstracts.R (second batch)
#ofile = 'trend'
# ii) data from decimal places study (used to develop extraction)
#load('../decimal.places/journal.meta.RData') # F1000, PLOS, Nature
#load('../decimal.places/journal.meta.ii.RData') # 
#ofile = 'test'
# iii) verify using data from 10.1093/bioinformatics/btx811
#load('data/Georgescu.Wren.RData') # from 1_data_from_Georgescu_Wren.R
#ofile = 'GWtest'
# iv) stratified sample for last 10 years
load('data/meta.numbers.RData') # from 0_find_abstracts_pubmed_numbers.R
ofile = 'tenyears'
# v) stratified sample for last 10 years for medical journals
#load('data/meta.medical.RData') # from 0_wikipedia_medicine.R
#ofile = 'tenyearsMedical'
# vi) random check of 50 abstracts
random.check = FALSE
if(random.check==TRUE){
  ofile = 'RandomCheck'
  load('data/meta.CI.RData') # from 0_find_abstracts.R (first batch of journals)
  temp = meta
  load('data/meta.CI.2.RData') # from 0_find_abstracts.R (second batch of journals)
  meta = rbind(meta, temp); remove(temp)
  meta = dplyr::sample_n(meta, size=50, replace = FALSE)
  # randomly select 50
}
# remove a few from the data; these need to be done by hand (see 1_individual_data_edits.R) ...
# ... or find a code 
# non-standard presentation of mean confidence interval, e.g., odds ratio using "odds ratio (95% confidence interval)"
# or some other non-standard wording, e.g., "(-0·23 points on a 100 point scale, 95% CI -0·56 to 0·14" 27955788
non.standard = unique(c('29267320','29261666','29267301','29220386','29176785','28732090','28750095',
                 '29229653','29229653','28719663','28360027','28632867','29037960','28729326',
                 '27955788','29116930','29074098','29116930','29020583','28827263','28771610',
                 '29121045',# all sent to Ruth
                 '29272278','29261792','29261724','29190670','29176889','29176842','29176802','29176785','29166388',
                 '29121660'))
source('1_output.non.standard.R') # export for separate data entry
meta = dplyr::filter(meta, pubmed %in% non.standard == F) 

############################################
### Section 4: extract CIs from abstract ###

# now loop one article at a time and extract data from abstract
data = any.data = NULL # 
for (a in 99884:nrow(meta)){ # can do shorter runs than nrow(meta) if online connection to pubmed breaks

  # get abstract
  rec <- parse_pubmed_xml(entrez_fetch(db="pubmed", id=meta$pubmed[a], rettype="xml")) # fetch and parse
  if(length(names(rec))==0){next} # empty result, so skip to next in for loop
  abstract = paste(rec$abstract, collapse=' ') # one bunch of text for abstract
  if(nchar(abstract)==0){next} # empty abstract (e.g., news article), so skip to next in for loop
  # can get rec$key_words, but not available for all papers

  # get date of paper
  date = strsplit(entrez_summary(db='pubmed', id=meta$pubmed[a])$sortpubdate, split=' ')[[1]][1]
  
  # for abstract remove plus/minus symbol plus number as we don't want these SDs to get confused with CIs
  where.pm = str_locate_all(pattern=pm.patterns, abstract)[[1]]
  while(length(where.pm)>0){
    start = str_sub(abstract, 1, where.pm[1,1]-1)
    end = str_sub(abstract, where.pm[1,2]+1, nchar(abstract))
    abstract = paste(c(start, ' ', end), sep='', collapse='') 
    where.pm = str_locate_all(pattern=pm.patterns, abstract)[[1]] # update
  }

  # formatting abstract
  #abstract = gsub('â€‰', '', abstract) # remove non-separating space, usually used between numbers - does not work, used iconv below instead
  Encoding(abstract) = 'UTF-8' # change encoding because of things like non-separating spaces 
  abstract = gsub('\\+', ' ', abstract) # replace every '+' with space 29260226
  abstract = gsub('--', '-', abstract) # remove double hyphen 11870509
  abstract = gsub('·', '.', abstract) # use a consitent decimal place
  abstract = gsub('US\\$|\\$|£|€|¥', '', abstract) # remove common monetary units
  abstract = iconv(abstract, from="UTF-8", to="ASCII", sub='') # now convert to ASCII and remove any codes; removes Greek letters and non-separting spaces
  abstract = str_replace_all(pattern=' or | or,', replace=' xx ', string=abstract) # convert word 'or' (lower case) to 'xx' in order to avoid later confusion with OR for odds ratio
  abstract = str_replace_all(pattern='\\. CI|, CI|; CI|\\(CI|\\[CI', replace=' 11\\% CI', string=abstract) # convert lone CI to '11% CI' so that it is detected by later code
  abstract = tolower(abstract) # convert to lower case
  # replace comma ci with space; also squashed p-value (because of 29240791)
  abstract = str_replace_all(pattern='p=|p<', replace=' p=', string=abstract)
  abstract = str_replace_all(pattern=',95%ci', replace='; 95%ci', string=abstract)
  abstract = str_replace_all(pattern=',95% ci', replace='; 95% ci', string=abstract)
  # ensure space after equals (because of 28864687)
  abstract = str_replace_all(pattern='=', replace='= ', string=abstract)
  # remove 24-hr because 'hr' gets confused with 'hazard ratio' (e.g., 15891328)
  abstract = str_replace_all(pattern='24-hr|24hr|24 hr|24-hour|24hour|24 hour', replace='', string=abstract)
  
  # fix common break between lower and upper CI of a hyphen (can also be percent on first number)
  # abstract='1-2, 3 - 4, 5 -6, 7%-8%, 9% - 10%' # test code, want to keep 'minus' 6
  where.hyphen = str_locate_all(pattern=hyphen.patterns, abstract)[[1]]
  while(length(where.hyphen)>0){
    start = str_sub(abstract, 1, where.hyphen[1,1]-1) # from start to where hyphen is 
    middle = str_sub(abstract, where.hyphen[1,1], where.hyphen[1,2]) # bit with hyphen
    end = str_sub(abstract, where.hyphen[1,2]+1, nchar(abstract)) # from where hyphen is to end
    abstract = paste(c(start, str_replace(middle, pattern = '-', replacement = ', '), end), sep='', collapse='') # replace hyphen with comma-space
    where.hyphen = str_locate_all(pattern=hyphen.patterns, abstract)[[1]] # update
  }
  # fix lack of space between units and number
  where.units = str_locate_all(pattern=units, abstract)[[1]] # redo find process because of extra spaces
  while(length(where.units)>0){
    start = str_sub(abstract, 1, where.units[1,1]) # from start to where units are 
    end = str_sub(abstract, where.units[1,1]+1, nchar(abstract)) # from where units are to end
    abstract = paste(c(start, ' ', end), sep='', collapse='') # replace units with space
    where.units = str_locate_all(pattern=units, abstract)[[1]] # redo find process because of extra spaces
  }
  # make sure there's punctuation between two numbers, or full-stop space and number
  where.nums = str_locate_all(pattern='[0-9] [0-9]|\\. [0-9]', abstract)[[1]]
  while(length(where.nums)>0){
    start = str_sub(abstract, 1, where.nums[1,1])
    end = str_sub(abstract, where.nums[1,2], nchar(abstract))
    abstract = paste(c(start, ',', end), sep='', collapse='') 
    where.nums = str_locate_all(pattern='[0-9] [0-9]|\\. [0-9]', abstract)[[1]] # update
  }
  # make sure there's space when 'to' is used to split intervals; because of 28753680
  where.to = str_locate_all(pattern='[0-9]to [0-9]|[0-9] to[0-9]|[0-9]to[0-9]', abstract)[[1]]
  while(length(where.to)>0){
    start = str_sub(abstract, 1, where.to[1,1])
    end = str_sub(abstract, where.to[1,2], nchar(abstract))
    abstract = paste(c(start, ',', end), sep='', collapse='') 
    where.to = str_locate_all(pattern='[0-9]to [0-9]|[0-9] to[0-9]|[0-9]to[0-9]', abstract)[[1]]
  }
  # fix lack of space between currencies and number units
  where.currency = str_locate_all(pattern=currency.patterns, abstract)[[1]] # redo find process because of extra spaces
  while(length(where.currency)>0){
    start = str_sub(abstract, 1, where.currency[1,1]+3) # from start to where currencies are 
    end = str_sub(abstract, where.currency[1,2], nchar(abstract)) # from where currencies are to end
    abstract = paste(c(start, ' ', end), sep='', collapse='') # add space
    where.currency = str_locate_all(pattern=currency.patterns, abstract)[[1]] # redo find process because of extra spaces
  }
  
  # remove commas in small numbers 
  # abstract = "83,2% (95%CI 66-92.7%) " # this is test code from 28926630
  where.commas = str_locate_all(pattern=commas.in.small.numbers, abstract)[[1]]
  while(length(where.commas)>0){
    start = str_sub(abstract, 1, where.commas[1,1]-1) # from start to where number is 
    middle = str_sub(abstract, where.commas[1,1], where.commas[1,2]-1) # bit with comma
    end = str_sub(abstract, where.commas[1,2], nchar(abstract)) # from where number is to end
    abstract = paste(c(start, str_replace(middle, pattern = ',', replacement = '.'), end), sep='', collapse='') # replace first comma with decimal
    where.commas = str_locate_all(pattern=commas.in.small.numbers, abstract)[[1]] # update
    # quick check
    if(str_count(pattern=',', middle)>1){cat('More than one comma for pubmed=',rec$pmid,', text=',middle,'\n')}
  }
  # remove commas in large numbers 
  # abstract = "1,000 100,000.00 1000 0.75,0.76" # this is test code
  where.commas = str_locate_all(pattern=commas.in.large.numbers, abstract)[[1]]
  while(length(where.commas)>0){
    start = str_sub(abstract, 1, where.commas[1,1]-1) # from start to where number is 
    middle = str_sub(abstract, where.commas[1,1], where.commas[1,2]) # bit with comma
    end = str_sub(abstract, where.commas[1,2]+1, nchar(abstract)) # from where number is to end
    abstract = paste(c(start, ' ', str_replace_all(middle, pattern = ',', replacement = ''), end), sep='', collapse='') # remove all commas
    where.commas = str_locate_all(pattern=commas.in.large.numbers, abstract)[[1]] # update
  }
  # remove spaces in large numbers 
  where.spaces = str_locate_all(pattern=spaces.in.large.numbers, abstract)[[1]]
  while(length(where.spaces)>0){
    start = str_sub(abstract, 1, where.spaces[1,1]-1) # from start to where number is 
    middle = str_sub(abstract, where.spaces[1,1], where.spaces[1,2]) # bit with space
    end = str_sub(abstract, where.spaces[1,2]+1, nchar(abstract)) # from where number is to end
    abstract = paste(c(start, ' ', str_replace_all(middle, pattern = ' ', replacement = ''), end), sep='', collapse='') # remove all spaces
    where.spaces = str_locate_all(pattern=spaces.in.large.numbers, abstract)[[1]] # update
  }
  
  ## remove "(CI)" after first mention of "confidence interval"
  abstract = str_replace_all(string=abstract, pattern=ci.abbreviation.patterns, replacement='confidence interval ')
  ## remove double mentions of CIs, e.g., "95% confidence interval, 95% CI", because double mentions fox the extraction process below, example 29145407
  individual.words = strsplit(abstract, split=' |\\(|\\)|,|;')[[1]]
  individual.words = individual.words[individual.words != ''] # remove empty words
  index = grep('confidence|interval|80\\%|90\\%|95\\%|99\\%|ci', individual.words) # just look for typical CI levels
  # find four mentions in a row, or five with a gap
  true.false = paste(as.numeric(1:length(individual.words)%in%index), collapse='') 
  mentions = str_locate_all(string = true.false, pattern='1111|11011|10111|11101')[[1]][,1] # just starts
  # remove two words to avoid double detection
  if(length(mentions)>0){
    for (q in mentions){
      individual.words = individual.words[1:length(individual.words) %in% c(q, q+1) == FALSE] # 
    }
  }
  abstract = paste(individual.words, collapse=' ') # put abstract back together
  
  # remove 'n = ...', because some people add sample size between mean and CI, e.g., PMID = 29149211
  where.n = str_locate_all(pattern=n.patterns, abstract)[[1]]
  while(length(where.n)>0){
    start = str_sub(abstract, 1, where.n[1,1]-1)
    end = str_sub(abstract, where.n[1,2]+1, nchar(abstract))
    abstract = paste(c(start, ' ', end), sep='', collapse='') 
    where.n = str_locate_all(pattern=n.patterns, abstract)[[1]] # update
  }
  # remove 'p = ...', because some people add p-value between mean and CI, e.g., PMID = 30339671
  where.p = str_locate_all(pattern=p.patterns, abstract)[[1]]
  while(length(where.p)>0){
    start = str_sub(abstract, 1, where.p[1,1]-1)
    end = str_sub(abstract, where.p[1,2]+1, nchar(abstract))
    abstract = paste(c(start, ' ', end), sep='', collapse='') 
    where.p = str_locate_all(pattern=p.patterns, abstract)[[1]] # update
  }
  # remove SEM/SD/SE because some people add these between mean and CI, e.g., PMID = 29175276
  where.sem = str_locate_all(pattern=sem.patterns, abstract)[[1]]
  while(length(where.sem)>0){
    start = str_sub(abstract, 1, where.sem[1,1]-1)
    end = str_sub(abstract, where.sem[1,2]+1, nchar(abstract))
    abstract = paste(c(start, ' ', end), sep='', collapse='') 
    where.sem = str_locate_all(pattern=sem.patterns, abstract)[[1]] # update
  }

  ## other odd spaces needed  
  # make sure there's a space before the confidence interval, e.g., 29287087
  abstract = str_replace_all(string=abstract, pattern='\\[95% ci|\\(95% ci', replacement=' 95% ci')
  # make sure there's a space after the confidence interval, e.g., 29261726, 29161285
  abstract = str_replace_all(string=abstract, pattern='95%ci:|95% ci:', replacement=' 95% ci ')
  # make sure there's a space in :-, e.g., 29244837
  abstract = str_replace_all(string=abstract, pattern=':-|\\.-', replacement=' -')
  
  # look for confidence intervals
  ci.places = str_locate_all(pattern=ci.pattern.spaces, string=abstract)[[1]] # all patterns, including annals
  ci.places.annals = str_locate_all(pattern=paste(annals.patterns, collapse='|'), string=abstract)[[1]] # 
  if(nrow(ci.places) == 0){ # no CIs, just record that
    any.data.frame = data.frame(pubmed=rec$pmid, journal=rec$journal, date=date, any.ci=FALSE)
  }
  if(nrow(ci.places) > 0){ # if any CIs to look for
    ### XXX, may need to move this
    any.data.frame = data.frame(pubmed=meta$pubmed[a], journal=rec$journal, date=date, any.ci=TRUE)
    
    # extract confidence levels, e.g., 95%
    ci.numbers = str_replace_all(str_sub(string=abstract, ci.places), pattern='ci\\.|cis\\.|[a-z]|\\%|:|-|\\[|\\(', replace='')
    ci.numbers = suppressWarnings(as.numeric(ci.numbers)) # supress warnings for empty numbers
    ci.numbers[ci.numbers==11] = NA # replace dummy code of 11 with missing
    # if first number is not missing and others are, then assume others are same as first
    if(nrow(ci.places) > 0){
      if( (is.na(ci.numbers)[1]==FALSE) & length(unique(ci.numbers)) == 2 & any(is.na(ci.numbers)) ){ # if first is not missing, and only one level, and some are missing
        ci.numbers[is.na(ci.numbers)] = ci.numbers[1] # replace any missing with first CI level
      }
    }
#    if(any(is.na(ci.numbers))==TRUE){cat('Empty CI level for', rec$pmid, ', strings=', str_sub(abstract, ci.places), '\n')}
    # extract positions of CI phrases, this is a word count
    word.starts = str_count(str_sub(abstract, start=1, end=ci.places[,1]), ' ') # count the spaces from the start of the abstract to the START of the CI phrase
    word.ends = str_count(str_sub(abstract, start=1, end=ci.places[,2]), ' ') # count the spaces from the start of the abstract to the END of the CI phrase
    max.words = str_count(abstract, "\\w+") # count maximum number of words
    end = word.ends+words.to.search 
    end[end>=max.words] = -1 # if beyond max words, then go to last work (negative); found by 23490371
    # number of words to skip depends on type of interval (Annals-type with no interval level)
    plus = rep(2, nrow(ci.places)) 
    if(nrow(ci.places.annals)>0){
      aindex = ci.places[,1] %in% ci.places.annals[,1]
      plus[aindex] = 1
    }
    ci.text = word(abstract, start=word.ends+plus, end=end) # now take next set of words from starting location
    # extract means
    mean.text = word(abstract, word.starts-words.to.search, word.starts-0) # words up to CI phrase; assumes mean comes before CI
    # loop through instances
    order = 0 # number the intervals in order of occurence in the text
    for (i in 1:nrow(ci.places)){
      # remove any number after 'per' and any two numbers after 'range'; also split into words
      ci.words = remove.per(strsplit(ci.text[[i]], split=what.to.split)[[1]])
      mean.words = remove.per(strsplit(mean.text[[i]], split=what.to.split)[[1]])

      # look for the type of interval based on mean text
      is.odds = str_detect(string=mean.text[[i]], pattern=or.pattern)
      is.risk = str_detect(string=mean.text[[i]], pattern=risk.pattern)
      is.hazard = str_detect(string=mean.text[[i]], pattern=hazard.pattern)
      is.auc = str_detect(string=mean.text[[i]], pattern=auc.pattern)
      # first remove CI patterns before looking for percent because otherwise get lots of false positives
      is.percent = str_detect(string=str_remove(mean.text[[i]], ci.pattern.spaces), pattern='\\%') # 

      # find next two numbers per CI
      nums = suppressWarnings(as.numeric(unlist(ci.words)))
      if(sum(is.na(nums)==FALSE) > 1){ # Any confidence intervals? (need two numbers) No numbers usually mean it was just text
        cis = nums[is.na(nums)==FALSE][1:2] # first two
        # find last number for mean
        nums = suppressWarnings(as.numeric(mean.words)) # suppressed warnings for turning 
        if(sum(is.na(nums)==FALSE) >= 1){ # any matching mean numbers
          mean = nums[is.na(nums)==FALSE]
          mean = mean[length(mean)] # last number
          order = order + 1 # order in text
          frame = data.frame(pubmed=meta$pubmed[a], journal=rec$journal, order=order,
                is.percent = is.percent, is.odds=is.odds, is.risk=is.risk, is.hazard=is.hazard, is.auc=is.auc, # all per abstract
                ci.level=ci.numbers[i], mean=mean, lower=cis[1], upper=cis[2]) # all per result
          data = rbind(data, frame)
        }
      }
    }
  } # end of any CIs
  # store data on any CIs (per abstract)
  any.data = rbind(any.data, any.data.frame)
  
  # update
  if(a%%100==0){
    cat('Up to ', a, '.\n', sep='') ## update progress
    Sys.sleep(60*10) # break for 10 mins (try to avoid over-loading pubmed)
    } 
} # end of any abstract data
  
#save(data, any.data, a, file='temp.RData') # temporary storage so work is not lost
### Section 5: final tidying and saving ###

## tidy and save
# tidy dates/pubmed
any.data$date = as.Date(as.character(any.data$date), format='%Y/%m/%d')
any.data$pubmed = as.numeric(as.character(any.data$pubmed))
data$pubmed = as.numeric(as.character(data$pubmed))
# save
ofile = paste('data/', ofile, '.RData', sep='')
save(non.standard, data, any.data, file=ofile)

