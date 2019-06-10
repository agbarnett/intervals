# Examining published confidence intervals
Examining confidence intervals in Medline abstracts and full-texts.

The data `Georgescu.Wren.RData` has 1,320,226 rows and the following eight variables:
* "pubmed", pubmed ID number 
* "journal", journal name
* "lower", lower confidence limit  
* "mean", mean    
* "upper", lower confidence limit   
* "Year", year published    
* "source", abstract or full-text
* "mistake", logical flag of whether it is a mistake because the mean is outside the limits

The Rmarkdown file `4_main_analysis_GW.Rmd` performs all the analyses used in the paper, which are predominantly empirical cumulative density plots.

The R file `1_find.intervals.R` is the code used to extract confidence intervals from abstracts written by me. This validates the more comprehensive code by Jonathan Wren, available here https://github.com/jdwren/ASEC.
