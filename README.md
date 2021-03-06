# Examining published confidence intervals
Examining confidence intervals in _Medline_ abstracts and full-texts.

The data `Georgescu.Wren.RData` has 1,320,226 rows and the following eight variables:
* "pubmed", pubmed ID number 
* "journal", journal name
* "lower", lower confidence limit  
* "mean", mean    
* "upper", upper confidence limit   
* "Year", year published    
* "source", abstract or full-text
* "mistake", logical flag of whether it is a mistake because the mean is outside the limits

The full data file was too large to upload in text format. The file `Georgescu.Wren.30.txt` shows the first 30 rows of the data in tab-delimited format.

The Rmarkdown file `4_main_analysis_GW.Rmd` performs all the analyses used in the paper, which are predominantly empirical cumulative density plots.

The R file `1_find.intervals.R` is the code used to extract confidence intervals from _pubmed_ abstracts written by me. This validates the more comprehensive code by Jonathan Wren, available here https://github.com/jdwren/ASEC.
