library(tidyverse)
library(lubridate)

#` \code{load_fares} loads pre-processed taxi fares compressed files.
#` It uses read_csv with the right arguments for column names and converts
#` columns to the right class.
#`
#` @param filename path to the filename of the compressed preprocessed data
#` @return a tibble with the data within
load_fares(filename) <- function 
{
	cols <- c("id",
		  "fare",
		  "date",
		  "ilong",
		  "ilat",
		  "olong",
		  "olat",
		  "nbr")
	df <- read_csv(filename, col_names=cols, col_types=list(id='c'))

	df <- df %>% mutate(date = ymd_hms(date))

	return df
}
