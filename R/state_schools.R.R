# https://www.insidehighered.com/news/2021/12/02/race-and-full-time-faculty-student-ratios-suny-cuny
library(tidyverse)
library(ipeds)

data(surveys)
View(surveys)

year <- 2019
data_dir <- '~/Downloads/ipeds/'
available_ipeds()

id <- 196060 # University at Albany

sectors <- c(`1` = 'Public, 4-year or above',
			 `2` = 'Private not-for-profit, 4-year or above',
			 `3` = 'Private for-profit, 4-year or above',
			 `4` = 'Public, 2-year',
			 `5` = 'Private not-for-profit, 2-year',
			 `6` = 'Private for-profit, 2-year',
			 `7` = 'Public, less-than 2-year', 
			 `8` = 'Private not-for-profit, less-than 2-year',
			 `9` = 'Private for-profit, less-than 2-year')

# getIPEDSDownloadDirectory() # Where are the data files downloaded

options(timeout = max(300, getOption("timeout")))
download_ipeds(year = year, force = FALSE, dir = data_dir)

ipeds_data <- load_ipeds(year = year, dir = data_dir)

# hd_help <- ipeds_help('HD', year = year, dir = data_dir) %>% filter(varName == 'SECTOR')
# hd_help$longDescription
hd <- ipeds_survey('HD', year = year, dir = data_dir) # Institutional Characteristics
effy <- ipeds_survey('EFFY', year = year, dir = data_dir) # 12-month duplicated headcount: 2017-18
efa <- ipeds_survey('EFA', year = year, dir = data_dir) # Race/ethnicity, gender, attendance status, and level of student: Fall 2018

# EFYTOTLT: Grand total
# EFYBKAAT: Black or African American total
# EFYHISPT: Hispanic or Latino total
effy %>% filter(UNITID == id & EFFYLEV == 2) %>% # Undergraduate total (2) Graduate is 4 and all students is 1
	select(UNITID, EFYTOTLT, EFYBKAAT, EFYHISPT) %>%
	rename(UNITID = UNITID,
		   TotalEnrollment = EFYTOTLT,
		   Black = EFYBKAAT,
		   Hispanic = EFYHISPT) %>%
	mutate(PercentBlackHispanic = (Black + Hispanic) / TotalEnrollment)

instructors <- ipeds_data[['S2018_IS']] 
instructors %>% filter(UNITID == id, SISCAT == 1) %>%
	select(UNITID, HRTOTLT) # All full-time instructional staff



ipeds_help('SAL_FACULTY', year = year, dir = data_dir) %>% View()

ny_public_schools <- hd %>%
	filter(STABBR == 'NY',
		   SECTOR %in% c(1, 4)) %>%
	mutate(SECTOR = factor(SECTOR, 
						   levels = as.integer(names(sectors)),
						   labels = sectors))

table(ny_public_schools$SECTOR, useNA = 'ifany')
nrow(ny_public_schools)




