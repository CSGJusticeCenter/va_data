/* ------------------------------------------------- *
* Create sample datasets for Population Measures course
	- "Facility" records
	- "Supervision" records
* Using NCRP data for 1991-2021
* Citation info: 
	United States. Bureau of Justice Statistics. National 
	Corrections Reporting Program, 1991-2021: Selected Variables. 
	Inter-university Consortium for Political and Social Research 
	[distributor], 2025-03-06. https://doi.org/10.3886/ICPSR39234.v1
	Accessed 3/12/2025

* Rachael Druckhammer, 3/18/2025
* ------------------------------------------------- */

* seed for random values
set seed 646787321

/* ------------------------------------------------- *
* import raw NCRP prison term data (admissions, releases, population)
* ------------------------------------------------- *
* NOTE: Due to file size limits, the raw NCRP data is not saved in this repo.
*	This section of code is included to show how the 250k sample file was made.

** import raw file
import delimited "NCRP 1991-2021\ICPSR_39234\DS0001\39234-0001-Data.tsv", clear

** drop records for releases prior to 2017 and records with no admission year
drop if releaseyr < 2017 | admityr == 9999

** randomly select 250,000 records to make smaller file
sample 250000, count

** save random sample file in repo 
export delimited "ncrp_raw_1991-2021_ds0001_250k_sample.csv", replace
* ------------------------------------------------- */

* ------------------------------------------------- *
* prep sample data csv 
* ------------------------------------------------- *
import delimited "ncrp_raw_1991-2021_ds0001_250k_sample.csv", clear varnames(1)

* set "missing" values to missing
replace admityr = . if admityr == 9999
replace releaseyr = . if releaseyr == 9999
replace reltype = . if releaseyr == .

* shorten ID number to create "fake" id - take last 7 chars & add "1" to front
generate person_id = "1" + substr(abt_inmate_id, -7, .)

* drop original ID var & unnecessary variables 
drop abt_inmate_id mand_prisrel_year proj_prisrel_year parelig_year timesrvd education sentlgth offdetail state ageadmit agerelease
order person_id sex race admityr admtype offgeneral releaseyr reltype

* drop duplicate records - doesn't matter which records 
** duplicates in all remaining variables
duplicates drop

** duplicates where everything is the same except race
duplicates drop person_id sex admityr admtype offgeneral releaseyr reltype, force

** duplicates where everything is the same except sex
duplicates drop person_id race admityr admtype offgeneral releaseyr reltype, force

** duplicates where everything is the same except offense
duplicates drop person_id sex race admityr admtype releaseyr reltype, force

** duplicates where everything is the same except release year & type
duplicates drop person_id sex race admityr admtype offgeneral, force

* get count of records per fake id
sort person_id admityr admtype offgeneral releaseyr reltype
by person_id: generate person_n = _n
by person_id: generate person_N = _N
tab person_N if person_n == 1

* -----------------------
* standardize SEX for each id number
** randomly assign value for sorting rows
generate sex_sort = runiform()
** re-count records in random order
sort person_id sex_sort
by person_id: generate pers_sex_n = _n
** clear out sex for all records except n = 1
replace sex = . if pers_sex_n > 1
tab sex		// rate seems about right for M/F
** get min value for each ID
by person_id: egen update_sex = min(sex)
tab update_sex sex, missing
** replace sex with updated value
replace sex = update_sex
drop update_sex sex_sort pers_sex_n

* standardize RACE for each id number
** randomly assign value for sorting rows
generate race_sort = runiform()
** re-count records in random order
sort person_id race_sort
by person_id: generate pers_race_n = _n
** clear out race for all records except n = 1
replace race = . if pers_race_n > 1
tab race
** get min value for each ID
by person_id: egen update_race = min(race)
tab update_race race, missing
** replace race with updated value
replace race = update_race
drop update_race race_sort pers_race_n

* -----------------------
* before creating fake dates, INCREASE years in data by 2, for more recent dates
foreach v in admit release {
	generate `v'_year = `v'yr + 2 if `v'yr != .
	summarize `v'yr `v'_year	
}
drop admityr releaseyr

* -----------------------
* randomly generate fake admission MONTH 
** get difference between admit and release year
generate los_apprx_yrs = release_year - admit_year
tab los_apprx_yrs, missing

** generate random month value (integer between 1 and 12)
generate adm_mon_rand = runiformint(1, 12)
tab adm_mon_rand, missing

** for admissions & releases in same year, how many were randomly set to October, November, or December?
tab adm_mon_rand if los_apprx_yrs == 0, missing

generate updt_mon_flag = 0
replace updt_mon_flag = 1 if los_apprx_yrs == 0 & adm_mon_rand > 9
tab adm_mon_rand updt_mon_flag if los_apprx_yrs == 0

** re-assign these months (don't get rid of ALL records, but want significantly less)
generate adm_mon_rand_updt = runiformint(1, 12) if updt_mon_flag == 1
replace adm_mon_rand_updt = adm_mon_rand if updt_mon_flag == 0
tab adm_mon_rand adm_mon_rand_updt if los_apprx_yrs == 0, missing
tab adm_mon_rand_updt if los_apprx_yrs == 0

* -----------------------
* randomly generate fake admission DAY
** set max days in each month (not worrying about leap years)
generate mon_maxdays = 31
replace mon_maxdays = 28 if adm_mon_rand_updt == 2
replace mon_maxdays = 30 if inlist(adm_mon_rand_updt, 4, 6, 9, 11)
tab adm_mon_rand_updt mon_maxdays

** generate random day for each month
generate adm_day_rand = runiformint(1, mon_maxdays)

** check min/max for each month
preserve
collapse (min) min_adm_day_rand=adm_day_rand (max) max_adm_day_rand=adm_day_rand, by(adm_mon_rand)
list, noobs abbreviate(20)
restore

** check dates for December admissions with los_apprx_yrs = 0
tab adm_day_rand if adm_mon_rand_updt == 12 & los_apprx_yrs == 0

** flag dates for Dec 30 & 31 - we want a very small # of records with 1-2 day LOS
generate updt_day_flag = 0
replace updt_day_flag = 1 if los_apprx_yrs == 0 & adm_mon_rand_updt == 12 & adm_day_rand >= 30
tab adm_mon_rand_updt updt_day_flag if los_apprx_yrs == 0, missing

** redistribute random days for records with LOS under 1 year and 12/31 random admission day
generate adm_day_rand_updt = runiformint(1, 31) if updt_day_flag == 1
replace adm_day_rand_updt = adm_day_rand if updt_day_flag == 0
tab adm_day_rand_updt if los_apprx_yrs == 0 & adm_mon_rand_updt == 12, missing
tab adm_day_rand_updt updt_day_flag if los_apprx_yrs == 0, missing

* -----------------------
* combine random month & day into admission date
generate admit_date_fmt = mdy(adm_mon_rand_updt, adm_day_rand_updt, admit_year)
format admit_date_fmt %td
summarize admit_date_fmt, format detail

* -----------------------
* randomly generate LOS for creating release date
** set range for number of days, based on appx LOS in years
**	(e.g., if los_apprx_yrs = 2, min = 730 and max = 1094)
tab los_apprx_yrs

* min = # of days in los_apprx_yrs
* 0 days here is OK - just means that calculated LOS will be 1 day
generate los_apprx_days_min = los_apprx_yrs * 365

* max = 1 less than # of days in los_apprx_yrs+1
* if los_apprx_yrs is 0, max = number of days to end of year
generate los_apprx_days_max = ((los_apprx_yrs + 1) * 365) - 1
replace los_apprx_days_max = datediff(admit_date_fmt, mdy(12, 31, release_year), "day") if los_apprx_yrs == 0
replace los_apprx_days_max = 364 if los_apprx_days_max == 365

* check min/max days for each value
preserve
collapse (min) los_apprx_days_min (max) los_apprx_days_max, by(los_apprx_yrs)
list, noobs abbreviate(20)
restore

** randomly select a number of days between min/max apprx days 
generate los_days_rand = runiformint(los_apprx_days_min, los_apprx_days_max)
summarize los_days_rand, detail

tab los_days_rand if los_days_rand < 30
tab los_days_rand if admit_date == mdy(12, 31, release_year)
tab los_days_rand if adm_mon_rand_updt == 12 & los_apprx_yrs == 0

* -----------------------
* create release date based on random LOS
generate release_date_fmt = admit_date_fmt + los_days_rand
format release_date_fmt %td
summarize release_date_fmt, format

** check for any records where release date is not in release_year
generate reldate_flag = 0
replace reldate_flag = 1 if year(release_date_fmt) != release_year
tab reldate_flag
tab los_apprx_yrs if reldate_flag == 1

** update fake release date to use release_year 
** fix leap year dates
generate release_date_updt = mdy(month(release_date_fmt), day(release_date_fmt), release_year) if reldate_flag == 1
replace release_date_updt = mdy(2, 28, release_year) if release_date_updt == . & month(release_date_fmt) == 2 & day(release_date_fmt) == 29
format release_date_updt %td

** check using updated release date
generate los_updt_chk = datediff(admit_date_fmt, release_date_updt, "day")
generate rel_updt_chk = admit_date_fmt + los_updt_chk
format rel_updt_chk %td

generate rel_updt_flag = 0
replace rel_updt_flag = 1 if reldate_flag == 1 & release_date_updt != rel_updt_chk
tab rel_updt_flag, missing
* all dates match! 

** overwrite release_date with updated values
replace release_date_fmt = release_date_updt if reldate_flag == 1 & release_date_updt != .
summarize release_date_fmt, format

** re-check for records where release date is not in release_year
generate reldate_flag_chk = 0
replace reldate_flag_chk = 1 if year(release_date_fmt) != release_year
tab reldate_flag_chk

** make sure all records with release_year have a release_date
tab release_year if release_date_fmt == . & release_year != ., missing

** drop vars for creating fake dates
drop adm_*_rand mon_maxdays *_flag *_chk *_updt los_* 

* -----------------------
* convert dates to DATE and DATETIME string vars 
** create string version of date vars in "YYYY-MM-DD" format
foreach v in admit release {	
	tostring `v'_date_fmt, generate(`v'_date) format(%tdCCYY-NN-DD) force
	replace `v'_date = "" if `v'_date_fmt == .
}

** randomly generate time for nonmissing dates
foreach v in admit release {
	* get random integer for hours and minutes
	*	use 24-hour clock for hours
	generate `v'_hr = runiformint(0, 23)
	generate `v'_min = runiformint(0, 59)
	
	* combine hours & minutes with ":00" seconds to create random time
	*	add leading zeros to hours/mins
	generate `v'_time = strofreal(`v'_hr, "%02.0f") + ":" + ///
		strofreal(`v'_min, "%02.0f") + ":00" if `v'_date != ""
}

** combine date & time into datetime
**	this can be a string since formatting will be lost in CSV export
foreach v in admit release {
	generate `v'_datetime = `v'_date + " " + `v'_time
}

** drop variables used to create datetime vars
drop *_hr *_min *_time

* -----------------------
* decode numeric values
** define value labels from codebook (note: shortened some labels for brevity)
label define sex 1 "Male" 2 "Female"
label define admtype 1 "New court commitment" 2 "Parole return/revocation" 3 "Other admission" 9 "Missing"
label define offgeneral 1 "Violent" 2 "Property" 3 "Drugs" 4 "Public order" 5 "Other/unspecified" 9 "Missing" 
label define race 1 "White, non-Hispanic" 2 "Black, non-Hispanic" 3 "Hispanic, any race" 4 "Other race(s), non-Hispanic" 9 "Missing"
label define reltype 1 "Conditional release" 2 "Unconditional release" 3 "Other release" 9 "Missing"

** add value labels to each variable
foreach v of varlist sex admtype offgeneral race reltype {
	label values `v' `v'
}

** decode variables
foreach v in sex race {
	rename `v' `v'_enc
	decode `v'_enc, generate(`v')
}
decode admtype, generate(admit_type)
decode reltype, generate(release_type)
decode offgeneral, generate(offense_category)

** drop encoded versions
drop *_enc admtype reltype offgeneral 

* -----------------------
* prep file for export
** drop extra vars
drop person_n person_N *_date_fmt 

** drop all variable labels
foreach v of varlist * {
	label variable `v'
}

** reorder vars
order person_id admit_date admit_datetime admit_type release_date release_datetime release_type offense_category sex race admit_year release_year
sort person_id admit_datetime

* ------------------------------------------------- *
* export datasets
* ------------------------------------------------- *
** export FULL file as csv
export delimited "pop_measures_sample_data.csv", replace

* -----------------------
* "FACILITY" dataset
** start with full file
import delimited "pop_measures_sample_data.csv", clear varnames(1) stringcols(_all)

** randomly select 10,000 records
sample 10000, count

** make sure values are distributed the way we want
tab1 admit_year release_year, missing
tab1 admit_type release_type offense_category, missing
tab1 sex race, missing

** build in scenarios for examples
sort person_id admit_datetime
* force some records to have 2023 admission & release dates
replace admit_date = "2023-02-13" in 2
replace admit_type = "New court commitment" in 2
replace release_date = "2023-10-14" in 2
replace release_type = "Conditional release" in 2
replace admit_date = "2023-05-01" in 4
replace admit_type = "Parole return/revocation" in 4
replace release_date = "2023-10-23" in 4
replace release_type = "Unconditional release" in 4
* force some people to have n>1
replace person_id = "10000119" in 5/6
replace sex = "Male" in 5/6 
replace race = "Black, non-Hispanic" in 5/6
replace admit_date = "2023-01-16" in 5
replace admit_type = "Parole return/revocation" in 5
replace offense_category = "Violent" in 5
replace release_date = "2023-03-24" in 5
replace release_type = "Unconditional release" in 5
replace admit_date = "2023-06-21" in 6
replace admit_type = "New court commitment" in 6
replace offense_category = "Public order" in 6
replace release_date = "" in 6
replace release_datetime = "" in 6
replace release_type = "" in 6
replace person_id = "10000159" in 8/9
replace sex = "Male" in 8/9
replace race = "White, non-Hispanic" in 8/9
replace admit_date = "2019-10-11" in 8 
replace admit_type = "New court commitment" in 8
replace release_date = "2023-06-19" in 8 
replace release_type = "Conditional release" in 8
replace admit_date = "2023-09-04" in 9
replace admit_type = "Parole return/revocation" in 9
replace release_date = "2023-12-06" in 9
replace release_type = "Conditional release" in 9
* flag updated records
generate updated = 0
replace updated = 1 in 2
replace updated = 1 in 4/6
replace updated = 1 in 8/9
tab updated

** update admit datetime after changing data
* pull out time from original datetime & append to updated date
generate admit_time = substr(admit_datetime, -8, .) 
replace admit_datetime = admit_date + " " + admit_time 
generate release_time = substr(release_datetime, -8, .) 
* flag records missing release time
generate no_reltime = 0
replace no_reltime = 1 if release_time == "" & release_date != ""
tab no_reltime updated, missing
* randomly generate time for records missing release time
generate new_rel_hr = runiformint(0, 23) if no_reltime == 1 & updated == 1
generate new_rel_min = runiformint(0, 59) if no_reltime == 1 & updated == 1
generate new_rel_time = strofreal(new_rel_hr, "%02.0f") + ":" + ///
	strofreal(new_rel_min, "%02.0f") + ":00" if no_reltime == 1 & updated == 1
* update release_datetime
replace release_datetime = release_date + " " + new_rel_time if no_reltime == 1 & updated == 1
* make sure date values match
generate adm_flag = 0
replace adm_flag = 1 if substr(admit_date, 1, 10) != substr(admit_datetime, 1, 10)
generate rel_flag = 0
replace rel_flag = 1 if substr(release_date, 1, 10) != substr(release_datetime, 1, 10)
tab1 adm_flag rel_flag
* drop flags & new values
drop *_time *_flag updated new_rel_* no_reltime

** update admit/release year after changing data
replace admit_year = substr(admit_date, 1, 4)
replace release_year = substr(release_date, 1, 4)

** check for any duplicates
duplicates drop

** DO NOT RE-SORT, or the records we just updated will be moved around!
** export file as csv
export delimited "pop_measures_sample_facility_data.csv", replace

* -----------------------
* "SUPERVISION" data
** start with full file
import delimited "pop_measures_sample_data.csv", clear varnames(1) stringcols(_all)

** randomly select 10,000 records
sample 10000, count

** make sure values are distributed the way we want
tab1 admit_year release_year, missing
tab1 offense_category, missing
tab1 sex race, missing

** rename admit/release dates
rename admit_date start_date
rename release_date end_date
rename admit_year start_year
rename release_year end_year 

** drop datetime & admit/release vars
drop *_datetime admit_type release_type

** change ID numbers
destring person_id, generate(id_num)
generate id_update = id_num + 123456
format id_update %9.0f
tostring id_update, replace
replace person_id = id_update
drop id_num id_update

** randomly assign probation or parole
generate rand_type = runiformint(1,2)
tab rand_type
* slightly decrease number of parole records
generate rand_parole = runiformint(1,3) if rand_type == 1
tab rand_type rand_parole, missing
replace rand_type = 2 if rand_parole == 1
tab rand_type
* categorize values
generate supervision_type = "Probation" if rand_type == 2
replace supervision_type = "Parole" if rand_type == 1
tab supervision_type, missing

** randomly assign outcome (completed, revoked, other)
generate rand_outcome = runiformint(1,3) if end_date != ""
tab rand_outcome
* we don't want equal frequencies, so re-assign all "other" values
generate rand_other = runiformint(1,3) if rand_outcome == 3
tab rand_outcome rand_other, missing
replace rand_outcome = rand_other if rand_other != .
tab rand_outcome
* re-assign revoked values to decrease frequency (exclude "other")
generate rand_revs = runiformint(1,2) if rand_outcome == 2
tab rand_outcome rand_revs, missing
replace rand_outcome = rand_revs if rand_revs != .
tab rand_outcome
* categorize values
generate end_reason = "Completed" if rand_outcome == 1
replace end_reason = "Revoked" if rand_outcome == 2
replace end_reason = "Other" if rand_outcome == 3
tab end_reason, missing

** drop random values
drop rand_*

** build in scenarios for examples
sort person_id start_date
* force a record to have n>1 
replace person_id = "10915996" in 3/4
replace supervision_type = "Probation" in 3/4
replace start_date = "2022-06-02" in 3
replace start_date = "2017-06-28" in 4
replace end_date = "" in 3/4
replace end_reason = "" in 3/4
replace offense_category = "Violent" in 3
replace offense_category = "Public order" in 4
replace race = "Black, non-Hispanic" in 3/4
replace sex = "Male" in 3/4

** update start/end year after changing data
replace start_year = substr(start_date, 1, 4)
replace end_year = substr(end_date, 1, 4)

** check for any duplicates
duplicates drop

** DO NOT RE-SORT, or the records we just updated will be moved around!
** export file as csv
export delimited "pop_measures_sample_supervision_data.csv", replace
