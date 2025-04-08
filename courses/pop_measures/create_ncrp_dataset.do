/* ------------------------------------------------- *
* Create sample dataset for Population Measures course
* Using NCRP data for 1991-2021
* Citation info: 
	United States. Bureau of Justice Statistics. National Corrections Reporting Program, 1991-2021: Selected Variables. Inter-university Consortium for Political and Social Research [distributor], 2025-03-06. https://doi.org/10.3886/ICPSR39234.v1
	Accessed 3/12/2025

* Rachael Druckhammer, 3/18/2025
* ------------------------------------------------- */

cls
clear all
macro drop all

* set seed for random values
set seed 646787321

* ------------------------------------------------- *
* import raw NCRP prison term data (admissions, releases, population)
* NOTE: due to large file size, the raw data is not saved in this repo
import delimited "NCRP 1991-2021\ICPSR_39234\DS0001\39234-0001-Data.tsv", clear

* drop variable labels - just all-caps version of variable name
foreach v of varlist * {
	label variable `v'
}

* drop variables we don't need
drop mand_prisrel_year proj_prisrel_year parelig_year timesrvd education sentlgth offdetail
rename abt_inmate_id person_id

* drop years we don't need
** don't need releases prior to 2017 or records with no admission year
tab1 admityr releaseyr, missing
drop if releaseyr < 2017 | admityr == 9999 

** set missing release year to actual missing value
replace releaseyr = . if releaseyr == 9999
tab releaseyr, missing
summarize admityr releaseyr, detail

** drop "missing" release types & age at release if releaseyr is blank 
** these people have not been released, so info is not actually missing
replace reltype = . if releaseyr == .
replace agerelease = . if releaseyr == .
tab1 reltype agerelease, missing

** cut down size of file - don't need 3M records!
** take 300 records per state & year
sample 300, count by(state releaseyr)

* save SAMPLE of raw data to this repo
export delimited "ncrp_raw_1991-2021_ds0001_sample.csv", replace

* ------------------------------------------------- *
* using sample data csv 
import delimited "ncrp_raw_1991-2021_ds0001_sample.csv", clear

* -----------------------
* randomly generate fake admission MONTH 
** get difference between admit and release year
generate los_apprx_yrs = releaseyr - admityr
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
generate admit_date = mdy(adm_mon_rand_updt, adm_day_rand_updt, admityr)
format admit_date %td

summarize admit_date, format detail

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
replace los_apprx_days_max = datediff(admit_date, mdy(12, 31, releaseyr), "day") if los_apprx_yrs == 0
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
tab los_days_rand if admit_date == mdy(12, 31, releaseyr)
tab los_days_rand if adm_mon_rand_updt == 12 & los_apprx_yrs == 0

* -----------------------
* create release date based on random LOS
generate release_date = admit_date + los_days_rand
format release_date %td

** check for any records where release date is not in releaseyr
generate reldate_flag = 0
replace reldate_flag = 1 if year(release_date) != releaseyr
tab reldate_flag
tab los_apprx_yrs if reldate_flag == 1

** update fake release date to use releaseyr 
** fix leap year dates
generate release_date_updt = mdy(month(release_date), day(release_date), releaseyr) if reldate_flag == 1
replace release_date_updt = mdy(2, 28, releaseyr) if release_date_updt == . & month(release_date) == 2 & day(release_date) == 29
format release_date_updt %td

** check using updated release date
generate los_updt_chk = datediff(admit_date, release_date_updt, "day")
generate rel_updt_chk = admit_date + los_updt_chk
format rel_updt_chk %td

generate rel_updt_flag = 0
replace rel_updt_flag = 1 if reldate_flag == 1 & release_date_updt != rel_updt_chk
tab rel_updt_flag, missing
* all dates match! 

** overwrite release_date with updated values
replace release_date = release_date_updt if reldate_flag == 1 & release_date_updt != .

** re-check for records where release date is not in releaseyr
generate reldate_flag_chk = 0
replace reldate_flag_chk = 1 if year(release_date) != releaseyr
tab reldate_flag_chk

** make sure all records with releaseyr have a release_date
tab releaseyr if release_date == . & releaseyr != ., missing

* -----------------------
* decode values
** define value labels from codebook (note: shortened some labels for brevity)
label define sex 1 "Male" 2 "Female"
label define admtype 1 "New court commitment" 2 "Parole return/revocation" 3 "Other admission" 9 "Missing"
label define offgeneral 1 "Violent" 2 "Property" 3 "Drugs" 4 "Public order" 5 "Other/unspecified" 9 "Missing" 
label define race 1 "White, non-Hispanic" 2 "Black, non-Hispanic" 3 "Hispanic, any race" 4 "Other race(s), non-Hispanic" 9 "Missing"
label define ageadmit 1 "18-24 years" 2 "25-34 years" 3 "35-44 years" 4 "45-54 years" 5 "55+ years" 9 "Missing"
label define agerelease 1 "18-24 years" 2 "25-34 years" 3 "35-44 years" 4 "45-54 years" 5 "55+ years" 9 "Missing" 
label define reltype 1 "Conditional release" 2 "Unconditional release" 3 "Other release" 9 "Missing"
label define state 1 "Alabama" 2 "Alaska" 4 "Arizona" 5 "Arkansas" 6 "California" 8 "Colorado" 9 "Connecticut" 10 "Delaware" 11 "District of Columbia" 12 "Florida" 13 "Georgia" 15 "Hawaii" 16 "Idaho" 17 "Illinois" 18 "Indiana" 19 "Iowa" 20 "Kansas" 21 "Kentucky" 22 "Louisiana" 23 "Maine" 24 "Maryland" 25 "Massachusetts" 26 "Michigan" 27 "Minnesota" 28 "Mississippi" 29 "Missouri" 30 "Montana" 31 "Nebraska" 32 "Nevada" 33 "New Hampshire" 34 "New Jersey" 35 "New Mexico" 36 "New York" 37 "North Carolina" 38 "North Dakota" 39 "Ohio" 40 "Oklahoma" 41 "Oregon" 42 "Pennsylvania" 44 "Rhode Island" 45 "South Carolina" 46 "South Dakota" 47 "Tennessee" 48 "Texas" 49 "Utah" 50 "Vermont" 51 "Virginia" 53 "Washington" 54 "West Virginia" 55 "Wisconsin"

** add value labels to each variable
foreach v of varlist sex admtype offgeneral race ageadmit agerelease reltype state {
	label values `v' `v'
}

** decode variables
foreach v in sex race state {
	rename `v' `v'_enc
	decode `v'_enc, generate(`v')
}
decode admtype, generate(admit_type)
decode reltype, generate(release_type)
decode offgeneral, generate(offense_category)
decode ageadmit, generate(age_at_admit)
decode agerelease, generate(age_at_release)

** drop encoded versions
drop *_enc admtype reltype offgeneral ageadmit agerelease

* -----------------------
* final file cleanup 
describe, fullnames

** drop vars for creating fake dates
drop adm_*_rand mon_maxdays *_flag *_chk *_updt los_* admityr releaseyr 

** reorder vars
order person_id admit_date admit_type release_date release_type offense_category sex race age_at_admit age_at_release state
sort person_id admit_date

** save file as csv
export delimited "pop_measures_sample_data.csv", replace

