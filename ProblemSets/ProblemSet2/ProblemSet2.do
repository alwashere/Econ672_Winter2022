/*===========================================================================
|	 Problem Set 2
===========================================================================*/

/* Define paths for data and output */
global data "Data"
global output "Output"
global estOpt "ar2 se obslast scalar(F)"

log using "$output/Econ672Ps2.log", replace

/*****************************************************************************
*	 Assignment:  Econ 672 Problem Set 2

*	Author: Jon Holder
*****************************************************************************/

/*****************************************************************************
*	 Q1 - Read in Data File
*****************************************************************************/

use "$data/econ672_njs_data.dta", clear
describe
estpost summarize
esttab . using $output/tableQ1.tex, ///
    replace ///
    keep(sex age race esum18i bfyrearn bfeduca totch18) ///
    noobs nonumbers ///
    cells( (mean(label("Mean")) sd(label("SD")) ///
           min(label("Min")) max(label("Max")) count(label("Num Obs"))) ) ///
    coeflabels(sex "Sex" ///
              age "Age" ///
              race "Race" ///
              esum18i "Earnings 18 months later" ///
              bfyrearn "Earnings at initial survey" ///
              bfeduca "Years of educations at initial survey" ///
              totch18 "Childer under 18yr at initial survey")

/*****************************************************************************
*	 Q2 - Create *treatment* variable
*****************************************************************************/

/* Create treatment =1 if in treatment group */
generate treatment =(ra_stat==1)
label variable treatment "=1 if in treatment group"

/* Drop observations that have missing earnings */
drop if esum18i==.

/* Drop treatment observations that were in treatment and not in JTPA */
drop if (enroll==0 & treatment==1)

tab treatment enroll

/*****************************************************************************
*	 Q3 - Create indicators key pre-experiment info is missing.
*****************************************************************************/

generate miss_child=(totch18==.)
label variable miss_child "=1 if no children <18 or data missing"

generate miss_educ=(bfeduca==.)
label variable miss_educ "=1 if education is 0 or data missing"

generate miss_earn=(bfyrearn==.)
label variable miss_earn "=1 if bfearn is 0 or data missing"

/* Replace missing values in main parameters with 0 */
replace totch18=0 if miss_child==1
replace bfeduca=0 if miss_educ==1
replace bfyrearn=0 if miss_earn==1

misstable summarize totch18 bfeduca bfyrearn

misstable summarize esum18i

/*****************************************************************************
*	 Q4 - Estimate OLS experimental assignment to earnings
*****************************************************************************/

regress esum18i treatment, robust
eststo baseOls

/* Print to log file */
esttab baseOls, ar2 se obslast scalars(F) ///
    coeflabels(treatment "In treatment group") ///
    mtitles("Earnings 18 month later")

/* Printable table for report */
esttab baseOls using $output/tableQ4.tex, ///
    replace ///
    $estOpt ///
    coeflabels(treatment "In treatment group") ///
    mtitles("Earnings 18 month later")

/*****************************************************************************
*	 Q5 - Estimate with OLS and covariates
*****************************************************************************/

global covariates "sex i.race age totch18 bfeduca bfyrearn i.site_num miss_child miss_educ miss_earn"

regress esum18i treatment $covariates, cluster(site_num)
eststo olsCovariates

/* Print to log file */
esttab baseOls olsCovariates, ///
    keep(treatment) ///
    $estOpt ///
    coeflabels(treatment "In treatment group") ///
        addnotes("Estimate with covariates are clusted by site location") ///
    mtitles("Earnings 18 month later" "With covariates")

/* Printable table for report */
esttab baseOls olsCovariates using $output/tableQ5.tex, ///
    replace ///
    keep(treatment) ///
    $estOpt ///
    coeflabels(treatment "In treatment group") ///
    addnotes("Estimate with covariates are clustered by site location") ///
    mtitles("Earnings 18 month later" "With covariates")


/*****************************************************************************/
/*****************************************************************************
*	 Difference-in-differences
*****************************************************************************/

/*****************************************************************************
*	 Q7 - Difference-in-Difference
*****************************************************************************/

/*
	Estimate difference between JTPA participation on earnings.  
	Control group is people enrolled in JTPA and in the control group.  
	Treatment group is people enrolled in JTPA and assigned to the treatment group. 
*/
generate diffEarn=esum18i-bfyrearn

tabstat diffEarn, statistics(mean) by(treatment)

regress diffEarn treatment,robust
eststo didBasic

/* DID Estimate include covariates */
regress diffEarn treatment ///
    sex i.race age totch18 bfeduca i.site_num ///
    miss_child miss_educ miss_earn, ///
    robust
eststo didCov

/* Table for display */
esttab didBasic didCov, ///
    keep(treatment) ///
    $estOpt ///
    mtitles("Basic" "With Covariates")
    
/* Table for output */
esttab didBasic didCov using $output/tableQ7.tex, ///
    replace ///
    keep(treatment) ///
    $estOpt ///
    mtitles("Basic Earnings" "With Covariates")


/*****************************************************************************/
/*****************************************************************************
*	 Regression Discontinuity
*****************************************************************************/

/*****************************************************************************
*	 Q9 - Drop Data from the control group with miss_earn==1
*****************************************************************************/

/* Drop treatment group */
drop if treatment==1

/* Drop if miss_earn ==1 */
drop if miss_earn==1

/*****************************************************************************
*	 Q10 - Create rdtreat=1 for Control Group with bfyrearn<=2650
*****************************************************************************/

generate rdtreat=(bfyrearn<=2650)

/*****************************************************************************
*	 Q11 - Construct a regression discontinuity with a window of \$500 on each side
*****************************************************************************/

global window "bfyrearn>=2150 & bfyrearn<=3150"

/* 
    Calculate differences in mean for data on each side
    of the window
*/
regress esum18i rdtreat if $window, robust
eststo rdBasic

/* Table for display */
esttab rdBasic, ///
    $estOpt ///
    keep(rdtreat) ///
    coeflabels(rdtreat "At or below cutoff") ///
    mtitles("Window") ///
    addnotes("Cutoff at $2,650" ///
             "Window is -/+ $500")
    
/* Table for output */
esttab rdBasic using $output/tableQ11.tex, ///
    replace ///
    $estOpt ///
    keep(rdtreat) ///
    coeflabels(rdtreat "At or below cutoff") ///
    mtitles("Window") ///
    addnotes("Cutoff at \\$2,650" ///
             "Window is -/+ \\$500")

rdplot esum18i bfyrearn if $window, c(2650) p(1)

/*****************************************************************************
*	 Q14 - Estimate Regression Discontinuity with OLS
*****************************************************************************/

/* RD with all data */
regress esum18i rdtreat bfyrearn i.rdtreat#c.bfyrearn ///
    , robust
eststo rdAllData

/* RD with -/+500 window */
regress esum18i rdtreat bfyrearn i.rdtreat#c.bfyrearn ///
    if $window, robust
eststo rdWindow
predict varRd, xb

twoway(line varRd bfyrearn if rdtreat==1) ///
      (line varRd bfyrearn if rdtreat==0) ///
      (scatter esum18i bfyrearn) ///
       if $window

/* Table for display */
esttab rdAllData rdWindow, ///
    $estOpt ///
    keep(rdtreat bfyrearn 1.rdtreat#c.bfyrearn) ///
    coeflabels(rdtreat "At or below cutoff"  ///
               bfyrearn "Earnings 1 year prior" ///
               1.rdtreat#c.bfyrearn "\beta Interaction") ///
    mtitles("All Data" "Window") ///
    addnotes("Cutoff at $2,650" ///
             "Window is -/+ $500")
    
/* Table for output */
esttab rdAllData rdWindow using $output/tableQ14.tex, ///
    replace ///
    $estOpt ///
    keep(rdtreat bfyrearn 1.rdtreat#c.bfyrearn) ///
    coeflabels(rdtreat "At or below cutoff"  ///
               bfyrearn "Earnings 1 year prior" ///
               1.rdtreat#c.bfyrearn "$\beta\$Interaction") ///
    mtitles("All Data" "Window") ///
    addnotes("Cutoff at \\$2,650" ///
             "Window is -/+ \\$500")



/***************************************************************************
>>>>>>>>>>>>>>    E N D    O F     F I L E  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
****************************************************************************/

log close
clear