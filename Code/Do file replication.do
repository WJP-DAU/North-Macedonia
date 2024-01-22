******************************************
*** DO FILE FOR NORTH MACEDONIA REPORT ***
******************************************

/*READ:
This do file will replicate the NM report created in R
*/

/*=====================================================================================================================================
					1. Settings and Data Loading
=====================================================================================================================================*/

clear all 
set more off, perm

/*
if "`c(username)'"=="nrodriguez" {
use "C:\Users\nrodriguez\OneDrive - World Justice Project\Programmatic\General Population Poll\GPP 2023\Merged Files\Historical Files\Merged.dta" , clear 
} 
else {
} 

keep if country=="North Macedonia"

save "C:\Users\nrodriguez\OneDrive - World Justice Project\Programmatic\Data Analytics\6. Country Reports\North-Macedonia\Data\Dataset for replication.dta", replace
*/

**** Upload data ***
if "`c(username)'"=="nrodriguez" {
cd "C:\Users\nrodriguez\OneDrive - World Justice Project\Programmatic\Data Analytics\6. Country Reports\North-Macedonia\" 
use "Data\Dataset for replication.dta"
} 
else {
} 


/*=====================================================================================================================================
					3. Exporting the data
=====================================================================================================================================*/


**                       **
******** SECTION I ********
**                       **

***** Chart 1: Perceptions About Authoritarian Behaviors

label var CAR_q67_G1 "Attack or attempt to discredit opposition parties"
label var CAR_q68_G1 "Prosecute and convict members of opposition parties"
label var CAR_q67_G2 "Attack or attempt to discredit the electoral system and other supervisory organs"
label var CAR_q61_G1 "Censor opinions from opposition groups"

label var CAR_q66_G1 "Seek to influence the promotion and removal of judges "
label var CAR_q65_G1 "Refuse to comply with court rulings that are not in their favor"
label var CAR_q64_G1 "Seek to limit the courts' competencies and freedom to interpret the law"

label var CAR_q60_G2 "Resort to misinformation to shape public opinion in their favor"
label var CAR_q64_G2 "Attack or attempt to discredit the media and civil society organizations that criticize them"
label var CAR_q65_G2 "Prosecute and convict journalists and leaders of civil society organizations"
label var CAR_q60_G1 "Censor information that comes from abroad"

tab CAR_q67_G1
tab2xl CAR_q67_G1 using "Outputs/Report replication.xlsx" , replace sheet("Perceptions of Authoritarian") row(2) col(2)

local t=10
foreach v in CAR_q68_G1 CAR_q67_G2 CAR_q61_G1 CAR_q66_G1 CAR_q65_G1 CAR_q64_G1 CAR_q60_G2 CAR_q64_G2 CAR_q65_G2 CAR_q60_G1 {
tab `v'
tab2xl `v' using "Outputs/Report replication.xlsx", sheet("Perceptions of Authoritarian") row(`t') col(2)
local t=`t'+8
}

***** Chart 2: Perceptions of Authoritarian Behavior, by Political Support 

recode CAR_q59_G1 CAR_q59_G2 (98 99=.)
recode CAR_q67_G1 CAR_q68_G1 CAR_q67_G2 CAR_q61_G1 CAR_q66_G1 CAR_q65_G1 CAR_q64_G1 CAR_q60_G2 CAR_q64_G2 CAR_q65_G2 CAR_q60_G1  (1 2=1) (3 4=0) (98 99=.)

gen support=CAR_q59_G1
replace support=CAR_q59_G2 if CAR_q59_G1==. & CAR_q59_G2!=.
replace support=. if CAR_q59_G1!=CAR_q59_G2 & CAR_q59_G1!=. & CAR_q59_G2!=.

label define support 1 "Gov Supporter" 2 "Non Gov Supporter"
label values support support

preserve

keep if year==2023

collapse (mean) CAR_q67_G1 CAR_q68_G1 CAR_q67_G2 CAR_q61_G1 CAR_q66_G1 CAR_q65_G1 CAR_q64_G1 CAR_q60_G2 CAR_q64_G2 CAR_q65_G2 CAR_q60_G1, by(country_year support)

label var CAR_q68_G1 "Prosecute and convict members of opposition parties"
label var CAR_q61_G1 "Censor opinions from opposition groups"
label var CAR_q67_G2 "Attack or attempt to discredit the electoral system and other supervisory organs"
label var CAR_q67_G1 "Attack or attempt to discredit opposition parties"

label var CAR_q64_G1 "Seek to limit the courts' competencies and freedom to interpret the law"
label var CAR_q66_G1 "Seek to influence the promotion and removal of judges "
label var CAR_q65_G1 "Refuse to comply with court rulings that are not in their favor"

label var CAR_q65_G2 "Prosecute and convict journalists and leaders of civil society organizations"
label var CAR_q64_G2 "Attack or attempt to discredit the media and civil society organizations that criticize them"
label var CAR_q60_G2 "Resort to misinformation to shape public opinion in their favor"
label var CAR_q60_G1 "Censor information that comes from abroad"

drop if support==.

export excel "Outputs/Report replication.xlsx", sheet("Perceptions of Author Gov") firstrow(varl) cell(A2) 

putexcel set "Outputs/Report replication.xlsx", sheet("Perceptions of Author Gov") modify
putexcel C1:F1 = "ATTACKS ON ELECTORAL", overwri bold merge hcenter txtwrap fpattern(solid,"40 148 170") font(calibri, 11, white)
putexcel G1:I1 = "ATTACKS ON THE JUDICIARY", overwri bold merge hcenter txtwrap fpattern(solid,"252 144 150") font(calibri, 11, white)
putexcel J1:M1 = "ATTACKS ON THE MEDIA AND MISINFORMATION", overwri bold merge hcenter txtwrap fpattern(solid,"0 59 136") font(calibri, 11, white)
putexcel C3:M5, overwri nformat(percent) 
putexcel A2:M2, overwri bold hcenter txtwrap

restore

***** Chart 3: Attitudes towards Authoritarianism and Rule of Law

recode CAR_q73 CAR_q74 q50 q51 (1 2=1) (3 4=2) (5 99=3)
recode q52 (1 2=2) (3 4=1) (5 99=3)

foreach v in CAR_q73 CAR_q74 q50 q51 q52 {
qui tab `v', g(`v'_)
}

preserve

keep if year==2023

collapse (mean) CAR_q73_1 CAR_q73_2 CAR_q73_3 CAR_q74_1 CAR_q74_2 CAR_q74_3 q50_1 q50_2 q50_3 q51_1 q51_2 q51_3 q52_1 q52_2 q52_3, by(country_year)

label var CAR_q73_1 "The president can attack the media, civil society, and opposition groups"
label var CAR_q73_2 "The president must respect the media, civil society, and opposition groups"
label var CAR_q73_3 "None of the above"

label var CAR_q74_1 "The president can undermine independent authorities."
label var CAR_q74_2 "The president must respect independent authorities." 
label var CAR_q74_3 "None of the above"

label var q50_1 "Government efficiency is more important than citizen influence."
label var q50_2 "It is important that citizens have a say in government matters, even at the expense of efficiency."
label var q50_3 "None of the above"

label var q51_1 "The president should not be bound by the laws or courts."
label var q51_2 "The president must always obey the law and the courts."
label var q51_3 "None of the above"

label var q52_1 "It is not necessary to obey the laws of a government that you did not vote for."
label var q52_2 "It is important to obey the government in power, no matter who you voted for."
label var q52_3 "None of the above"

export excel "Outputs/Report replication.xlsx", sheet("Attitudes towards") firstrow(varl) cell(A1)

putexcel set "Outputs/Report replication.xlsx", sheet("Attitudes towards") modify
putexcel A1:Q1, overwri bold hcenter txtwrap
putexcel B2:Q2, overwri nformat(percent)

restore


***** Chart 4: Fundamental Freedoms

gl freedom "q46c_G2 q46f_G2 q46g_G2 q46c_G1 q46e_G2 q46d_G2 q46f_G1 q46a_G2 q46d_G1 q46e_G1 q46h_G2"
recode $freedom (1 2 =1) (3 4 =0) (99 98=.) 

preserve

drop if year<2014

collapse (mean) $freedom , by(country_year)

label var q46c_G2 "People can express opinions against the government"
label var q46f_G2 "Civil society organizations can express opinions against the government"
label var q46g_G2 "Political parties can express opinions against the government"
label var q46c_G1 "Media can express opinions against the government"
label var q46e_G2 "Media can expose cases of corruption"

label var q46d_G2 "People can attend community meetings"
label var q46f_G1 "People can join any political organization"
label var q46a_G2 "People can organize around an issue or petition"

label var q46d_G1 "Local government officials are elected through a clean process"
label var q46e_G1 "People can vote freely without feeling harassed or pressured"
label var q46h_G2 "Religious minorities can observe their holy days"

export excel "Outputs/Report replication.xlsx", sheet("Fundamental Freedoms") firstrow(varl) cell(A1)

putexcel set "Outputs/Report replication.xlsx", sheet("Fundamental Freedoms") modify
putexcel B2:M4, overwri nformat(percent) 
putexcel A1:M1, overwri bold hcenter txtwrap

restore

***** Chart 5: Fundamental Freedoms, by Political Support

preserve

keep if year==2023

collapse (mean) $freedom , by(country_year support)

drop if support==.

label var q46c_G2 "People can express opinions against the government"
label var q46f_G2 "Civil society organizations can express opinions against the government"
label var q46g_G2 "Political parties can express opinions against the government"
label var q46c_G1 "Media can express opinions against the government"
label var q46e_G2 "Media can expose cases of corruption"

label var q46d_G2 "People can attend community meetings"
label var q46f_G1 "People can join any political organization"
label var q46a_G2 "People can organize around an issue or petition"

label var q46d_G1 "Local government officials are elected through a clean process"
label var q46e_G1 "People can vote freely without feeling harassed or pressured"
label var q46h_G2 "Religious minorities can observe their holy days"

export excel "Outputs/Report replication.xlsx", sheet("Fundamental Freedoms political") firstrow(varl) cell(A1)

putexcel set "Outputs/Report replication.xlsx", sheet("Fundamental Freedoms political") modify
putexcel B2:M5, overwri nformat(percent) 
putexcel A1:M1, overwri bold hcenter txtwrap

restore


***** Chart 6: Government Accountability

recode q43_G2 (98 99=.)
qui tab q43_G2, g(q43_G2_)

*6.1

preserve

drop if year<2014

collapse (mean) q43_G2_3, by(country_year)

label var q43_G2_3 "Percentage of respondents who believe that high-ranking government officials would be held accountable for breaking the law" 

export excel "Outputs/Report replication.xlsx", sheet("Accountability 6.1") firstrow(varl) cell(A1)

putexcel set "Outputs/Report replication.xlsx", sheet("Accountability 6.1") modify
putexcel B2:C4, overwri nformat(percent) 
putexcel A1:C1, overwri bold hcenter txtwrap

restore

*6.2

preserve

keep if year==2023

collapse (mean) q43_G2_3, by(country_year support)

drop if support==.

label var q43_G2_3 "Percentage of respondents who believe that high-ranking government officials would be held accountable for breaking the law" 

export excel "Outputs/Report replication.xlsx", sheet("Accountability 6.2") firstrow(varl) cell(A1)

putexcel set "Outputs/Report replication.xlsx", sheet("Accountability 6.2") modify
putexcel B2:D5, overwri nformat(percent) 
putexcel A1:D1, overwri bold hcenter txtwrap

restore


**                       ***
******** SECTION II ********
**                       ***

***** Chart 7: Perceptions of Corruption by Institution Over Time

gl corruption "q2a q2d q2b q2c q2e q2g q2f " 

recode $corruption (3 4=1) (1 2=0) (98 99=.)

preserve

drop if year<2014

collapse (mean) $corruption, by(country_year)

label var q2a "Members of Congress"
label var q2d "Police Officers"

label var q2b "Local Government Officers"
label var q2c "National Government Officers"

label var q2e "Prosecutors"
label var q2g "Judges & Magistrates"
label var q2f "Public Defense Attorneys"

export excel "Outputs/Report replication.xlsx", sheet("Corruption over time") firstrow(varl) cell(A1)

putexcel set "Outputs/Report replication.xlsx", sheet("Corruption over time") modify 
putexcel A1:I1, overwri bold hcenter txtwrap
putexcel B2:I5, overwri nformat(percent)

restore

***** Chart 8: Attitudes Towards Corrupt Behaviors

label var CAR_q2c "A private citizen offering a bribe to a public official to speed up administrative procedures"

label var CAR_q2f "A law enforcement officer asking for a bribe"
label var CAR_q2g "A company official asking for a bribe from a job applicant"
label var CAR_q2b "A public officer asking for a bribe to speed up administrative procedures"

label var CAR_q2d "An elected official taking pubilc funds for private uses"
label var CAR_q2e "An elected official using stolen public funds to assist his or her community"
label var CAR_q2a "A public officer being recruited on the basis of family ties and friendship networks"

tab CAR_q2c
tab2xl CAR_q2c using "Outputs/Report replication.xlsx", sheet("Attitudes Towards Corrupt") row(2) col(2)

local t=10
foreach v in CAR_q2f CAR_q2g CAR_q2b CAR_q2d CAR_q2e CAR_q2a  {
tab `v'
tab2xl `v' using "Outputs/Report replication.xlsx", sheet("Attitudes Towards Corrupt") row(`t') col(2)
local t=`t'+8
}

***** Chart 9: Bribery

gl bribery "q4a q4b q4d q4e" 

recode $bribery (98 99 =.)

*Chart 9.1

preserve

drop if year<2017

collapse (mean) $bribery, by(country_year)

label var q4a "Request a Government Permit"
label var q4b "Request Public Benefits or Assistance"
*label var q4c "Obtain a Birth Certificate or Government-Issued ID"
label var q4d "Secure a Place at a Public School"
label var q4e "Use Public Health Services"

export excel "Outputs/Report replication.xlsx", sheet("Bribery Victimization 1") firstrow(varl)

putexcel set "Outputs/Report replication.xlsx", sheet("Bribery Victimization 1") modify
putexcel B2:F3, overwri nformat(percent)
putexcel A1:F1, overwri bold hcenter txtwrap

restore

*Chart 9.2

label var NM_q5_2d "Public Defense Attorneys"
label var CAR_q8c "Prosecutors"
label var CAR_q8e "Customs Officers"
label var CAR_q8b "Judges and Magistrates"
label var CAR_q8a "Police Officers"
label var CAR_q8k "Elected Representatives"

tab NM_q5_2d
tab2xl NM_q5_2d using "Outputs/Report replication.xlsx", sheet("Bribery Victimization 2") row(2) col(2)

local t=7
foreach v in CAR_q8c CAR_q8e CAR_q8b CAR_q8a CAR_q8k  {
tab `v'
tab2xl `v' using "Outputs/Report replication.xlsx", sheet("Bribery Victimization 2") row(`t') col(2)
local t=`t'+6
}

***** Chart 10: Trust

gen trust_police=q1d
gl trust "q1a trust_police q1b q1c q1e q1g q1f"

recode $trust (1 2=1) (3 4=0) (98 99=.)

preserve

keep if year>2012

collapse (mean) $trust, by(country_year)

label var q1a "People living in their community" 
label var trust_police "Police Officers"
label var q1b "Local Government Officers"
label var q1c "National Government Officers"
label var q1e "Prosecutors"
label var q1g "Judges & Magistrates"
label var q1f "Public Defense Attorneys"

export excel "Outputs/Report replication.xlsx", sheet("Trust") firstrow(varl) cell(A1)

putexcel set "Outputs/Report replication.xlsx", sheet("Trust") modify
putexcel A2:H4, overwri nformat(percent)
putexcel A1:H1, overwri bold hcenter txtwrap

restore

***** Chart 11: DISAGGREGATED PERCEPTIONS

preserve

keep if year==2023

collapse (mean) q2a q2g q2d q2b q2c q2e q2f q1a q1g trust_police q1b q1c q1e q1f , by(country_year support)

drop if support==.

label var q2a "Members of Congress"
label var q2g "Judges & Magistrates"
label var q2d "Police Officers"
label var q2b "Local Government Officers"
label var q2c "National Government Officers"
label var q2e "Prosecutors"
label var q2f "Public Defense Attorneys"

label var q1a "People living in their community" 
label var q1g "Judges & Magistrates"
label var trust_police "Police Officers"
label var q1b "Local Government Officers"
label var q1c "National Government Officers"
label var q1e "Prosecutors"
label var q1f "Public Defense Attorneys"

export excel "Outputs/Report replication.xlsx", sheet("Dissagregated perceptions") firstrow(varl) cell(A1)

putexcel set "Outputs/Report replication.xlsx", sheet("Dissagregated perceptions") modify
putexcel A2:P3, overwri nformat(percent)
putexcel A1:P1, overwri bold hcenter txtwrap

restore


**                       ****
******** SECTION III ********
**                       ****

***** Chart 12.1 Victimization rate, by type of crime

*Group crimes according to category of crimes
gen property_crimes=0
foreach v of varlist EXP_q8a_1 EXP_q8a_2 EXP_q8a_3 EXP_q8a_4 EXP_q8a_5 EXP_q8a_6 EXP_q8a_8 {
replace property_crimes=1 if `v'==1
}

gen life_integrity=0
foreach v of varlist EXP_q8a_7 EXP_q8a_12 EXP_q8b_1 EXP_q8b_2 EXP_q8b_3 {
replace life_integrity=1 if `v'==1
}

gen corruption=0
foreach v of varlist EXP_q8a_9 EXP_q8a_10 EXP_q8a_11 {
replace corruption=1 if `v'==1
}

preserve

keep if year==2023

collapse (mean) corruption property_crimes life_integrity, by(country_year)

label var property_crimes "Property crimes"
label var life_integrity "Against life and integrity"
label var corruption "Corruption, Finance and Commercial"

export excel "Outputs/Report replication.xlsx", sheet("Types of crimes experienced") firstrow(varl) cell(A1)

putexcel set "Outputs/Report replication.xlsx", sheet("Types of crimes experienced") modify
putexcel B2:F2, overwri nformat(percent)
putexcel A1:F1, overwri bold txtwrap

restore


***** Chart 12.2 Data on crime victimization and reporting in COUNTRY

drop crimes_a crimes_b crime

egen crimes_a=rowtotal(EXP_q8a_1 EXP_q8a_2 EXP_q8a_3 EXP_q8a_4 EXP_q8a_5 EXP_q8a_6 EXP_q8a_7 EXP_q8a_8 EXP_q8a_9 EXP_q8a_10 EXP_q8a_11 CAR_q47a_12)
egen crimes_b=rowtotal(EXP_q8a_12 EXP_q8b_1 EXP_q8b_2 EXP_q8b_3 CAR_q47b_5)

*This rate includes crimes outside the country
gen crime_rate=0
replace crime_rate=1 if crimes_a>=1
replace crime_rate=1 if crimes_b>=1

recode EXP_q8d EXP_q8f EXP_q8h (98 99=.)

gen not_of=1-EXP_q8f

tab EXP_q8h, gen(EXP_q8h_)

gen afraid=0 if EXP_q8h!=.
replace afraid=1 if EXP_q8h_3==1
replace afraid=1 if EXP_q8h_9==1
*replace afraid=1 if EXP_q8h_10==1
replace afraid=1 if EXP_q8h_4==1

gen not_help=0 if EXP_q8h!=.
replace not_help=1 if EXP_q8h_7==1
replace not_help=1 if EXP_q8h_5==1
replace not_help=1 if EXP_q8h_2==1
replace not_help=1 if EXP_q8h_6==1

gen admin=0 if EXP_q8h!=.
replace admin=1 if EXP_q8h_1==1
replace admin=1 if EXP_q8h_8==1

preserve

keep if year==2023

collapse (mean) crime_rate EXP_q8d EXP_q8f not_of not_help EXP_q8h_11 afraid admin EXP_q8h_10, by(country_year)

gen not_report=1-EXP_q8d

order country crime_rate EXP_q8d not_report

label var crime_rate "% victims of crime"
label var EXP_q8d "% reported crime"
label var not_report "% did not report crime"
label var EXP_q8f "% official reporting"
label var not_of "% non-official reporting"

label var not_help "Respondent did not think reporting would help"
label var EXP_q8h_11 "Other"
label var afraid "Respondent was afraid or embarrassed"
label var admin "Respondent had administrative issues"
label var EXP_q8h_10 "Respondent did not trust the police"

export excel "Outputs/Report replication.xlsx", sheet("Crime victimization rates") firstrow(varl) cell(A1)

putexcel set "Outputs/Report replication.xlsx", sheet("Crime victimization rates") modify
putexcel B2:R2, overwri nformat(percent)
putexcel A1:R1, overwri bold txtwrap

restore


***** Chart 13.1 Perceptions of Security in COUNTRY Over Time

recode q9 (1 2=1) (3 4=0) (98 99=.)

preserve

keep if year>2012

collapse (mean) q9, by(country_year)

label var q9 "Walking in their neighborhoods at night"

export excel "Outputs/Report replication.xlsx", sheet("Safety over time") firstrow(varl) cell(A1)
putexcel set "Outputs/Report replication.xlsx", sheet("Safety over time") modify
putexcel B2:C4, overwri nformat(percent)
putexcel A1:C1, overwri bold txtwrap

restore

***** Chart 13.2 Perceptions of Security in X, by Sociodemographic Characteristic

*Clean variables for the regression

preserve

drop crimes_a crimes_b crime*

egen crimes_a=rowtotal(EXP_q8a_1 EXP_q8a_2 EXP_q8a_3 EXP_q8a_4 EXP_q8a_5 EXP_q8a_6 EXP_q8a_7 EXP_q8a_8 EXP_q8a_9 EXP_q8a_10 EXP_q8a_11 CAR_q47a_12)
egen crimes_b=rowtotal(EXP_q8a_12 EXP_q8b_1 EXP_q8b_2 EXP_q8b_3 CAR_q47b_5)

*This rate includes crimes outside the country
gen crime_rate=0
replace crime_rate=1 if crimes_a>=1
replace crime_rate=1 if crimes_b>=1

recode q9 (1 2=1) (3 4=0) (98 99=.)
recode q8a (99=0.)

gen gend_r=gend
recode gend_r (1=0) (2=1)

gen urban_r=Urban
recode urban_r (2=0)

gen age_r=0
replace age_r=1 if age<30 & age!=.

gen age_r_car=0
replace age_r_car=1 if age==1 | age==2

gen edu_r=edu
recode edu_r (1 2 3=1) (4 5 6 7=0) (99=.)

gen color_r=COLOR
recode color_r (1 2 3 4=1) (5 6 7 8 9 10 11=0)

gen fin2=fin
recode fin2 (1 2=1) (3 4 5=2)

gen fin_r=fin2
recode fin_r (2=0) (99=.)

*Regressions
log using "Outputs/Security_regressions", replace

logit q9 gend_r urban_r edu_r age_r crime_rate color_r fin_r if country_year=="North Macedonia_2023"
margins, dydx(gend_r urban_r edu_r age_r crime_rate color_r fin_r)

log close

restore

***** Chart 14: Perceptions of the Criminal Justice System

*Chart 14.1

gl criminal "q49a q49b_G1 q49c_G1 q49d_G1 EXP_q23d_G1 q49e_G1 q49c_G2 q49e_G2 q49b_G2"
recode $criminal (1 2=1) (3 4=0) (98 99=.)

preserve

keep if year>=2017

collapse (mean) $criminal, by(country_year)

drop if q49a==.

label var q49a "Effective"
label var q49b_G1 "Timeliness"
label var q49c_G1 "Access"
label var q49d_G1 "Uniform quality old"
label var EXP_q23d_G1 "Uniform quality"
label var q49e_G1 "Appropiate punishments"
label var q49c_G2 "Equal treatment of the accused"
label var q49e_G2 "Presumption of innocence" 
label var q49b_G2 "Equal treatment of victims"

export excel "Outputs/Report replication.xlsx", sheet("CJ 1") firstrow(varl) cell(A1)

putexcel set "Outputs/Report replication.xlsx", sheet("CJ 1") modify
putexcel B2:K4, overwri nformat(percent)
putexcel A1:K1, overwri bold hcenter txtwrap

restore

*Chart 14.2

preserve

keep if year==2023

collapse (mean) $criminal, by(country_year support)

drop if q49a==.
drop if support==.

label var q49a "Effective"
label var q49b_G1 "Timeliness"
label var q49c_G1 "Access"
label var q49d_G1 "Uniform quality old"
label var EXP_q23d_G1 "Uniform quality"
label var q49e_G1 "Appropiate punishments"
label var q49c_G2 "Equal treatment of the accused"
label var q49e_G2 "Presumption of innocence" 
label var q49b_G2 "Equal treatment of victims"

export excel "Outputs/Report replication.xlsx", sheet("CJ 2") firstrow(varl) cell(A1)

putexcel set "Outputs/Report replication.xlsx", sheet("CJ 2") modify
putexcel B2:K4, overwri nformat(percent)
putexcel A1:K1, overwri bold hcenter txtwrap

restore


***** Chart 15: Criminal Justice Actors

recode q48f_G2 q48g_G2 q48h_G1 (1 2=1) (3 4=0) (98 99=.)

gl actors "q1e q1f q1g q2e q2f q2g q48f_G2 q48h_G1 q48g_G2"

preserve

keep if year>2012

collapse (mean) $actors, by(country_year)

label var q1e "Prosecutors"
label var q1f "Defense attorneys"
label var q1g "Judges & Magistrates"

label var q2e "Prosecutors"
label var q2f "Defense Attorneys"
label var q2g "Judges & Magistrates"

label var q48f_G2 "Prosecutors"
label var q48h_G1 "Public defenders"
label var q48g_G2 "Judges & Magistrates"

export excel "Outputs/Report replication.xlsx", sheet("Criminal Justice Actors") firstrow(varl) cell(A2)
putexcel set "Outputs/Report replication.xlsx", sheet("Criminal Justice Actors") modify
putexcel B1:D1 = "Trust", overwri bold merge hcenter txtwrap fpattern(solid,"40 148 170") font(calibri, 11, white)
putexcel E1:G1 = "Corruption", overwri bold merge hcenter txtwrap fpattern(solid,"243 108 33") font(calibri, 11, white)
putexcel H1:J1 = "Effectiveness", overwri bold merge hcenter txtwrap fpattern(solid,"85 86 90") font(calibri, 11, white)
putexcel B3:J5, overwri nformat(percent)
putexcel A2:J2, overwri bold hcenter txtwrap

restore


***** Chart 16: Police

*Efectiveness - Serve the public

recode q48c_G2 EXP_q22i_G2 EXP_q22h_G2 (1 2=1) (3 4=0) (98 99=.)

preserve

keep if year==2023

collapse (mean) q48c_G2 EXP_q22i_G2 EXP_q22h_G2, by(country_year)

label var q48c_G2 "Are available to help when needed" //EXP_q22c_G2, q48c_G2
label var EXP_q22i_G2 "Serve the interests of the community" //EXP_q22i_G2
label var EXP_q22h_G2 "Serve the interests of regular citizens" //EXP_q22h_G2

export excel "Outputs/Report replication.xlsx", sheet("Serve the public") firstrow(varl) cell(A1)

putexcel set "Outputs/Report replication.xlsx", sheet("Serve the public") modify
putexcel B2:E23, overwri nformat(percent)
putexcel A1:E1, overwri bold hcenter txtwrap

restore

*Efectiveness - Crime Control & Safety 

recode q48b_G2 q48a_G2 q48b_G1 EXP_q24e_G2 (1 2=1) (3 4=0) (98 99=.)

preserve

keep if year==2023

collapse (mean) q48b_G2 q48a_G2 q48b_G1 EXP_q24e_G2, by(country_year)

label var q48b_G2 "Help them feel safe" 
label var q48a_G2 "Resolve security problems in the community" //EXP_q22a_G2 q48a_G2 
label var q48b_G1 "Perform effective and lawful investigations" //EXP_q22b_G1 q48b_G1
label var EXP_q24e_G2 "Respond to crime reports" //EXP_q24e_G2

export excel "Outputs/Report replication.xlsx", sheet("Crime control&Safety") firstrow(varl) cell(A1)
putexcel set "Outputs/Report replication.xlsx", sheet("Crime control&Safety") modify
putexcel B2:G2, overwri nformat(percent)
putexcel A1:G1, overwri bold hcenter txtwrap

restore


*Legitimacy - Due process

recode q48a_G1 q48c_G1 q48d_G2 (1 2=1) (3 4=0) (98 99=.)
recode EXP_q22e_G1 (3 4=1) (1 2=0) (98 99=.)

preserve

keep if year==2023

collapse (mean) EXP_q22e_G1 q48d_G2 q48a_G1 q48c_G1, by(country_year)

label var EXP_q22e_G1 "Police do not use excessive force" 
label var q48d_G2 "Treat all people with respect" //EXP_q22d_G2 q48d_G2
label var q48a_G1 "Act lawfully" //EXP_q22a_G1 q48a_G1
label var q48c_G1 "Respect the rights of suspects" //EXP_q22c_G1 q48c_G1


export excel "Outputs/Report replication.xlsx", sheet("Due process") firstrow(varl) cell(A1)

putexcel set "Outputs/Report replication.xlsx", sheet("Due process") modify
putexcel B2:F23, overwri nformat(percent)
putexcel A1:F1, overwri bold hcenter txtwrap

restore

*Legitimacy - Discrimination

gl discrim q18a_norm q18c_norm q18e_norm q18d_norm EXP_q17g_norm

preserve

keep if year==2023

collapse (mean) $discrim, by(country_year)

label var q18a_norm "Economic status"
label var q18c_norm "Ethnic background"
label var q18e_norm "Foreigner status"
label var q18d_norm "Relgion"
label var EXP_q17g_norm "Skin color"

export excel "Outputs/Report replication.xlsx", sheet("Discrimination") firstrow(varl) cell(A1)

putexcel set "Outputs/Report replication.xlsx", sheet("Discrimination") modify
putexcel B2:K2, overwri nformat(percent)
putexcel A1:K1, overwri bold hcenter txtwrap

restore

*Legitimacy - Corruption

gen corr_pol=1-q2d
recode q48e_G2 (1 2=1) (3 4=0) (98 99=.)
recode EXP_q22k_G2 EXP_q22j_G2 (3 4=1) (1 2=0) (98 99=.)

preserve

keep if year==2023

collapse (mean) corr_pol EXP_q22k_G2 q48e_G2 EXP_q22j_G2, by(country_year)

label var corr_pol "Are not involved in corrupt practices" //q2d
label var EXP_q22k_G2 "Do not serve the interests of gangs" //EXP_q22k_G2 
label var q48e_G2 "Investigate crimes in an independent manner" //EXP_q22e_G2 q48e_G2
label var EXP_q22j_G2 "Do not serve the interests of politicians" //EXP_q22j_G2

export excel "Outputs/Report replication.xlsx", sheet("Police Corruption") firstrow(varl) cell(A1)

putexcel set "Outputs/Report replication.xlsx", sheet("Police Corruption") modify
putexcel B2:F23, overwri nformat(percent)
putexcel A1:F1, overwri bold hcenter txtwrap

restore

*Legitimacy - Trust

recode EXP_q8e (98 99=.)
tab EXP_q8e, gen(report_)

preserve

keep if year==2023

collapse (mean) q9 trust_police report_1, by(country_year)

label var q9 "Feel safe in their neighborhoods"
label var trust_police "Trust police"
label var report_1 "Report a crime when they are a victim" //EXP_q8e==1


export excel "Outputs/Report replication.xlsx", sheet("Trust and crime reporting") firstrow(varl) cell(A1)

putexcel set "Outputs/Report replication.xlsx", sheet("Trust and crime reporting") modify
putexcel B2:E23, overwri nformat(percent)
putexcel A1:E1, overwri bold hcenter txtwrap

restore

*Legitimacy - Accountability

preserve

keep if year==2023

recode q48d_G1 EXP_q22f_G1 EXP_q22g_G1 EXP_q22h_G1 (1 2=1) (3 4=0) (98 99=.)

collapse (mean) q48d_G1 EXP_q22h_G1 EXP_q22g_G1 EXP_q22f_G1, by(country_year)

label var q48d_G1 "Are held accountable for violating laws" // EXP_q22d_G1 q48d_G1
label var EXP_q22h_G1 "Are investigated for misconduct" // EXP_q22h_G1
label var EXP_q22g_G1 "Are held accountable for accepting bribes" //EXP_q22g_G1
label var EXP_q22f_G1 "Are held accountable for seeking bribes" //EXP_q22f_G1

export excel "Outputs/Report replication.xlsx", sheet("Accountability and Sanctions") firstrow(varl) cell(A1)

putexcel set "Outputs/Report replication.xlsx", sheet("Accountability and Sanctions") modify
putexcel B2:F23, overwri nformat(percent)
putexcel A1:F1, overwri bold hcenter txtwrap

restore

***** Chart 17: Victim Support

gl victim EXP_q24g_G2 EXP_q24a_G2 EXP_q24f_G2 EXP_q24b_G2 EXP_q24c_G2 EXP_q23f_G1 EXP_q24b_G1 EXP_q24a_G1 EXP_q24d_G2 EXP_q24d_G1 EXP_q24c_G1

recode $victim (1 2 =1) (3 4=0) (98 99=.)

preserve

keep if year==2023

collapse (mean) $victim, by(country_year)

label var EXP_q24g_G2 "Are addressed by the police using accessible language"
label var EXP_q24a_G2 "Receive protection from the police if their safety is in danger"
label var EXP_q24f_G2 "Receive a clear explanation of the process when reporting a crime to the police"
label var EXP_q24b_G2 "Receive protection during criminal proceedings to prevent repeated victimization"
label var EXP_q24c_G2 "Receive adequate care and protection as victims of sexual crimes"
label var EXP_q23f_G1 "Are guaranteed their rights in criminal justice proceedings"
label var EXP_q24b_G1 "Are believed when they report a crime"
label var EXP_q24a_G1 "Receive prompt and courteous attention when reporting a crime"
label var EXP_q24d_G2 "Receive adequate care and protection as victims of domestic violence"
label var EXP_q24d_G1 "Receive information and legal advice when going to the authorities"
label var EXP_q24c_G1 "Receive effective and timely medical and psychological care"

export excel "Outputs/Report replication.xlsx", sheet("Victim Support") firstrow(varl) cell(A1)

putexcel set "Outputs/Report replication.xlsx", sheet("Victim Support") modify
putexcel A1:M1, overwri bold hcenter txtwrap
putexcel B2:M23, overwri nformat(percent)

restore

***** Chart 18: A2J

global variable "q19_A1 q19_A2 q19_A3 q19_B1 q19_B2 q19_B3 q19_B4 q19_C1 q19_C2 q19_C3 q19_C4 q19_D1 q19_D2 q19_D3 q19_D4 q19_D5 q19_D6 q19_E1 q19_E2 q19_E3 q19_F1 q19_F2 q19_G1 q19_G2 q19_G3 q19_H1 q19_H2 q19_H3 q19_I1 q19_J1 q19_J2 q19_J3 q19_J4 q19_K1 q19_K2 q19_K3 q19_L1 q19_L2" 
global variable1 "q21"
global variable2 "q22a q22b"
global variable3 "q23 q24"
global variable4 "q25_1 q25_2 q25_3 q25_4 q25_5 q25_6 q25_7 q25_8 q25_9 q25_99"
global variable5 " q27 q28 q29 q30"
global variable6 "q32a q32b q32c q32d q32f q32g"
global variable7 "q33a q33b q33c q33d q33e q33f q33g"
global variable8 "q34"
global variable9 "q36a q36b q36c"
global variable10 "q37a q37c q37d"
global variable11 "q38 q39"
global variable12 "q40_1 q40_2 q40_3 q40_4 q40_5 q40_6 q40_7"
global variable13 "q41a q41b q41c q41d"
global variable14 "q42a q42b q42c q42d q42g q42g_1 q42h q42h_1"

// Numeric variables
global variable15 "q37b q42e q42f"

recode $variable (99=.)  
recode $variable2 (99=.) 
recode $variable3 (99=.) 
recode $variable4 (99=.) 
recode $variable5 (99=.) 
recode $variable6 (99=.) 
recode $variable7 (99=.) 
recode $variable8 (99=.) 
recode $variable9 (99=.) 
recode $variable10 (99=.) 
recode $variable11 (99=.) 
recode $variable12 (99=.) 
recode $variable13 (99=.) 
recode $variable14 (99=.) 
recode $variable15 (99=.)


**** Part 1 ****

**Overall**
drop ndisputes
egen ndisputes=rowtotal(q19_A1-q19_L2)

**Overall**
gen had_dispute=0 if ndisputes==0
replace had_dispute=1 if ndisputes>0

drop q34_merge

gen q34_merge=q34
replace q34_merge=q30 if q30!=.

gen q35_merge18=q35
replace q35_merge18=q31+3 if q31<99
replace q35_merge18=99 if q31==99

//Incidence by problem groups 

*Accidental Illness & Injury, coded as 0 if F1 and F2 are answered as no, 1 otherwise
gen Injury=0
foreach y in F1 F2 {
replace Injury=1 if q19_`y'==1
}

*Citizenship & ID, which is coded as 0 if J1, J2, J3 are answered as no, 1 otherwise. Immigration here is grouped with the rest, this might not be the right call
gen Citizen=0
foreach x in J1 J2 J3 {
replace Citizen=1 if q19_`x'==1
}

*Community & Natural Resources, coded as 0 if E3 and H3 are answered as no, 1 otherwise (at least one yes). B4 Could go here
gen Community=0
foreach y in  H3 E3 {
replace Community=1 if q19_`y'==1
}

*Consumer problems, coded as 0 if A1, A2 and A3 are answered as no, 1 otherwise (at least one yes). Once again a reminder that all 37 variables in q34 are coded as 1=Yes, 0=No
gen Consumer=0
foreach x in A1 A2 A3 {
replace Consumer=1 if q19_`x'==1
}

*Employment problems, coded as 0 if G1, G2 and G3 are answered as no, 1 otherwise (at least one yes).
gen Employment=0
foreach x in G1 G2 G3 {
replace Employment=1 if q19_`x'==1
}

*Education problems, coded as 0 if E1 and E2  are answered as no, 1 otherwise (at least one yes).
gen Education=0
foreach x in E1 E2 {
replace Education=1 if q19_`x'==1
}

*Family problems, coded as 0 if D1, D2, D3, D4, D5 and D6 are answered as no, 1 otherwise (at least one yes). 
gen Family=0
foreach y in D1 D2 D3 D4 D5 D6 {
replace Family=1 if q19_`y'==1
}

*Housing problems, coded as 0 if C1, C2, C3 and C4 are answered as no, 1 otherwise (at least one yes).
gen Housing=0
foreach x in C1 C2 C3 C4 {
replace Housing=1 if q19_`x'==1
}

*Land problems, coded as 0 if B1, B2, B3 and B4 are answered as no, 1 otherwise (at least one yes). B4 Could be considered a community problem as well
gen Land=0
foreach x in B1 B2 B3 B4 {
replace Land=1 if q19_`x'==1
}

*Law Enforcement problems, for this one we only have q19_I1	 (Being beaten up or arrested without justification by a member of the police or the military.)
gen law_enf=q19_I1

*Money & Debt problems, coded as 0 if L1 and L2 are answered as no, 1 otherwise (at least one yes).
gen MoneyDebt=0
foreach x in L1 L2 K1 K2 K3 {
replace Money=1 if q19_`x'==1
}

*Public Services, coded as 0 if H1 and H2 are answered as no, 1 otherwise
gen Govt_payment=0
foreach x in H1 H2 J4 {
replace Govt_payment=1 if q19_`x'==1
}

preserve

keep if year==2023

collapse (mean) had_dispute Consumer MoneyDebt Govt_payment Housing Land Community Employment Family Citizen Injury Education law_enf, by(country_year) 

label var had_dispute "Total incidence"
label var Injury "Injury"
label var Citizen "Citizenship & ID"
label var Community "Commmunity Resources"
label var Consumer "Consumer"
label var Employment "Employment"
label var Education "Education"
label var Family "Family"
label var Housing "Housing"
label var Land "Land"
label var law_enf "Law Enforcement"
label var MoneyDebt "Money & Debt"
label var Govt_payment "Public Services"

export excel using "Outputs/Report replication.xlsx" , sheet("1. Incidence") firstrow(varlabel)
putexcel set "Outputs/Report replication.xlsx", sheet("1. Incidence") modify
putexcel A1:N1, overwri bold hcenter txtwrap
putexcel B2:N2, overwri nformat(percent)

restore 

**** Part 2 ****

* Legal Capability 

gen get_advice=q41b 
recode get_advice (1 2=1) (3 4=0) (99=.)

gen allexpert=q41c
recode allexpert (1 2=1) (3 4=0) (99=.)

gen conf_outcome=q41d 
recode conf_outcome (1 2=1) (3 4=0) (99=.)

preserve

keep if year==2023

collapse (mean) get_advice allexpert conf_outcome, by(country_year) 

label var get_advice "Information"
label var allexpert "Expert_Help"
label var conf_outcome "Confidence"

export excel using "Outputs/Report replication.xlsx" , sheet("2. Legal capability") firstrow(varlabel)
putexcel set "Outputs/Report replication.xlsx", sheet("2. Legal capability") modify
putexcel A1:D1, overwri bold hcenter txtwrap
putexcel B2:D2, overwri nformat(percent)

restore

**** Part 3 ****

egen advisors=rowtotal(q25_1 -q25_99)

* Type of advisor 
gen friendfamily=0 if advisors>0
replace friendfamily=1 if q25_1==1 

gen lawyer=0 if advisors>0
replace lawyer=1 if q25_2==1

gen govlegalaid=0 if advisors>0 
replace govlegalaid=1 if q25_3==1

gen courtgovbody=0 if advisors>0
replace courtgovbody=1 if q25_4==1 

gen healthprof=0 if advisors>0
replace healthprof=1 if q25_5==1 

gen tradeunion=0 if advisors>0
replace tradeunion=1 if q25_6==1 

gen religious=0 if advisors>0
replace religious=1 if q25_7==1 

gen civilsoc=0 if advisors>0
replace civilsoc=1 if q25_8==1 

gen otherorg=0 if advisors>0
replace otherorg=1 if q25_9==1 

preserve

keep if year==2023

collapse (mean) q24 friendfamily lawyer otherorg civilsoc healthprof courtgovbody tradeunion govlegalaid religious, by(country_year) 

label var q24 "Sources of help"
label var friendfamily "Friend or family"
label var lawyer "Lawyer or Professional Advice Service"
label var otherorg "Other Organization"
label var civilsoc "Civil Society Organization or Charity"
label var healthprof "Health or Welfare Professional"
label var courtgovbody "Court or Government Body or Police"
label var tradeunion "Trade Union or Employer"
label var govlegalaid "Government Legal Aid Office"
label var religious "Religious or Community Leader"

export excel using "Outputs/Report replication.xlsx" , sheet("3. Sources of help") firstrow(varlabel)
putexcel set "Outputs/Report replication.xlsx", sheet("3. Sources of help") modify
putexcel A1:L1, overwri bold hcenter txtwrap
putexcel B2:L2, overwri nformat(percent) 

restore

**** Part 4 ****

gen done_fully=.
replace done_fully=1 if q34_merge==4 
replace done_fully=0 if q34_merge==1 |  q34_merge==2 |  q34_merge==3

gen done_partially=.
replace done_partially=1 if q34_merge==3 
replace done_partially=0 if q34_merge==1 |  q34_merge==2 |  q34_merge==4

gen ongoing=.
replace ongoing=1 if q34_merge==1 | q34_merge==2
replace ongoing=0 if q34_merge==3 |  q34_merge==4 

preserve

keep if year==2023

collapse (mean) done_fully done_partially, by(country_year) 

label var done_fully "Fully_Resolved"
label var done_partially "Problem_Persists"

export excel using "Outputs/Report replication.xlsx" , sheet("4. Problem Status") firstrow(varlabel) 
putexcel set "Outputs/Report replication.xlsx", sheet("4. Problem Status") modify
putexcel A1:C1, overwri bold hcenter txtwrap
putexcel B2:C2, overwri nformat(percent) 

restore

**** Part 5 ****

gen fair=q36a
gen slow=q36b
gen expensive=q36c

* Duration (we are only excluding the 99s , same as report 2018) 
gen duration=q37b if q28==1 & q37b>=0 & q37b!=99

/*
gen financialdiff=0 if q37c!=.
replace financialdiff=1 if q37d==3
replace financialdiff=1 if q37d==4
*/

gen financialdiff=0 if q34_merge==3 | q34_merge==4
replace financialdiff=1 if q37d==3
replace financialdiff=1 if q37d==4

*Satisfied with the outcome
gen satisfied=q38
recode satisfied (1 2=1) (3 4=0) (99=.)

* Process was in their favor 

gen favor=0 if q37a<98
replace favor=1 if q37a==1

* Satisfaction so far 
gen sat_ongoing=q39
recode sat_ongoing (1 2=1) (3 4=0) (99=.)

preserve

keep if year==2023

collapse (mean) fair duration financialdiff, by(country_year) 

label var fair "Fair"
label var duration "Time"
label var financialdiff "Financial_Difficulty"

order country

export excel using "Outputs/Report replication.xlsx" , sheet("5. Process") firstrow(varlabel) 
putexcel set "Outputs/Report replication.xlsx", sheet("5. Process") modify
putexcel A1:D1, overwri bold hcenter txtwrap
putexcel B2:B9, overwri nformat(percent) 
putexcel D2:D9, overwri nformat(percent) 

restore


**** Part 6 ****

*Hardship

egen hardship=rowtotal(q42a-q42d)
gen Had_hardship=0 if hardship==0 & had_dispute==1 
replace Had_hardship=1 if hardship>0 

gen missingwork=0 if q42e==0
replace missingwork=1 if q42e!=0 
replace missingwork=. if q42e==. 

gen visithealthcare=q42g 

gen hospitalization=q42h 

preserve

keep if year==2023

collapse (mean) Had_hardship q42a q42c q42b q42d, by(country_year) 

label var Had_hardship "Experienced a hardship"
label var q42a "Health"
label var q42c "Economic"
label var q42b "Interpersonal"
label var q42d "Substance_Abuse"

order country


export excel using "Outputs/Report replication.xlsx" , sheet("6. Problem Impact") firstrow(varlabel) 
putexcel set "Outputs/Report replication.xlsx", sheet("6. Problem Impact") modify
putexcel A1:F1, overwri bold hcenter txtwrap
putexcel B2:F2, overwri nformat(percent) 

restore




