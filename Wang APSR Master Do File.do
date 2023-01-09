************************************************************************************************************
*Replication*
*Blood Is Thicker Than Water: Elite Kinship Networks and State Building in Imperial China*

*American Political Science Review*

*Yuhua Wang*
*Department of Government*
*Harvard University*
*yuhuawang@fas.harvard.edu*

*Created on January 30, 2019*
*This version: November 17, 2021*
*Stata 16.1*
************************************************************************************************************

************************
*Preface*
************************

*net install st0594.pkg
*net install pdslasso.pkg
*net install interflex
*put poet.ado (included the replication file) in your Stata ado folder (poet.ado is downloaded from http://www.stephenchaudoin.com/: poet.zip under "Do We Really Know the WTO Cures Cancer?")

clear
estimates clear 
set more off

*Change this directory to your local folder
cd "/Users/ywang/Dropbox/Book Project on State Building/Wang Anshi reform article/journal submission/apsr/final/replication file/"

************************************
*Standardize all variables*
************************************

use "Wang APSR Master Data",clear

global y "support_continuous support_dummy"
global x "kinproximity kinproximity_children kinproximity_politician kinproximity_wd kin_county_w kin_prefecture_w kin_province_w kinproximity_wp1 kinproximity_wp2 kinproximity_wp3 kinproximity_wp4 kinproximity_wp5 kinproximity_wp6 kinproximity_wp7 kinproximity_wp8 kinproximity_wp9"
global controls "rank_change betweenness degree bonacich kin_node children supportfaction kinmedianexposuretoexternal50 kinmedianexposuretointernal50 kinexposuretoexternalwar kinexposuretomassrebel avg_ruggedness father_exam dist_grandfather_father father_official grandfather_official uncle_any_official rank_first rank_ave rank_max"

center $y $x $controls if sample==1, inplace standardize

*clean up all the labels*

label var	kin_node	"N of kin"
label var	support_dummy	"Support for reform (dichotomous)"
label var	support_continuous	"Support for reform (continuous)"
label var	kinproximity	"Local concentration of kin"
label var	kinproximity_wp1	"Local concentration of kin (matrilineal discount 0.1)"
label var	kinproximity_wp2	"Local concentration of kin (matrilineal discount 0.2)"
label var	kinproximity_wp3	"Local concentration of kin (matrilineal discount 0.3)"
label var	kinproximity_wp4	"Local concentration of kin (matrilineal discount 0.4)"
label var	kinproximity_wp5	"Local concentration of kin (matrilineal discount 0.5)"
label var	kinproximity_wp6	"Local concentration of kin (matrilineal discount 0.6)"
label var	kinproximity_wp7	"Local concentration of kin (matrilineal discount 0.7)"
label var	kinproximity_wp8	"Local concentration of kin (matrilineal discount 0.8)"
label var	kinproximity_wp9	"Local concentration of kin (matrilineal discount 0.9)"
label var	kinproximity_wd	"Local concentration of kin (relational distance discount)"
label var	kin_county_w	"Herfindahl index of kin concentration (county)"
label var	kin_prefecture_w	"Herfindahl index of kin concentration (prefecture)"
label var	kin_province_w	"Herfindahl index of kin concentration (province)"
label var	degree	"Degree centrality"
label var	betweenness	"Betweenness centrality"
label var	bonacich	"Bonacich power"
label var	supportfaction	"Factional tie with reform leader"
label var	rank_change	"Politician rank change"
label var	father_exam	"Father exam"
label var	father_official	"Father official status"
label var	grandfather_official	"Grandfather official status"
label var	kinexposuretoexternalwar	"Kin exposure to external wars"
label var	kinexposuretomassrebel	"Kin exposure to mass rebellions"
label var	kinmedianexposuretoexternal50	"Kin centroid exposure to external wars"
label var	kinmedianexposuretointernal50	"Kin centroid exposure to mass rebellions"
label var	children	"N of children"
label var	kinproximity_children	"Local concentration of kin/N of children"
label var	avg_ruggedness	"Ruggedness Index"
label var	dist_grandfather_father	"Father migration"
label var	kinproximity_politician	"Local concentration of kin (politician)"
label var	uncle_any_official	"Uncle official status"
label var	rank_first	"Politician's first rank"
label var	rank_ave	"Politician's average rank"
label var	rank_max	"Politician's highest rank"

*save the dataset*

save "Wang APSR Master Data_st.dta", replace

************************************************************************
*Tables and figures in the main text*
************************************************************************

************************************
*FIGURE 1. Examples of Kinship Networks*
************************************

*See Wang APSR R Script.R*

************************************
*FIGURE 2. Example of a Kinship Network*
************************************

*Constructed in LaTex using tikz*

************************************
*FIGURE 3. Tomb Epitaph Example*
************************************

*Photos*

************************************
*TABLE 1. OLS Estimates of the Correlation between Father Migration and Geography of Kinship Network*
************************************

use "Wang APSR Master Data_st.dta", clear

local controls "i.hometown_prefecture_id"

xi: reg kinproximity dist_grandfather_father if sample==1,cl(hometown_prefecture_id)
estimates store fathermodel1
estadd ysumm

xi: reg kinproximity dist_grandfather_father `controls' if sample==1,cl(hometown_prefecture_id)
estimates store fathermodel2
estadd ysumm

esttab fathermodel1 fathermodel2 using fathermodel.tex, ///
	replace fragment keep(dist_grandfather_father) ///
	b(3) se(3) posthead(\cmidrule(lr){2-3}) ///
	cells(b(star fmt(3)) se(fmt(3) par)) staraux star(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
	label booktabs noconstant obslast nodepvars nomtitles nolines gaps ///
	scalars("r2 \(R^{2}\)") sfmt(3) ///
	stats(ymean ysd N r2, fmt(3 3 0 3) labels("Outcome mean" "Outcome std.dev." "Observations" "R2")) ///
	indicate("Prefecture FE = *Ihometown*")

************************************
*FIGURE 4. Major Politicians’ Attitudes toward the State-Building Reform*
************************************

*See Wang APSR R Script.R*

************************************
*TABLE 2. OLS Estimates of the Correlation between Geography of Kinship Network and Support for Reform*
************************************

use "Wang APSR Master Data_st.dta", clear

local controls1 "i.hometown_prefecture_id"
local controls2 "betweenness kin_node children supportfaction rank_ave kinmedianexposuretoexternal50 kinmedianexposuretointernal50  avg_ruggedness father_exam dist_grandfather_father i.hometown_prefecture_id"

xi: reg support_continuous kinproximity,cl(hometown_prefecture_id)
estimates store benchmarkmodel1
estadd ysumm

xi: reg support_continuous kinproximity `controls1',cl(hometown_prefecture_id)
estimates store benchmarkmodel2
estadd ysumm

xi: reg support_continuous kinproximity `controls2',cl(hometown_prefecture_id)
estimates store benchmarkmodel3
estadd ysumm

pdslasso support_continuous kinproximity _Ihometown__11123-_Ihometown__101191 (betweenness kin_node children supportfaction kinmedianexposuretoexternal50 kinmedianexposuretointernal50  avg_ruggedness father_exam dist_grandfather_father),cl(hometown_prefecture_id)
estimates store benchmarkmodellasso
estadd ysumm

esttab benchmarkmodel1 benchmarkmodel2 benchmarkmodel3 benchmarkmodellasso using benchmarkmodelfull.tex, ///
	replace fragment keep(kinproximity betweenness kin_node children supportfaction rank_ave kinmedianexposuretoexternal50 kinmedianexposuretointernal50  avg_ruggedness father_exam dist_grandfather_father) ///
	b(3) se(3) posthead(\cmidrule(lr){2-5}) ///
	cells(b(star fmt(3)) se(fmt(3) par)) staraux star(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
	 label booktabs noconstant obslast nodepvars nomtitles nolines gaps ///
	scalars("r2 \(R^{2}\)" ) sfmt(3) ///
	stats(ymean ysd N r2, fmt(3 3 0 3) labels("Outcome mean" "Outcome std.dev." "Observations" "R2")) ///
	indicate("Prefecture FE = *Ihometown_*")
	
************************************************************************
*Tables and Figures in the Supplementary Materials*
************************************************************************

************************************
*Figure A1-1: Northern Song Borders, 960–1127 CE*
************************************

*Map constructed in QGIS*

************************************
*Figure A1-2: Major Politicians’ Local Concentration of Kin (Estimating Sample)*
************************************

*See Wang APSR R Script.R*

************************************
*Figure A1-3: Major Politicians’ Attitudes toward the State-Building Reform (Estimating Sample)*
************************************

*See Wang APSR R Script.R*

************************************
*Figure A1-4: Correlations between Major Politicians’ Attitudes toward the State-Building Re- form and Their Political Ranks*
************************************

*See Wang APSR R Script.R*

************************************
*Figure A1-5: Northern Song Politicians Marriage Network, 1067–1085 CE*
************************************

*See Wang APSR R Script.R*

************************************
*Figure A1-6: Conflicts in Northern Song, 1016–1065 CE*
************************************

*Map constructed in QGIS*
*raw data on external war battles, see "external war 1016-1065.csv" in the replication package*
*raw data on mass rebellion battles, see "mass rebellions 1016-1065.csv" in the replication package*

************************************
*Table A1-1: Summary Statistics (Whole Sample)*
************************************

use "Wang APSR Master Data.dta", clear

global y "support_continuous policy_opinion_tri support_dummy"
global x "kinproximity kinproximity_children kinproximity_politician kinproximity_wd kin_county_w kin_prefecture_w kin_province_w kinproximity_wp1 kinproximity_wp2 kinproximity_wp3 kinproximity_wp4 kinproximity_wp5 kinproximity_wp6 kinproximity_wp7 kinproximity_wp8 kinproximity_wp9"
global controls "rank_change betweenness degree bonacich kin_node children children_group supportfaction rank_first rank_ave rank_max kinmedianexposuretoexternal50 kinmedianexposuretointernal50 kinexposuretoexternalwar kinexposuretomassrebel avg_ruggedness father_exam dist_grandfather_father father_official grandfather_official uncle_any_official"

estpost sum  $y $x $controls

esttab using sumstats_whole.tex, modelwidth(10 10 20 20 20) cell((count(label(N)) ///
	mean(fmt(3) label(Mean)) sd(fmt(3) label(Standard Deviation)) min(fmt(3) label(Min.)) max(fmt(3) label(Max.)))) ///
	plain noobs label nomtitle nonumber replace

************************************
*Table A1-2: Summary Statistics (Estimating Sample)*
************************************

use "Wang APSR Master Data.dta", clear

global y "support_continuous policy_opinion_tri support_dummy"
global x "kinproximity kinproximity_children kinproximity_politician kinproximity_wd kin_county_w kin_prefecture_w kin_province_w kinproximity_wp1 kinproximity_wp2 kinproximity_wp3 kinproximity_wp4 kinproximity_wp5 kinproximity_wp6 kinproximity_wp7 kinproximity_wp8 kinproximity_wp9"
global controls "rank_change betweenness degree bonacich kin_node children children_group supportfaction rank_first rank_ave rank_max kinmedianexposuretoexternal50 kinmedianexposuretointernal50 kinexposuretoexternalwar kinexposuretomassrebel avg_ruggedness father_exam dist_grandfather_father father_official grandfather_official uncle_any_official"

estpost sum  $y $x $controls if sample==1

esttab using sumstats_estimating.tex, modelwidth(10 10 20 20 20) cell((count(label(N)) ///
	mean(fmt(3) label(Mean)) sd(fmt(3) label(Standard Deviation)) min(fmt(3) label(Min.)) max(fmt(3) label(Max.)))) ///
	plain noobs label nomtitle nonumber replace

************************************
*Table A1-3: Comparing Estimating Sample with Observations with Missing Data*
************************************

use "Wang APSR Master Data.dta",clear

ttest support_continuous,by(sample)
ttest kinproximity,by(sample)
ttest betweenness,by(sample)
ttest kin_node,by(sample)
ttest children,by(sample)
ttest supportfaction,by(sample)
ttest rank_ave,by(sample)
ttest kinmedianexposuretoexternal50,by(sample)
ttest kinmedianexposuretointernal50,by(sample)
ttest avg_ruggedness,by(sample)
ttest father_exam,by(sample)
ttest dist_grandfather_father,by(sample)	

************************************
*Table A1-4: Distribution of Politicians across Prefectures*
************************************

use "Wang APSR Master Data.dta",clear

collapse (sum) count if sample==1,by(hometown_prefecture_id)

list hometown_prefecture_id count

************************************
*Table A1-5: Distribution of Politicians across Provinces*
************************************

use "Wang APSR Master Data.dta",clear

collapse (sum) count if sample==1,by(hometownprovince_id)

list hometownprovince_id count

************************************
*Table A1-6: Family Members’ Occupations and Geography of Kinship Network: OLS Estimates*
************************************	
	
use "Wang APSR Master Data_st.dta", clear

local controls "i.hometown_prefecture_id"

xi: reg kinproximity father_official if sample==1,cl(hometown_prefecture_id)
estimates store trademodel1
estadd ysumm

xi: reg kinproximity grandfather_official if sample==1,cl(hometown_prefecture_id)
estimates store trademodel2
estadd ysumm

xi: reg kinproximity uncle_any_official if sample==1,cl(hometown_prefecture_id)
estimates store trademodel3
estadd ysumm

xi: reg kinproximity father_official grandfather_official uncle_any_official if sample==1,cl(hometown_prefecture_id)
estimates store trademodel4
estadd ysumm

xi: reg kinproximity father_official grandfather_official uncle_any_official `controls' if sample==1,cl(hometown_prefecture_id)
estimates store trademodel5
estadd ysumm

esttab trademodel1 trademodel2 trademodel3 trademodel4 trademodel5 using trademodel.tex, ///
	replace fragment keep(father_official grandfather_official uncle_any_official) ///
	b(3) se(3) posthead(\cmidrule(lr){2-6}) ///
	cells(b(star fmt(3)) se(fmt(3) par)) staraux star(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
	label booktabs noconstant obslast nodepvars nomtitles nolines gaps ///
	scalars("r2 \(R^{2}\)") sfmt(3) ///
	stats(ymean ysd N r2, fmt(3 3 0 3) labels("Outcome mean" "Outcome std.dev." "Observations" "R2")) ///
	indicate("Prefecture FE = *Ihometown*")

************************************
*Table A1-7: Marginal Effect of Local Concentration of Kin on Support for Reform Conditional on Kin Exposure to External Wars and Kin Exposure to Mass Rebellions: OLS Estimates*
************************************

use "Wang APSR Master Data_st.dta", clear

*generating interaction terms and label them*
gen kinproximity_exposuretoexternal=kinproximity*kinexposuretoexternalwar
gen kinproximity_exposuretomass=kinproximity*kinexposuretomassrebel

label var kinproximity_exposuretoexternal "Local Concentration of kin*Kin exposure to external wars"
label var kinproximity_exposuretomass "Local Concentration of kin*Kin exposure to mass rebellions"

xi: reg support_continuous kinproximity kinexposuretoexternalwar kinproximity_exposuretoexternal,cl(hometown_prefecture_id)
estimates store interact1
estadd ysumm

xi: reg support_continuous kinproximity kinexposuretomassrebel kinproximity_exposuretomass,cl(hometown_prefecture_id)
estimates store interact2
estadd ysumm

esttab interact1 interact2 using interactmodel.tex, ///
	replace fragment keep(kinproximity kinexposuretoexternalwar kinproximity_exposuretoexternal kinexposuretomassrebel kinproximity_exposuretomass) ///
	order(kinproximity kinexposuretoexternalwar kinproximity_exposuretoexternal kinexposuretomassrebel kinproximity_exposuretomass) ///
	b(3) se(3) posthead(\cmidrule(lr){2-3}) ///
	cells(b(star fmt(3)) se(fmt(3) par)) staraux star(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
	label booktabs noconstant obslast nodepvars nomtitles nolines gaps ///
	scalars("r2 \(R^{2}\)") sfmt(3) ///
	stats(ymean ysd N r2, fmt(3 3 0 3) labels("Outcome mean" "Outcome std.dev." "Observations" "R2"))
	
************************************
*Figure A1-7: Marginal Effect of Local Concentration of Kin on Support for Reform Conditional on Kin Exposure to External Wars*
************************************

use "Wang APSR Master Data_st.dta", clear

interflex support_continuous kinproximity kinexposuretoexternalwar, cl(hometown_prefecture_id) type(linear) ylabel(Support for reform) dlabel(Local Concentration of Kin) xlabel(Kin Exposure to External Wars)

************************************
*Figure A1-8: Marginal Effect of Local Concentration of Kin on Support for Reform Conditional on Kin Exposure to Mass Rebellions*
************************************

use "Wang APSR Master Data_st.dta", clear

interflex support_continuous kinproximity kinexposuretomassrebel, cl(hometown_prefecture_id) type(linear) ylabel(Support for Reform) dlabel(Local Concentration of Kin) xlabel(Kin Exposure to Mass Rebellions)

************************************
*Figure A1-9: OLS Estimates with Different Matrilineal Discount Rates*
************************************

use "Wang APSR Master Data_st.dta", clear

local wp "1 2 3 4 5 6 7 8 9"

local lassocontrols "dist_grandfather_father i.hometown_prefecture_id"

foreach p of local wp {
	qui xi: reg support_continuous kinproximity_wp`p' `lassocontrols',cl(hometown_prefecture_id)
	parmest, le(90 95) sa("matrilinealdiscount_`p'.dta", replace)
}

local wp "1 2 3 4 5 6 7 8 9"

foreach p of local wp {
	use "matrilinealdiscount_`p'.dta", clear
	gen N=_n
	keep if N==1
	gen matrilinealdis="`p'"
	keep parm estimate min90 max90 min95 max95 matrilinealdis
	save, replace
}

local wp "1 2 3 4 5 6 7 8 9"
use "matrilinealdiscount_1.dta", clear
foreach p of local wp {
	append using "matrilinealdiscount_`p'.dta"
	}
drop in 1
save "matrilinealdiscount.dta", replace

encode matrilinealdis, gen(matrilinealdiscount)
sort matrilinealdiscount
save, replace

use "matrilinealdiscount.dta", clear

label define yaxis 1 "0.1" 2 "0.2" 3 "0.3" 4 "0.4" 5 "0.5" ///
	6 "0.6" 7 "0.7" 8 "0.8" 9 "0.9"
label values matrilinealdiscount yaxis

twoway (rcap min90 max90 matrilinealdiscount, horizontal) ///
     (rcap min95 max95 matrilinealdiscount, msize(tiny) horizontal) ///
	(scatter matrilinealdiscount estimate,msize(small) msymbol(o) mcol(gs0)) ///
	, scheme(s1mono) ///
	legend(off) ///
	xscale(range(-0.6 .1)) xlabel(-0.6(.1) 0.1) xtitle("Standardized Coefficient") ///
	xline(0, lpattern(dash)) ///
	yscale(range(1 9)) yscale(reverse) ylabel(1(1)9, angle(0) valuelabel labsize(vsmall)) ytick(1(1)9) ytitle("Matrilineal Discount") 
graph export matrilinealdiscount.png, as(png) width(2500) replace
cap graph close graph

************************************
*Figure A1-10: OLS Estimates Dropping One Politician at a Time*
************************************

use "Wang APSR Master Data_st.dta", clear

local politician "7	8 10 12	13	17	20	26	29	34	36	37	40	42	46	48	50	51	55	59	61	62	65	70	72	79	83	84	85	88	89	92	99	110	112	113	114	118	119	120"

local lassocontrols "dist_grandfather_father i.hometown_prefecture_id"

foreach p of local politician {
	qui xi: reg support_continuous kinproximity `lassocontrols' if politician_id!=`p', cl(hometown_prefecture_id)
	parmest, le(90 95) sa("nopolitician_`p'.dta", replace)
}

local politician "7	8 10 12	13	17	20	26	29	34	36	37	40	42	46	48	50	51	55	59	61	62	65	70	72	79	83	84	85	88	89	92	99	110	112	113	114	118	119	120"

foreach p of local politician {
	use "nopolitician_`p'.dta", clear
	gen N=_n
	keep if N==1
	gen droppolitician="`p'"
	keep parm estimate min90 max90 min95 max95 droppolitician
	save, replace
}

local politician "7	8 10 12	13	17	20	26	29	34	36	37	40	42	46	48	50	51	55	59	61	62	65	70	72	79	83	84	85	88	89	92	99	110	112	113	114	118	119	120"

use "nopolitician_7.dta", clear
foreach p of local politician {
	append using "nopolitician_`p'.dta"
	}
drop in 1
save "nopolitician.dta", replace

use "nopolitician.dta", clear

*Give each politician a consecutive number*
gen politician=1 if droppolitician=="7"
replace politician=2 if droppolitician=="8"
replace politician=3 if droppolitician=="10"
replace politician=4 if droppolitician=="12"
replace politician=5 if droppolitician=="13"
replace politician=6 if droppolitician=="17"
replace politician=7 if droppolitician=="20"
replace politician=8 if droppolitician=="26"
replace politician=9 if droppolitician=="29"
replace politician=10 if droppolitician=="34"
replace politician=11 if droppolitician=="36"
replace politician=12 if droppolitician=="37"
replace politician=13 if droppolitician=="40"
replace politician=14 if droppolitician=="42"
replace politician=15 if droppolitician=="46"
replace politician=16 if droppolitician=="48"
replace politician=17 if droppolitician=="50"
replace politician=18 if droppolitician=="51"
replace politician=19 if droppolitician=="55"
replace politician=20 if droppolitician=="59"
replace politician=21 if droppolitician=="61"
replace politician=22 if droppolitician=="62"
replace politician=23 if droppolitician=="65"
replace politician=24 if droppolitician=="70"
replace politician=25 if droppolitician=="72"
replace politician=26 if droppolitician=="79"
replace politician=27 if droppolitician=="83"
replace politician=28 if droppolitician=="84"
replace politician=29 if droppolitician=="85"
replace politician=30 if droppolitician=="88"
replace politician=31 if droppolitician=="89"
replace politician=32 if droppolitician=="92"
replace politician=33 if droppolitician=="99"
replace politician=34 if droppolitician=="110"
replace politician=35 if droppolitician=="112"
replace politician=36 if droppolitician=="113"
replace politician=37 if droppolitician=="114"
replace politician=38 if droppolitician=="118"
replace politician=39 if droppolitician=="119"
replace politician=40 if droppolitician=="120"

sort politician
save, replace

use "nopolitician.dta", clear

twoway (rcap min90 max90 politician, horizontal) ///
      (rcap min95 max95 politician, msize(tiny) horizontal) ///
	(scatter politician estimate,msize(small) msymbol(o) mcol(gs0)) ///
	, scheme(s1mono) ///
	legend(off) ///
	xscale(range(-1 .1)) xlabel(-1(.1) 0.1) xtitle("Standardized Coefficient") ///
	xline(0, lpattern(dash)) ///
	yscale(range(1 40)) yscale(reverse) ylabel(1(1)40, angle(0) valuelabel labsize(vsmall)) ytick(1(1)40) ytitle("Excluded Politician") 
graph export droppolitician.png, as(png) width(2500) replace
cap graph close graph

************************************
*Figure A1-11: OLS Estimates using Randomly Assigned Political Attitudes*
************************************

use "Wang APSR Master Data.dta", clear

*Standardize the data again, because the previous standardization was based on the estimating sample. This exercise uses observations outside the estimating sample*

global y "support_continuous"
global x "kinproximity dist_grandfather_father"

center $y $x, inplace standardize

label var	support_continuous	"Support for reform (continuous)"
label var	kinproximity	"Local concentration of kin"
label var dist_grandfather_father "Farther migration"

*run 100 trials*
*make sure to set the seed to obtain the same results every time*

set seed 339487731

forvalues i=1(1)100 { 
generate support_random`i' = uniform()<=0.5
replace support_random`i'=support_continuous if support_continuous~=.
}  

local lassocontrols "dist_grandfather_father i.hometown_prefecture_id"

forvalues i=1(1)100 { 
quietly xi:reg support_random`i' kinproximity `lassocontrols',cl(hometown_prefecture_id)
parmest, le(90 95) sa("supportrandom_`i'.dta", replace)
} 

local trialno "1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16	17	18	19	20	21	22	23	24	25	26	27	28	29	30	31	32	33	34	35	36	37	38	39	40	41	42	43	44	45	46	47	48	49	50	51	52	53	54	55	56	57	58	59	60	61	62	63	64	65	66	67	68	69	70	71	72	73	74	75	76	77	78	79	80	81	82	83	84	85	86	87	88	89	90	91	92	93	94	95	96	97	98	99	100"
foreach i of local trialno {
	use "supportrandom_`i'.dta", clear
	gen N=_n
	keep if N==1
	gen trialno="`i'"
	keep parm estimate min90 max90 min95 max95 trialno
	save, replace
}

local trialno "1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16	17	18	19	20	21	22	23	24	25	26	27	28	29	30	31	32	33	34	35	36	37	38	39	40	41	42	43	44	45	46	47	48	49	50	51	52	53	54	55	56	57	58	59	60	61	62	63	64	65	66	67	68	69	70	71	72	73	74	75	76	77	78	79	80	81	82	83	84	85	86	87	88	89	90	91	92	93	94	95	96	97	98	99	100"
use "supportrandom_1.dta", clear
foreach i of local trialno {
	append using "supportrandom_`i'.dta"
	}
drop in 1
save "supportrandom.dta", replace

use "supportrandom.dta", clear
encode trialno, gen(trial)
sort trial
save, replace

clear all

use "supportrandom.dta", clear

sort estimate
gen Trial=_n

gen null=(max90>=0)
tab null

twoway (rcap min90 max90 Trial, vertical) ///
      (rcap min95 max95 Trial, msize(tiny) vertical) ///
	(scatter estimate Trial,msize(small) msymbol(o) mcol(gs0)) ///
	, scheme(s1mono) ///
	legend(off) ///
	xscale(range(0 100)) xlabel(0(10)100) xtitle("Trial") ///
	yline(0, lpattern(dash)) ///
	yscale(range(-0.6 0.1)) ylabel(-0.6(0.1)0.1, angle(0) valuelabel) ytick(-0.6(0.1)0.1) ytitle("Standardized Coefficient") 
graph export random.png, as(png) width(2500) replace
cap graph close graph

************************************
*Table A1-8: Geography of Kinship Network and Support for Reform: OLS Estimates with Dichotomous Dependent Variable*
************************************

use "Wang APSR Master Data_st.dta", clear

local lassocontrols "dist_grandfather_father i.hometown_prefecture_id"

xi: reg support_dummy kinproximity,cl(hometown_prefecture_id)
estimates store dummymodel1
estadd ysumm

xi: reg support_dummy kinproximity `lassocontrols',cl(hometown_prefecture_id)
estimates store dummymodel2
estadd ysumm

esttab dummymodel1 dummymodel2 using dummymodel.tex, ///
	replace fragment keep(kinproximity dist_grandfather_father) ///
	b(3) se(3) posthead(\cmidrule(lr){2-3}) ///
	cells(b(star fmt(3)) se(fmt(3) par)) staraux star(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
	 label booktabs noconstant obslast nodepvars nomtitles nolines gaps ///
	scalars("r2 \(R^{2}\)" ) sfmt(3) ///
	stats(ymean ysd N r2, fmt(3 3 0 3) labels("Outcome mean" "Outcome std.dev." "Observations" "R2")) ///
	indicate("Prefecture FE = *Ihometown_*")

************************************
*Table A1-9: Geography of Kinship Network and Support for Reform: OLS Estimates with Trichotomous Dependent Variable*
************************************

use "Wang APSR Master Data.dta", clear

*standardize this dataset again, because the previous standardization was based on the estimating sample (40 obs)

global y "policy_opinion_tri"
global x "kinproximity"
global controls "dist_grandfather_father"

center $y $x $controls if policy_opinion_tri~=. & kinproximity~=., inplace standardize

label var policy_opinion_tri "Support for reform (trichotomous)"
label var	kinproximity	"Local concentration of kin"
label var	dist_grandfather_father	"Father migration"

local lassocontrols "dist_grandfather_father i.hometown_prefecture_id"
 
xi: reg policy_opinion_tri kinproximity,cl(hometown_prefecture_id)
estimates store trimodel1
estadd ysumm

xi: reg policy_opinion_tri kinproximity `lassocontrols',cl(hometown_prefecture_id)
estimates store trimodel2
estadd ysumm

esttab trimodel1 trimodel2 using trimodel.tex, ///
	replace fragment keep(kinproximity dist_grandfather_father) ///
	b(3) se(3) posthead(\cmidrule(lr){2-3}) ///
	cells(b(star fmt(3)) se(fmt(3) par)) staraux star(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
	 label booktabs noconstant obslast nodepvars nomtitles nolines gaps ///
	scalars("r2 \(R^{2}\)" ) sfmt(3) ///
	stats(ymean ysd N r2, fmt(3 3 0 3) labels("Outcome mean" "Outcome std.dev." "Observations" "R2")) ///
	indicate("Prefecture FE = *Ihometown_*")

************************************
*Table A1-10: Geography of Kinship Network and Support for Reform: OLS Estimates with Policy-Relevant Sample*
************************************

use "Wang APSR Master Data.dta", clear

*standardize this dataset again, because the previous standardization was based on the estimating sample of the main analysis (40 obs)

global y "support_continuous"
global x "kinproximity"
global controls "dist_grandfather_father"

center $y $x $controls if sample==1 & sector~=4, inplace standardize

label var support_continuous "Support for reform (continuous)"
label var	kinproximity	"Local concentration of kin"
label var	dist_grandfather_father	"Father migration"

local lassocontrols "dist_grandfather_father i.hometown_prefecture_id"

xi: reg support_continuous kinproximity if sector~=4,cl(hometown_prefecture_id)
estimates store policymakermodel1
estadd ysumm

xi: reg support_continuous kinproximity `lassocontrols' if sector~=4,cl(hometown_prefecture_id)
estimates store policymakermodel2
estadd ysumm

esttab policymakermodel1 policymakermodel2 using policymakermodel.tex, ///
	replace fragment keep(kinproximity dist_grandfather_father) ///
	b(3) se(3) posthead(\cmidrule(lr){2-3}) ///
	cells(b(star fmt(3)) se(fmt(3) par)) staraux star(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
	 label booktabs noconstant obslast nodepvars nomtitles nolines gaps ///
	scalars("r2 \(R^{2}\)" ) sfmt(3) ///
	stats(ymean ysd N r2, fmt(3 3 0 3) labels("Outcome mean" "Outcome std.dev." "Observations" "R2")) ///
	indicate("Prefecture FE = *Ihometown_*") 

************************************
*Table A1-11: Geography of Kinship Network and Support for Reform: OLS Estimates with Politician’s Own Marriage Network*
************************************

use "Wang APSR Master Data.dta", clear

*standardize this dataset again, because the previous standardization was based on the estimating sample of the main analysis (40 obs)

global y "support_continuous"
global x "kinproximity_politician"
global controls "dist_grandfather_father"

center $y $x $controls if sample==1 & kinproximity_politician~=., inplace standardize

label var support_continuous "Support for reform (continuous)"
label var	kinproximity_politician	"Local concentration of kin (politician)"
label var	dist_grandfather_father	"Father migration"

local lassocontrols "dist_grandfather_father i.hometown_prefecture_id"

qui xi: reg support_continuous kinproximity_politician if sample==1,cl(hometown_prefecture_id)
estimates store politicianmarriagemodel1
estadd ysumm

qui xi: reg support_continuous kinproximity_politician `lassocontrols' if sample==1,cl(hometown_prefecture_id)
estimates store politicianmarriagemodel2
estadd ysumm

esttab politicianmarriagemodel1 politicianmarriagemodel2 using politicianmarriagemodel.tex, ///
	replace fragment keep(kinproximity_politician dist_grandfather_father) ///
	b(3) se(3) posthead(\cmidrule(lr){2-3}) ///
	cells(b(star fmt(3)) se(fmt(3) par)) staraux star(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
	 label booktabs noconstant obslast nodepvars nomtitles nolines gaps ///
	scalars("r2 \(R^{2}\)" ) sfmt(3) ///
	stats(ymean ysd N r2, fmt(3 3 0 3) labels("Outcome mean" "Outcome std.dev." "Observations" "R2")) ///
	indicate("Prefecture FE = *Ihometown_*")

************************************
*Table A1-12: Geography of Kinship Network and Support for Reform: IV Estimates with Politician’s Own Marriage Network*
************************************

use "Wang APSR Master Data.dta", clear

*standardize this dataset again, because the previous standardization was based on the estimating sample of the main analysis (40 obs)

global y "support_continuous"
global x "kinproximity_politician kinproximity"
global controls "dist_grandfather_father"

center $y $x $controls if sample==1 & kinproximity_politician~=., inplace standardize

label var support_continuous "Support for reform (continuous)"
label var	kinproximity_politician	"Local concentration of kin (politician)"
label var	kinproximity	"Local concentration of kin"
label var	dist_grandfather_father	"Father migration"

*2SLS*

local lassocontrols "dist_grandfather_father i.hometown_prefecture_id"

qui xi: ivregress 2sls support_continuous (kinproximity=kinproximity_politician) if sample==1,cl(hometown_prefecture_id)
estimates store politicianmarriageIVmodel1
estadd ysumm

qui xi: ivregress 2sls support_continuous (kinproximity=kinproximity_politician) `lassocontrols' if sample==1,cl(hometown_prefecture_id)
estimates store politicianmarriageIVmodel2
estadd ysumm

esttab politicianmarriageIVmodel1 politicianmarriageIVmodel2 using politicianmarriageIVmodel.tex, ///
	replace fragment keep(kinproximity dist_grandfather_father) ///
	b(3) se(3) posthead(\cmidrule(lr){2-3}) ///
	cells(b(star fmt(3)) se(fmt(3) par)) staraux star(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
	 label booktabs noconstant obslast nodepvars nomtitles nolines gaps ///
	scalars("r2 \(R^{2}\)" ) sfmt(3) ///
	stats(ymean ysd N r2, fmt(3 3 0 3) labels("Outcome mean" "Outcome std.dev." "Observations" "R2")) ///
	indicate("Prefecture FE = *Ihometown_*")

*First Stage (with F-stats)*

local lassocontrols "dist_grandfather_father i.hometown_prefecture_id"

xi: reg	kinproximity kinproximity_politician if sample==1,cl(hometown_prefecture_id)
estadd ysumm
*F-stat*
test kinproximity_politician=0
estimates store IVmodel_first1

 * F(  1,    24) =   68.77
            *Prob > F =    0.0000

			
xi: reg	kinproximity kinproximity_politician `lassocontrols' if sample==1,cl(hometown_prefecture_id)
estadd ysumm
*F-stat*
test kinproximity_politician=0
estimates store IVmodel_first2	
	
   *F(  1,    24) = 2855.86
            *Prob > F =    0.0000

esttab IVmodel_first1 IVmodel_first2 using IVmodel_first.tex, ///
	replace fragment keep(kinproximity_politician dist_grandfather_father) ///
	b(3) se(3) posthead(\cmidrule(lr){2-3}) ///
	cells(b(star fmt(3)) se(fmt(3) par)) staraux star(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
	 label booktabs noconstant obslast nodepvars nomtitles nolines gaps ///
	scalars("r2 \(R^{2}\)" ) sfmt(3) ///
	stats(ymean ysd N r2, fmt(3 3 0 3) labels("Outcome mean" "Outcome std.dev." "Observations" "R2")) ///
	indicate("Prefecture FE = *Ihometown_*")
	
************************************
*Table A1-13: Geography of Kinship Network and Support for Reform: OLS Estimates with Transformed Independent Variables*
************************************

use "Wang APSR Master Data_st.dta", clear

*IHS*
gen kinproximity_IHS=ln(kinproximity + sqrt(kinproximity^2 + 1))
label var kinproximity_IHS "Local concentration of kin (IHS)"

*Square root*
gen kinproximity_sqt=(kinproximity + 1)^(1/2)
label var kinproximity_sqt "Local concentration of kin (square root)"


local lassocontrols "dist_grandfather_father i.hometown_prefecture_id"

xi: reg support_continuous kinproximity_IHS,cl(hometown_prefecture_id)
estimates store transmodel1
estadd ysumm

xi: reg support_continuous kinproximity_IHS `lassocontrols',cl(hometown_prefecture_id)
estimates store transmodel2
estadd ysumm

xi: reg support_continuous kinproximity_sqt,cl(hometown_prefecture_id)
estimates store transmodel3
estadd ysumm

xi: reg support_continuous kinproximity_sqt `lassocontrols',cl(hometown_prefecture_id)
estimates store transmodel4
estadd ysumm

esttab transmodel1 transmodel2 transmodel3 transmodel4 using transmodel.tex, ///
	replace fragment keep(kinproximity_IHS kinproximity_sqt dist_grandfather_father) ///
	order(kinproximity_IHS kinproximity_sqt dist_grandfather_father) ///
	b(3) se(3) posthead(\cmidrule(lr){2-5}) ///
	cells(b(star fmt(3)) se(fmt(3) par)) staraux star(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
	 label booktabs noconstant obslast nodepvars nomtitles nolines gaps ///
	scalars("r2 \(R^{2}\)" ) sfmt(3) ///
	stats(ymean ysd N r2, fmt(3 3 0 3) labels("Outcome mean" "Outcome std.dev." "Observations" "R2")) ///
	indicate("Prefecture FE = *Ihometown_*")

************************************
*Table A1-14: Geography of Kinship Network and Support for Reform: OLS Estimates with Local Concentration of Kin Weighted by Number of Children*
************************************

use "Wang APSR Master Data_st.dta", clear

local lassocontrols "dist_grandfather_father i.hometown_prefecture_id"

qui xi: reg support_continuous kinproximity_children,cl(hometown_prefecture_id)
estimates store normalizedmodel1
estadd ysumm

qui xi: reg support_continuous kinproximity_children `lassocontrols',cl(hometown_prefecture_id)
estimates store normalizedmodel2
estadd ysumm

esttab normalizedmodel1 normalizedmodel2 using normalizedmodel.tex, ///
	replace fragment keep(kinproximity_children dist_grandfather_father) ///
	b(3) se(3) posthead(\cmidrule(lr){2-3}) ///
	cells(b(star fmt(3)) se(fmt(3) par)) staraux star(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
	 label booktabs noconstant obslast nodepvars nomtitles nolines gaps ///
	scalars("r2 \(R^{2}\)" ) sfmt(3) ///
	stats(ymean ysd N r2, fmt(3 3 0 3) labels("Outcome mean" "Outcome std.dev." "Observations" "R2")) ///
	indicate("Prefecture FE = *Ihometown_*")

************************************
*Table A1-15: Geography of Kinship Network and Support for Reform: OLS Estimates with Relational Distance Weighted Independent Variable*
************************************

use "Wang APSR Master Data_st.dta", clear

local lassocontrols "dist_grandfather_father i.hometown_prefecture_id"

qui xi: reg support_continuous kinproximity_wd,cl(hometown_prefecture_id)
estimates store dweightedmodel1
estadd ysumm

qui xi: reg support_continuous kinproximity_wd `lassocontrols',cl(hometown_prefecture_id)
estimates store dweightedmodel2
estadd ysumm

esttab dweightedmodel1 dweightedmodel2 using dweightedmodel.tex, ///
	replace fragment keep(kinproximity_wd dist_grandfather_father) ///
	b(3) se(3) posthead(\cmidrule(lr){2-3}) ///
	cells(b(star fmt(3)) se(fmt(3) par)) staraux star(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
	 label booktabs noconstant obslast nodepvars nomtitles nolines gaps ///
	scalars("r2 \(R^{2}\)" ) sfmt(3) ///
	stats(ymean ysd N r2, fmt(3 3 0 3) labels("Outcome mean" "Outcome std.dev." "Observations" "R2")) ///
	indicate("Prefecture FE = *Ihometown_*")

************************************
*Table A1-16: Geography of Kinship Network and Support for Reform: OLS Estimates with Herfindahl Index of Kin Concentration*
************************************

use "Wang APSR Master Data_st.dta", clear

local lassocontrols "dist_grandfather_father i.hometown_prefecture_id"

qui xi: reg support_continuous kin_county_w kin_node, cl(hometown_prefecture_id)
estimates store hhicountymodel1
estadd ysumm

qui xi: reg support_continuous kin_county_w kin_node `lassocontrols', cl(hometown_prefecture_id)
estimates store hhicountymodel2
estadd ysumm

qui xi: reg support_continuous kin_prefecture_w kin_node, cl(hometown_prefecture_id)
estimates store hhiprefmodel1
estadd ysumm

qui xi: reg support_continuous kin_prefecture_w kin_node `lassocontrols', cl(hometown_prefecture_id)
estimates store hhiprefmodel2
estadd ysumm

qui xi: reg support_continuous kin_province_w kin_node, cl(hometown_prefecture_id)
estimates store hhiprovmodel1
estadd ysumm

qui xi: reg support_continuous kin_province_w kin_node `lassocontrols', cl(hometown_prefecture_id)
estimates store hhiprovmodel2
estadd ysumm

esttab hhicountymodel1 hhicountymodel2 hhiprefmodel1 hhiprefmodel2 hhiprovmodel1 hhiprovmodel2 using herfindahlmodel.tex, ///
	replace fragment keep(kin_county_w kin_prefecture_w kin_province_w kin_node dist_grandfather_father) ///
	order(kin_county_w kin_prefecture_w kin_province_w dist_grandfather_father kin_node dist_grandfather_father) ///
	b(3) se(3) posthead(\cmidrule(lr){2-7}) ///
	cells(b(star fmt(3)) se(fmt(3) par)) staraux star(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
	label booktabs noconstant obslast nodepvars nomtitles nolines gaps ///
	scalars("r2 \(R^{2}\)") sfmt(3) ///
	stats(ymean ysd N r2, fmt(3 3 0 3) labels("Outcome mean" "Outcome std.dev." "Observations" "R2")) ///
	indicate("Prefecture FE = *Ihometown_*") 

************************************
*Table A1-17: Geography of Kinship Network and Support for Reform: OLS Estimates with Alternative Centrality Measures*
************************************

use "Wang APSR Master Data_st.dta", clear

local controls1 "degree"
local controls2 "bonacich"
local lassocontrols "dist_grandfather_father i.hometown_prefecture_id"

xi: reg support_continuous kinproximity `controls1',cl(hometown_prefecture_id)
estimates store degreemodel1
estadd ysumm

xi: reg support_continuous kinproximity `controls1' `lassocontrols',cl(hometown_prefecture_id)
estimates store degreemodel2
estadd ysumm

xi: reg support_continuous kinproximity `controls2',cl(hometown_prefecture_id)
estimates store bonacichmodel1
estadd ysumm

xi: reg support_continuous kinproximity `controls2' `lassocontrols',cl(hometown_prefecture_id)
estimates store bonacichmodel2
estadd ysumm

esttab degreemodel1 degreemodel2 bonacichmodel1 bonacichmodel2 using centralitymodel.tex, ///
	replace fragment keep(kinproximity degree bonacich dist_grandfather_father) ///
	order(kinproximity degree bonacich dist_grandfather_father) ///
	b(3) se(3) posthead(\cmidrule(lr){2-5}) ///
	cells(b(star fmt(3)) se(fmt(3) par)) staraux star(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
	 label booktabs noconstant obslast nodepvars nomtitles nolines gaps ///
	scalars("r2 \(R^{2}\)" ) sfmt(3) ///
	stats(ymean ysd N r2, fmt(3 3 0 3) labels("Outcome mean" "Outcome std.dev." "Observations" "R2")) ///
	indicate("Prefecture FE = *Ihometown_*") 

************************************
*Table A1-18: Geography of Kinship Network and Support for Reform: OLS Estimates Controlling for Number of Children Flexibly*
************************************

use "Wang APSR Master Data_st.dta", clear

local lassocontrols "dist_grandfather_father i.hometown_prefecture_id"

xi: reg support_continuous kinproximity i.children_group,cl(hometown_prefecture_id)
estimates store childrenmodel1
estadd ysumm

xi: reg support_continuous kinproximity i.children_group `lassocontrols',cl(hometown_prefecture_id)
estimates store childrenmodel2
estadd ysumm

esttab childrenmodel1 childrenmodel2 using childrenmodel.tex, ///
	replace fragment keep(kinproximity _Ichildren__2 _Ichildren__3 dist_grandfather_father) ///
	order(kinproximity _Ichildren__2 _Ichildren__3 dist_grandfather_father) ///
	b(3) se(3) posthead(\cmidrule(lr){2-3}) ///
	cells(b(star fmt(3)) se(fmt(3) par)) staraux star(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
	 label booktabs noconstant obslast nodepvars nomtitles nolines gaps ///
	scalars("r2 \(R^{2}\)" ) sfmt(3) ///
	stats(ymean ysd N r2, fmt(3 3 0 3) labels("Outcome mean" "Outcome std.dev." "Observations" "R2")) ///
	indicate("Prefecture FE = *Ihometown_*")

************************************
*Table A1-19: Geography of Kinship Network and Support for Reform: OLS Estimates Controlling for Politician's Highest or First Rank*
************************************	

use "Wang APSR Master Data_st.dta", clear

local lassocontrols "dist_grandfather_father i.hometown_prefecture_id"

xi: reg support_continuous kinproximity rank_max,cl(hometown_prefecture_id)
estimates store rankmodel1
estadd ysumm

xi: reg support_continuous kinproximity rank_max `lassocontrols',cl(hometown_prefecture_id)
estimates store rankmodel2
estadd ysumm

xi: reg support_continuous kinproximity rank_first,cl(hometown_prefecture_id)
estimates store rankmodel3
estadd ysumm

xi: reg support_continuous kinproximity rank_first `lassocontrols',cl(hometown_prefecture_id)
estimates store rankmodel4
estadd ysumm

esttab rankmodel1 rankmodel2 rankmodel3 rankmodel4 using rankmodel.tex, ///
	replace fragment keep(kinproximity rank_max rank_first dist_grandfather_father) ///
	order(kinproximity rank_max rank_first dist_grandfather_father) ///
	b(3) se(3) posthead(\cmidrule(lr){2-5}) ///
	cells(b(star fmt(3)) se(fmt(3) par)) staraux star(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
	 label booktabs noconstant obslast nodepvars nomtitles nolines gaps ///
	scalars("r2 \(R^{2}\)" ) sfmt(3) ///
	stats(ymean ysd N r2, fmt(3 3 0 3) labels("Outcome mean" "Outcome std.dev." "Observations" "R2")) ///
	indicate("Prefecture FE = *Ihometown_*")
	
************************************
*Table A1-20: Geography of Kinship Network and Support for Reform: OLS Estimates with Imputed Datasets*
************************************

use "Wang APSR Master Data.dta", clear

*preserve the data and restore later to not to mess it up

preserve 

mi set mlong

mi register imputed kinproximity policy_opinion_tri betweenness kin_node children supportfaction rank_ave kinmedianexposuretoexternal50 kinmedianexposuretointernal50  avg_ruggedness father_exam dist_grandfather_father hometown_prefecture_id

mi misstable summarize kinproximity policy_opinion_tri betweenness kin_node children supportfaction rank_ave kinmedianexposuretoexternal50 kinmedianexposuretointernal50  avg_ruggedness father_exam dist_grandfather_father hometown_prefecture_id, all

*make sure to include a seed to obtain the same results every time*

mi impute mvn kinproximity policy_opinion_tri betweenness kin_node children supportfaction rank_ave kinmedianexposuretoexternal50 kinmedianexposuretointernal50  avg_ruggedness father_exam dist_grandfather_father hometown_prefecture_id,add(20) rseed(1115)

xi: eststo: mi estimate, post:reg policy_opinion_tri kinproximity, cl(hometown_prefecture_id)
estimates store mimodel

esttab mimodel  using mimodel.tex, ///
	replace fragment keep(kinproximity) ///
	b(3) se(3) posthead(\cmidrule(lr){2}) ///
	cells(b(star fmt(3)) se(fmt(3) par)) staraux star(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
	 label booktabs noconstant obslast nodepvars nomtitles nolines gaps ///
	scalars("r2 \(R^{2}\)" ) sfmt(3) ///
	stats(N, fmt(0) labels("Observations"))

restore

************************************
*Table A1-21: Geography of Kinship Network and Support for Reform: OLS Estimates with Province Fixed Effects*
************************************

use "Wang APSR Master Data_st.dta", clear

local lassocontrols "dist_grandfather_father i.hometownprovince_id"

xi: reg support_continuous kinproximity,cl(hometownprovince_id)
estimates store provmodel1
estadd ysumm

xi: reg support_continuous kinproximity `lassocontrols',cl(hometownprovince_id)
estimates store provmodel2
estadd ysumm

esttab provmodel1 provmodel2 using provmodel.tex, ///
	replace fragment keep(kinproximity dist_grandfather_father) ///
	b(3) se(3) posthead(\cmidrule(lr){2-3}) ///
	cells(b(star fmt(3)) se(fmt(3) par)) staraux star(* 0.10 ** 0.05 *** 0.01) collabels(none) ///
	 label booktabs noconstant obslast nodepvars nomtitles nolines gaps ///
	scalars("r2 \(R^{2}\)" ) sfmt(3) ///
	stats(ymean ysd N r2, fmt(3 3 0 3) labels("Outcome mean" "Outcome std.dev." "Observations" "R2")) ///
	indicate("Provincial FE = *Ihometownp_*")
	
************************************
*Table A1-22: Using Selection on Observables to Assess the Bias from Unobservables*
************************************

use "Wang APSR Master Data_st.dta", clear

*shorten variable names for poet*
gen kinexpexter=kinmedianexposuretoexternal50
gen kinexpinter=kinmedianexposuretointernal50
tabulate hometown_prefecture_id, generate(hometown_p)

*Combination 1: all covariates including prefecture fixed effects*
poet support_continuous betweenness kin_node children supportfaction rank_ave kinexpexter kinexpinter  avg_ruggedness father_exam dist_grandfather_father hometown_p2 hometown_p2 hometown_p3 hometown_p4 hometown_p5 hometown_p6 hometown_p7 hometown_p8 hometown_p9 hometown_p10 hometown_p11 hometown_p12 hometown_p13 hometown_p14 hometown_p15 hometown_p16 hometown_p17 hometown_p18 hometown_p19 hometown_p20 hometown_p21 hometown_p22 hometown_p23 hometown_p24 hometown_p25 hometown_p26 hometown_p27 hometown_p28 hometown_p29 hometown_p30 hometown_p31 hometown_p32 hometown_p33 hometown_p34 hometown_p35 hometown_p36 hometown_p37 hometown_p38 hometown_p39 hometown_p40 hometown_p41 hometown_p42 hometown_p43 hometown_p44 hometown_p45 hometown_p46 hometown_p47 hometown_p48 hometown_p49 hometown_p50 hometown_p51 hometown_p52 hometown_p53 hometown_p54 hometown_p55 hometown_p56 hometown_p57 hometown_p58 hometown_p59 hometown_p60 hometown_p61 hometown_p62, treat(kinproximity)

*AET Ratio=13.6497
				
*Combination 2: only prefecture fixed effects*
poet support_continuous hometown_p2 hometown_p2 hometown_p3 hometown_p4 hometown_p5 hometown_p6 hometown_p7 hometown_p8 hometown_p9 hometown_p10 hometown_p11 hometown_p12 hometown_p13 hometown_p14 hometown_p15 hometown_p16 hometown_p17 hometown_p18 hometown_p19 hometown_p20 hometown_p21 hometown_p22 hometown_p23 hometown_p24 hometown_p25 hometown_p26 hometown_p27 hometown_p28 hometown_p29 hometown_p30 hometown_p31 hometown_p32 hometown_p33 hometown_p34 hometown_p35 hometown_p36 hometown_p37 hometown_p38 hometown_p39 hometown_p40 hometown_p41 hometown_p42 hometown_p43 hometown_p44 hometown_p45 hometown_p46 hometown_p47 hometown_p48 hometown_p49 hometown_p50 hometown_p51 hometown_p52 hometown_p53 hometown_p54 hometown_p55 hometown_p56 hometown_p57 hometown_p58 hometown_p59 hometown_p60 hometown_p61 hometown_p62, treat(kinproximity)

*AET Ratio=15.3284

*The End*
