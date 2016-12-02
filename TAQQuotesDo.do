*============================================================
*============================================================
*============================================================
* quote data
*============================================================
*============================================================
*============================================================
clear
insheet using "~/Documents/quotes.csv"


*============================================================
* fixing time stamp
*============================================================

rename time_m t
gen tl=length(t)
gen tst1=substr(t,1,11) if tl==17
replace tst1=substr(t,1,12) if tl==18
gen h=substr(t,1,1) if tl==17
gen m=substr(t,3,2) if tl==17
gen s=substr(t,6,2) if tl==17
gen ms=substr(t,9,3) if tl==17

replace h=substr(t,1,2) if tl==18
replace m=substr(t,4,2) if tl==18
replace s=substr(t,8,2) if tl==18
replace ms=substr(t,10,3) if tl==18

destring h m s ms, replace
gen double time=hms(h,m,s)
format time %tCHH.MM.SS.sss
replace time=time+ms


drop  t tst1 h m s ms tl  qu_* 
order date time
rename sym_root ticker

drop if time>tc(16:00:00.000)
drop if time<tc(9:30:00.000)



*========================================================
* computing the NBBO
*========================================================

global venues "B C J K M P Q V X Y Z"

* STATA's precision is an issue
	foreach ww in bid ask bidsiz asksiz { 
		gen double `ww'2=round(`ww'*100)/100
		drop `ww'
		gen double `ww'=`ww'2
		drop `ww'2
		}
		

foreach vv of global venues {
	foreach ww in bid ask bidsiz asksiz { 
		gen double `ww'_`vv'=round(`ww'*100)/100 if ex=="`vv'"
		}
	}
foreach vv of global venues {
	foreach ww in bid ask bidsiz asksiz { 
		replace `ww'_`vv'=`ww'_`vv'[_n-1] if  `ww'_`vv'==.

		}
	replace ask_`vv'=. if ask_`vv'==0	
	}

gen double bidnbbo=max(bid_B,bid_C ,bid_J ,bid_K ,bid_M ,bid_P ,bid_Q ,bid_V ,bid_X ,bid_Y ,bid_Z)
gen double asknbbo=min(ask_B,ask_C ,ask_J ,ask_K ,ask_M ,ask_P ,ask_Q ,ask_V ,ask_X ,ask_Y ,ask_Z)


gen double bidsizenbbo=0
gen double asksizenbbo=0
foreach vv of global venues {
	foreach ww in  bid ask { 
		replace `ww'sizenbbo=`ww'sizenbbo+`ww'siz_`vv' if `ww'_`vv'==`ww'nbbo
		}
	}

drop ask_* bid_* asksiz_* bidsiz_* 
drop ex ask bid asksiz bidsiz

gen double mp=round(askn*100+bidn*100)/200
gen double qs=round(askn*1000-bidn*1000)/10
gen double qsp=qs/mp*100
gen double depth=asksizenbbo+bidsizenbbo


save temp, replace

*==========================================================
* first stop: computing the time-weighted spread per "day"
*==========================================================

gen double et=time[_n+1]-time
replace et=0 if et==.
foreach vv in qs qsp	 mp depth {
	replace `vv'=`vv'*et
	}

collapse (sum) et qs qsp mp depth , by(ticker date)

foreach vv in qs qsp mp depth {
	replace `vv'=`vv'/et
	}

save qs_per_day_MSFT, replace

*==========================================================================
* second stop: computing the per millisecond-NBBO for matching with trades
*==========================================================================
use temp, clear

* the problem: have potentially many quotes per millisecond; many possible approaches; I use the plain average for the midpoint
collapse mp, by(time)

gen double t2=tim
format t2 %tCHH.MM.SS.sss

save qs_per_ms_MSFT, replace
