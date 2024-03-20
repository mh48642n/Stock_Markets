//clears out memory before beginning
clear

//sets working directory
cd "C:\Users\marvi\OneDrive\Documents\GitHub\Stock_Markets"

//imports data into stata
//import excel standard_tzs.xlsx, firstrow clear
//import excel monthly_dataset.xlsx, firstrow clear
import excel dataset_final.xlsx, firstrow clear

//declaring data to be time series
//for monthly_dataset
gen time = tm(1999m6) + _n - 1
format time %tm
tsset time

//for weekly_dataset
//generate time = tw(2001w1) + _n - 1
//format time %tw
//tsset time

//for daily_dataset
//generate time = td(01jan2000) + _n - 1
//format time %td
//tsset time

//ordering datasets observations 
order time

//labelling variables
label variable sp_500 "S&P 500 Index Values"
label variable russell_2000 "Russell 2000 Index Values"
label variable eff_fund_rates "Federal Funds Rate"
label variable futures_10year "10 Year Treasury Note Futures"
label variable t_note_10yr "10 Year Note Yield Rates"
label variable Aaa "Aaa Corporate bond rate"
label variable Baa "Baa Corporate bond rate"
label variable euro_us_ex "Euro/USD Exchange Rate"
label variable public_debt "Public Debt"
label variable public_held "Public Held Debt"
label variable govt_held "Government Held Debt"
label variable M2 "M2 Monetary Supply"
label variable budget "US Monthly Budget Surplus or Deficit"
label variable inflation_expectations_1yr "1 year Inflation Expectations"
label variable inflation_expectations_10 "10 year Inflation Expectations"
label variable real_gdp "S&P Forecasted Monthly GDP Index"
label variable raised "Debt Ceiling was Raised"
label variable suspend "Debt Ceiling was Suspended"
label variable meeting "FOMC meeting this month"
label variable auction_day "Auction this month for 10 year Note"

//adding new variables for later regressions
//gen bg_ratio = (budget/real_gdp) * 100 
//gen dg_ratio = (public_debt/real_gdp) * 100

//label variable bg_ratio "Budget to GDP ratio"
//label variable dg_ratio "Debt to GDP ratio"


/*Viewing Data*/
describe
summarize
cor


/*MULTIVARIATE VECTOR AUTO-REGRESSION*/
	//test if data is stationary
	dfuller d.russell_2000
	dfuller d.sp_500
	dfuller d.futures_10year
	dfuller d.t_note_10yr
	
	//test for normality
	varnorm
	
	//Lag length checker
	varsoc t_note_10yr russell_2000 Aaa Baa euro_us_ex
	varsoc t_note_10yr sp_500 Aaa Baa euro_us_ex
	
	
	//Index Price: Russell 2000 
	//No autocorrelation in the residuals at the 95% range and the var is stable 
	var D.(t_note_10yr russell_2000 Aaa Baa euro_us_ex) if time >= tm(1999m7), lags(1/3)
    var t_note_10yr russell_2000 Aaa Baa euro_us_ex if time >= tm(1999m7), lags(1/3)
	varlmar
	varstable	
	vargranger
	
	//Index Price: S&P 500
	//No autocorrelation in the residuals at the 95% range and the var is stable 
	var D.(t_note_10yr sp_500 Aaa Baa euro_us_ex) if time >= tm(1999m7), lags(1/3)
	var t_note_10yr sp_500 Aaa Baa euro_us_ex if time >= tm(1999m7), lags(1/3)
	varlmar
	varstable	
	vargranger
	
/*Dynamic Regression Models*/
	*first differenced of estimates
	quietly reg D.(futures_10year L.futures_10year)
	local r2_a = e(r2_a)
	eststo model_target_d: quietly newey D.(futures_10year L.futures_10year), lag(1)
	estadd scalar r2_a = `r2_a'
	 
	qui reg D.(futures_10year L.futures_10year eff_fund_rates inflation_expectations_10 euro_us_ex)
	local r2_a = e(r2_a)
	eststo full_model_d: quietly newey D.(futures_10year L.futures_10year eff_fund_rates inflation_expectations_10 euro_us_ex), lag(1)
	estadd scalar r2_a = `r2_a'
	
	qui reg D.(t_note_10yr L.futures_10year)
	local r2_a = e(r2_a)
	eststo model_td: quietly newey D.(t_note_10yr L.futures_10year), lag(1)
	estadd scalar r2_a = `r2_a'
	
	qui reg D.(t_note_10yr L.futures_10year eff_fund_rates inflation_expectations_1yr inflation_expectations_10 public_debt budget)
	local r2_a = e(r2_a)
	eststo full_model_td: quietly newey D.(t_note_10yr L.futures_10year eff_fund_rates inflation_expectations_1yr inflation_expectations_10 public_debt budget), lag(1)
	estadd scalar r2_a = `r2_a'
	
		
	*second set of estimates
	quietly reg futures_10year L.futures_10year
	local r2_a = e(r2_a)
	eststo model_target: quietly newey futures_10year L.futures_10year, lag(1)
	estadd scalar r2_a = `r2_a'
	 	 
	qui reg futures_10year L.futures_10year eff_fund_rates inflation_expectations_10 euro_us_ex
	local r2_a = e(r2_a)
	eststo full_model: quietly newey futures_10year L.futures_10year eff_fund_rates inflation_expectations_10 euro_us_ex, lag(1)
	estadd scalar r2_a = `r2_a'

	qui reg t_note_10yr L.futures_10year
	local r2_a = e(r2_a)
	eststo model_t: quietly newey t_note_10yr L.futures_10year , lag(1)
	estadd scalar r2_a = `r2_a'
	
	qui reg t_note_10yr L.futures_10year eff_fund_rates inflation_expectations_1yr inflation_expectations_10 public_debt budget
	local r2_a = e(r2_a)
	eststo full_model_t: quietly newey t_note_10yr L.futures_10year eff_fund_rates inflation_expectations_1yr inflation_expectations_10 public_debt budget, lag(1)
	estadd scalar r2_a = `r2_a'
	
	*1 and 2 has dependent variable for futures 
	*3 and 4 has dependent variable for treasury
	
	*regressions for differenced regressions
	esttab model_target_d full_model_d model_td full_model_td using differenced_reg.html, stats(r2_a, labels(Adj. R-squared)) nodepvars label title("Table 3: Differenced Regressions") nomtitles varwidth(45) modelwidth(65) 
	
	*regressions for level regressions
	esttab model_target full_model model_t full_model_t using level_reg.html, stats(r2_a, labels(Adjusted R-squared)) nodepvars label title("Table 2: Leveled Regressions") nomtitles varwidth(45) modelwidth(65) 


/*White Test*/
	estat bgod
	
/*Structural Vector Auto-Regression*/
    /*Short Term*/
	
	//Public Debt on Rates
	//Models the contemporaneous effects across three independent variables
//	matrix A = (1, 0, 0\., 1, 0\., ., 1)
//	matrix B = (., 0, 0\0,. , 0\0, 0, .)
	

		/*AB models*/
//		svar D.(Public_Debt ten_year_futures avg_Baa), aeq(A) beq(B) 
//		svar D.(avg_Baa avg_10yr avg_russell), aeq(A) beq(B) 
//		svar D.(avg_10yr avg_Baa avg_russell), aeq(A) beq(B)
        
		/*Counterfactual*/
//		svar D.(avg_funds avg_10yr avg_Baa), aeq(A) beq(B)

	//Public Debt on Financial Markets with 6 x 6 matrix
	matrix A = (1, 0, 0, 0, 0, 0\., 1, 0, 0, 0, 0\., ., 1, 0, 0, 0\., ., ., 1, 0, 0\., ., ., ., 1, 0\., ., ., ., ., 1)
							   
	matrix B = (., 0, 0, 0, 0, 0\0, ., 0, 0, 0, 0\0, 0, ., 0, 0, 0\0, 0, 0, ., 0, 0\0, 0, 0, 0, ., 0\0, 0, 0, 0, 0, .)
	
	
		/*AB Models*/
//		svar Public_Debt avg_10yr avg_Baa avg_russell, aeq(A) beq(B)
		svar D.(budget public_debt t_note_10yr futures_10year Aaa russell_2000), aeq(A) beq(B)
		irf set "impr.irf"
	    irf create test0, step(8)
		irf graph oirf, impulse(d.budget d.public_debt d.t_note_10yr d.futures_10year d.Aaa d.russell_2000) response(d.russell_2000)	

		svar budget public_debt t_note_10yr futures_10year Aaa russell_2000, aeq(A) beq(B)
		irf set "impr.irf"
	    irf create test1, step(8)
		irf graph oirf, impulse(budget public_debt t_note_10yr futures_10year Aaa russell_2000) response(russell_2000)
		
		/*Another angle*/
//		svar Public_Debt ten_year_futures avg_funds avg_russell,aeq(A) beq(B)
		svar D.(budget public_debt t_note_10yr futures_10year Aaa sp_500), aeq(A) beq(B)
		irf set "impr1.irf"
		irf create test1, step(8)
		irf graph oirf, impulse(D.Public_Debt D.ten_year_futures D.avg_funds D.avg_russell) response(D.avg_russell)

		svar budget public_debt t_note_10yr futures_10year Aaa sp_500, aeq(A) beq(B)
		irf set "impr1.irf"
		irf create test1, step(8)
		irf graph oirf, impulse(budget public_debt t_note_10yr futures_10year Aaa sp_500) response(sp_500)








