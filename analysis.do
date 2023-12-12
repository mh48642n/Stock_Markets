//clears out memory before beginning
clear

//sets working directory
cd "C:\Users\marvi\OneDrive\Documents\GitHub\Stock_Markets"

//imports data into stata
import excel standard_tzs.xlsx, firstrow clear
import excel monthly_dataset.xlsx, firstrow clear
import excel weekly_dataset.xlsx, firstrow clear

//declaring data to be time series
//for monthly_dataset
generate time = tm(2001m1) + _n - 1
format time %tm
tsset time

//for weekly_dataset
generate time = tw(2001w1) + _n - 1
format time %tw
tsset time

//for daily_dataset
generate time = td(01jan2000) + _n - 1
format time %td
tsset time

//dropping observations 
drop if missing(sp_500)
drop dates
order time

//labelling variables
label variable avg_Baa "Baa Yields"
label variable avg_10yr "Treasury Yields"
label variable avg_funds "Federal Funds Rate"
label variable avg_future "FFR Futures"
label variable avg_russell "Russell Index"
label variable avg_standards "S&P 500 Index"
label variable avg_rv "Russell 2000 Trading Volume"
label variable avg_sp "S&P 500 Trading Volume"
label variable Public_Held "Public Held Debt"
label variable Govt_Held "Government Held Debt"
label variable ten_year_futures "10-yr Futures"
label variable futures_vol "10-yr Futures Trading Volume"


/*BIVARIATE VECTOR AUTO-REGRESSION*/
	//test if data is stationary
	dfuller s_russellp, trend
	dfuller s_sp500_p, trend
	
	//Index Prices: Russell 2000 and S&P 500 
	//No autocorrelation in the residuals at the 95% range and the var is stable 
	var D.(avg_10yr avg_Baa avg_russell)
	estimates store var1
	var D.(avg_funds avg_10yr avg_russell)
	estimates store var2
	
	var avg_10yr avg_Baa avg_russell
	estimates store var3
	var avg_funds avg_10yr avg_russell
    estimates store var4
	/*Structural Vector Auto-Regression*/
    /*Short Term*/
	
	//Public Debt on Rates
	//Models the contemporaneous effects across three independent variables
	matrix A = (1, 0, 0\., 1, 0\., ., 1)
	matrix B = (., 0, 0\0,. , 0\0, 0, .)
	

		/*AB models*/
		svar D.(Public_Debt ten_year_futures avg_Baa), aeq(A) beq(B) 
		svar D.(avg_Baa avg_10yr avg_russell), aeq(A) beq(B) 
		svar D.(avg_10yr avg_Baa avg_russell), aeq(A) beq(B)
        
		/*Counterfactual*/
		svar D.(avg_funds avg_10yr avg_Baa), aeq(A) beq(B)

	//Public Debt on Financial Markets with 4 x 4 matrix
	matrix A = (1, 0, 0, 0\., 1, 0, 0\., ., 1, 0\., ., ., 1)
	matrix B = (., 0, 0, 0\0, ., 0, 0\0, 0, ., 0\0, 0, 0, .)
	
		/*AB Models*/
		svar Public_Debt avg_10yr avg_Baa avg_russell, aeq(A) beq(B)
		svar D.(Public_Debt avg_10yr avg_Baa avg_russell), aeq(A) beq(B)
		estimates save svar_pdandyields
		
		/*Another angle*/
		svar Public_Debt avg_funds ten_year_futures avg_russell,aeq(A) beq(B)
		svar D.(Public_Debt avg_funds ten_year_futures avg_russell), aeq(A) beq(B)
		estimates save svar_pffr
//post diagnostic checks for equations
/*Casuality tests*/
vargranger

/*Autocorrelation in the lag orders residuals*/
varlmar

/*Running FEVD and IRF graphs*/ 
//IRF on index prices
irf create d_results_3, set(calm, replace) step(12)
irf create modela, set(results1) step(8)
irf graph irf, irf(d_results_3) response(avg_russell)	
irf graph sirf, irf(modela) response(s_sp500_p)
//FEVD 


//Printing tables



//Use matrix to make 

//Casuality table matrices

	//level
	matrix lvl_tbi = r(gstats)

	//difference


frmttable using result_0, statmat(lvl_tbi) sdec(3, 0, 3) hlines(110001000100010001) vlines(101011) title("Granger Causality(Level): Treasury Yields, Baa Yields and Index Value") ctitles("Dependent", "Explanatory", "Chi^2", "DF", "Probability of Chi^2") rtitles("Treasury Yields", "Baa Yields" \ "", "Russell Index" \"", "ALL" \ "Baa Yields", "Treasury Yields" \"", "Russell Index", \"", "ALL" \"Russell Index","Treasury Yields" \"", "Baa Yields" \ "","ALL") basefont(roman fs10) 

frmttable using level_fti, statmat(results1) sdec(3, 0, 3) hlines(110001000100010001) vlines(101011) title("Granger Causality(Level): Federal Funds Rate, Treasury Yields, and Index Value") ctitles("Dependent", "Explanatory", "Chi^2", "DF", "Probability of Chi^2") rtitles("Federal Funds", "Ten Yr Yields" \ "", "Russell Index" \"", "ALL" \"Treasury Yields", "Federal Funds"\"", "Russell Index", \"", "ALL" \"Russell Index","Federal Funds" \"", "10 Yr Yields" \ "","ALL") basefont(roman fs10)

frmttable using diff_tbi, statmat(d_results) sdec(3, 0, 3) hlines(110001000100010001) vlines(101011) title("Granger Causality(Differenced): Treasury Yields, Baa Yields and Index Value") ctitles("Dependent", "Explanatory", "Chi^2", "DF", "Probability of Chi^2") rtitles("Treasury Yields", "Baa Yields" \ "", "Russell Index" \"", "ALL" \ "Baa Yields", "Treasury Yields" \"", "Russell Index", \"", "ALL" \"Russell Index","Treasury Yields" \"", "Baa Yields" \ "","ALL") basefont(roman fs10)

frmttable using diff_fti, statmat(d_results1) sdec(3, 0, 3) hlines(110001000100010001) vlines(101011) title("Granger Causality(Differenced): Federal Funds Rate, Treasury Yields, and Index Value") ctitles("Dependent", "Explanatory", "Chi^2", "DF", "Probability of Chi^2") rtitles("Federal Funds", "Ten Yr Yields" \ "", "Russell Index" \"", "ALL" \"Treasury Yields", "Federal Funds"\"", "Russell Index", \"", "ALL" \"Russell Index","Federal Funds" \"", "10 Yr Yields" \ "","ALL") basefont(roman fs10) 

frmttable using level_tbi, statmat(results_2) sdec(3, 0, 3) hlines(110001000100010001) vlines(101011) title("Granger Causality(Leveled): Public Debt, Treasury Yields, Baa Yields and Russell Index") ctitles("Dependent", "Explanatory", "Chi^2", "DF", "Probability of Chi^2") rtitles("Public Debt", "Treasury Yields"\"","Baa Yields"\"","Russell Index"\"","All" \ "Treasury Yields","Public Debt"\"","Baa Yields"\"","Russell Index"\"","All"\"Baa Yields","Public Debt"\"","Treasury Yields"\"","Russell Index"\"","All"\"Russell Index","Public Debt"\"", "Treasury Yields"\"", "Baa Yields"\"", "Russell Index"\"",  "All") basefont(roman fs10)

frmttable, statmat(d_results2) sdec(3, 0, 3) hlines(110001000100010001) vlines(101011) title("Granger Causality(Differenced): Public Debt, Treasury Yields, Baa Yields and Russell Index") ctitles("Dependent", "Explanatory", "Chi^2", "DF", "Probability of Chi^2") rtitles("Public Debt", "Treasury Yields"\"","Baa Yields"\"","Russell Index"\"","All" \ "Treasury Yields","Public Debt"\"","Baa Yields"\"","Russell Index"\"","All"\"Baa Yields","Public Debt"\"","Treasury Yields"\"","Russell Index"\"","All"\"Russell Index","Public Debt"\"", "Treasury Yields"\"", "Baa Yields"\"", "Russell Index"\"",  "All") basefont(roman fs10) 

frmttable using dif_ptbr, statmat(d_results2) sdec(3, 0, 3) hlines(110001000100010001) vlines(101011) title("Granger Causality(Differenced): Public Debt, Treasury Yields, Baa Yields and Russell Index") ctitles("Dependent", "Explanatory", "Chi^2", "DF", "Probabality of Chi^2") rtitles("Public Debt", "Treasury Yields"\"","Baa Yields"\"","Russell Index"\"","All" \ "Treasury Yields","Public Debt"\"","Baa Yields"\"","Russell Index"\"","All"\"Baa Yields","Public Debt"\"","Treasury Yields"\"","Russell Index"\"","All"\"Russell Index","Public Debt"\"", "Treasury Yields"\"", "Baa Yields"\"",  "All") basefont(roman fs10)


