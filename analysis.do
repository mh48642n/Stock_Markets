//clears out memory before beginning
clear

//sets working directory
cd "C:\Users\marvi\OneDrive\Documents\GitHub\Stock_Markets"

//imports data into stata
//import excel standard_tzs.xlsx, firstrow clear
//import excel monthly_dataset.xlsx, firstrow clear
import excel weekly_dataset.xlsx, firstrow clear

//declaring data to be time series
//for monthly_dataset
//generate time = tm(2001m1) + _n - 1
//format time %tm
//tsset time

//for weekly_dataset
generate time = tw(2001w1) + _n - 1
format time %tw
tsset time

//for daily_dataset
//generate time = td(01jan2000) + _n - 1
//format time %td
//tsset time

//dropping observations 
//drop if missing(sp_500)
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

/*Viewing Data*/
describe
summarize
cor


/*MULTIVARIATE VECTOR AUTO-REGRESSION*/
	//test if data is stationary
	dfuller d.(avg_standards), trend
	dfuller d.(avg_russell), trend
	
	//Lag length checker
	varsoc D.(avg_10yr avg_Baa avg_russell)
	varsoc D.(avg_funds avg_10yr avg_russell)
	
	
	//Index Price: Russell 2000 
	//No autocorrelation in the residuals at the 95% range and the var is stable 
	var D.(avg_10yr avg_Baa avg_russell)
    varlmar
	varstable	
	vargranger
	
	var D.(avg_funds avg_10yr avg_russell)
	varlmar
	varstable	
	vargranger
	
//	var avg_10yr avg_Baa avg_russell	
//	var avg_funds avg_10yr avg_russell
 
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

	//Public Debt on Financial Markets with 4 x 4 matrix
	matrix A = (1, 0, 0, 0\., 1, 0, 0\., ., 1, 0\., ., ., 1)
	matrix B = (., 0, 0, 0\0, ., 0, 0\0, 0, ., 0\0, 0, 0, .)
	
	
	

	
		/*AB Models*/
//		svar Public_Debt avg_10yr avg_Baa avg_russell, aeq(A) beq(B)
		svar D.(Public_Debt avg_10yr avg_Baa avg_russell), aeq(A) beq(B)
		irf set "impr.irf"
	    irf create test0, step(8)
		irf graph oirf, impulse(D.Public_Debt D.avg_10yr D.avg_Baa D.avg_russell) response(D.avg_russell)	

		
		/*Another angle*/
//		svar Public_Debt ten_year_futures avg_funds avg_russell,aeq(A) beq(B)
		svar D.(Public_Debt ten_year_futures avg_funds avg_russell), aeq(A) beq(B)
		irf set "impr1.irf"
		irf create test1, step(8)
		irf graph oirf, impulse(D.Public_Debt D.ten_year_futures D.avg_funds D.avg_russell) response(D.avg_russell)









