//clears out memory before beginning
clear

//sets working directory
cd "C:\Users\marvi\OneDrive\Documents\GitHub\Stock_Markets"

//imports data into stata
import excel stationary_dataset.xlsx, firstrow clear
import excel monthly_dataset.xlsx, firstrow clear

//declaring data to be time series
//for monthly_dataset
generate t = tm(2001m1) + _n - 1
format t %tm
tsset t

//for daily_dataset
generate time = td(01jan2001) + _n - 1
format time %td
tsset time

//dropping observations 
drop if missing(s_sp500_p)
drop dates
order time

/*BIVARIATE VECTOR AUTO-REGRESSION*/
	//test if data is stationary
	dfuller s_russellp, trend
	dfuller s_sp500_p, trend
	
	//Index Prices: Russell 2000 and S&P 500 
	var s_sp500_p s_funds_rate s_Baa s_T_rate
	var s_russellp s_funds_rate s_Baa s_T_rate
     
	//Debt and rates
	var s_T_rate s_Baa s_govtheld
	var s_T_rate s_Baa s_publicheld
 
/*Structural Vector Auto-Regression*/
/*Short Term*/
	
	//Public Debt on Rates
	//Models the contemporaneous effects across three independent variables
	matrix A = (1, 0, 0\., 1, 0\., ., 1)
	matrix B = (., 0, 0\0,. , 0\0, 0, .)
	
		/*AB models*/
		svar s_publicheld s_T_rate s_Baa, aeq(A) beq(B) 
		
		/*Counterfactual*/
		svar s_funds_rate s_T_rate s_Baa, aeq(A) beq(B) 

	//Index Prices and Public Debt
	//Models the contemporaneous effects between these independent variables
	matrix A = (1,0,0,0\.,1,0,0\.,.,1,0\.,.,.,1)
	matrix B = (.,0,0,0\0,.,0,0\0,0,.,0\0,0,0,.)
	
		/*AB models*/
		svar s_publicheld s_T_rate s_Baa s_sp500_p, aeq(A) beq(B)  
		svar s_publicheld s_T_rate s_Baa s_russellp, aeq(A) beq(B)
	
//post diagnostic checks for equations
/*Casuality tests*/
vargranger

/*Autocorrelation in the lag orders residuals*/
varlmar

/*Running FEVD and IRF graphs*/ 
//IRF on Baa yields

//IRF on T_rate	
	
	