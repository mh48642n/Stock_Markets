//clears out memory before beginning
clear

//sets working directory
cd "C:\Users\marvi\OneDrive\Documents\GitHub\Stock_Markets"

//imports data into stata
import excel dataset_tsz.xlsx, firstrow clear

//gives the description of each variable 
codebook
describe

//declaring data to be time series
//this points out that the data is daily
format dates %td
tsset dates


/*BIVARIATE VECTOR AUTO-REGRESSION*/
	//test if data is stationary
	
	
	//Index Prices: Russell 2000 and S&P 500 
	//Also compare changes to the funds rate to monetary base and then prices
	var 

	//Index Prices and Unemployment
	
