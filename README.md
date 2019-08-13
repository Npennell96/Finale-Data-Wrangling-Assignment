# Finale-Data-Wrangling-Assignment
The finale assignment for my honours class, Data Pre-processing. In this assignment all the data wrangling skills that have been taught are used and explained. 

[rpubs link](http://rpubs.com/Npennell/MATH2349_Assignment3)

## Executive Summary
The selected data was taken, through all the main data preprocessing steps:
	 
  * **Get:** The data is open source baseball data taken from the Lahman Baseball Database and read in with `readr`, due to its speed being much faster, than base read functions.  
  * **Understand:** The variable types where examined and changed using, base functions and written functions. Some level names where simplified as well to help with understanding. 
  * **Tidy and Manipulate:** The data was checked against the main tidy data principles. 3 new variables where added for some extra insight.   
  * **Scan:** Missing values where searched for, found and removed or replaced with the mean. Outliers where found and went through two sperate treatments, Tukey's method and z-score, to try find which method was more applicable.  
  * **Transform:** The distribution of each numerical value was looked at. With one variable going through power transformation.  
  
The only bumps in the road appeared in dealing with outliers, due to the data coming from a real-world application. The result where still rather pleasing in whole.
