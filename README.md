# fitnessTrackerCaseStudy (Google Analytics Case Study)

## Goal and Hypothesis

## Abstract

## Method of Analysis and Proof

I began my analysis by writing a simple driver file: [summaryDriver.R](summaryDriver.R). I commonly implement this practice so I can comment out parts of code when I am satisfied with them.  The driver reads in the data to an R dataframe then passes it to the categorized functions.

As with all analysis, I needed to understand the simple observations of the data before more complex observations could be made.  I wrote a function that plots a histogram and a scatterplot relative to time for the each column in the dataframe passed to the function.  

I began with the dailyActivity.csv, and proceeded to summarize each *.csv. 

<div style='display:inline'>
<img src="plots\dailyActivity_merged\SummaryHist_Calories_byCount.png" alt="Calories Historgram" title="Calories Historgram" width="400" height="400" /> 
<img src="plots\dailyActivity_merged\SummaryScatter_Calories_byId.png" alt="Calories Historgram" title="Calories Historgram" width="400" height="400" /> 
</div>

At this point I start to form 

~[](plots\dailyActivity_merged\TotalStepsvsCalories_byId.png)

![](plots\dailyActivity_merged\Distance_ByTypeBar.png)

Interesting Point: 
![](plots\dailyActivity_merged\CaloriesandStep-DistanceRatio.png)

![Calories over Time](/plots/dailyCalories_merged/CaloriesOverTime.png)