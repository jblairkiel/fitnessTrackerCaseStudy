# fitnessTrackerCaseStudy (Google Analytics Case Study)

## Method of Analysis and Proof

I began my analysis by writing a simple driver file: [summaryDriver.R](summaryDriver.R). I commonly use this practice so I can comment out parts of code when I am satisfied with them, and reduce overall runtime.  The driver reads in the data to an R dataframe then passes it to the categorized functions.

As with all analysis, I needed to understand the simple observations of the data before more complex observations could be made.  I wrote a function that plots a histogram and a scatterplot relative to time, for each column in the dataframe passed to the function.  
___
### Summarizing the data for context

I began with the dailyActivity.csv, and proceeded to summarize more *.csv files. 

![SummaryHist_Calories_byCount](plots\dailyActivity_merged\SummaryHist_Calories_byCount.png)

![SummaryScatter_Calories_byId](plots\dailyActivity_merged\SummaryScatter_Calories_byId.png)
___
### Daily Activity Intensities


At this point I start to reinforce my hypothesis by confirming how the overwhelming amount of SedementaryActivity is the overwhelming amount of time.  The second figure in this second is a scatter plot of the daily minutes in each IntensityType compared to the amount of calories burned each day.  I removed the points in the third graph to better view this relationship.

![TypeMinutes_ByID](plots\dailyActivity_merged\TypeMinutes_ByID.png)

![CaloriesbyMinutes_ByTypeTrend](plots\dailyActivity_merged\CaloriesbyMinutes_ByTypeTrend.png)

![CaloriesbyMinutes_ByTypeTrendOnly](plots\dailyActivity_merged\CaloriesbyMinutes_ByTypeTrendOnly.png)

___
### Burning more Calories

I made an intersting observation by creating a ratio of DailyTotalSteps / DailyTotalDistance for each TrackerUser.  My hypothesis is that if someone is running, they are going to be taking longer strides to cover a farther distance.  Compare this to a person who is not exercising, but rather staying more sedentary their normal walking pace will cover less distance per step and makes up the bulk of the tracked "Sedentary" and "Light" activity.

![CaloriesandStep-DistanceRatio](plots\dailyActivity_merged\CaloriesandStep-DistanceRatio.png)

___
###  Staying Active and not Quitting

A final observation I made was that even people who quit excercising before the complete study was finished, saw themselves becoming more conditioned burning more calories but due to subjective factors for each user, inevitably burned out.  Reinforcing how well these potential "quitters" were doing at the height of their progress, could have energized them to continue excercising before they dropped off of the study.  The first figure shows the calories burned by all participants.  I noticed the drop-offs, filtered them and added trendlines.

![Calories over Time](/plots/dailyCalories_merged/CaloriesOverTime.png)
![QuittersCaloriesOverTime](plots\dailyCalories_merged\QuittersCaloriesOverTime.png)