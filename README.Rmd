# Finale-Data-Wrangling-Assignment
The finale assignment for my honours class, Data Pre-processing. In this assignment all the data wrangling skills that have been taught are used and explained. 

---
title: "MATH2349 Semester 2, 2018"
author: "Nicholas Pennell, s3588789"
subtitle: Assignment 3
output:
  html_document:
    df_print: paged
    html_notebook: default
    toc: yes
  pdf_document:
    toc: yes
---

## Required packages 

```{r, warning=FALSE, message=FALSE}

  library(dplyr) # Data wrangling
  library(tidyr) # Data wrangling
  library(readr) # Reading data
  library(ggplot2) # Plots
  library(outliers) # outliers
  library(DT) # Tables

```


## Executive Summary 

The selected data was taken, through all the main data preprocessing steps:
	 
  * **Get:** The data is open source baseball data taken from the Lahman Baseball Database and read in with `readr`, due to its speed being much faster, than base read functions.  
  * **Understand:** The variable types where examined and changed using, base functions and written functions. Some level names where simplified as well to help with understanding. 
  * **Tidyand Manipulate:** The data was checked against the main tidy data principles. 3 new variables where added for some extra insight.   
  * **Scan:** Missing values where searched for, found and removed or replaced with the mean. Outliers where found and went through two sperate treatments, Tukey's method and z-score, to try find which method was more applicable.  
  * **Transform:** The distribution of each numerical value was looked at. With one variable going through power transformation.  
  
The only bumps in the road appeared in dealing with outliers, due to the data coming from a real-world application. The result where still rather pleasing in a whole.

<a href="#top">Back to top</a>


## Data 

The data is from the [Lahman Baseball Database](http://www.seanlahman.com/). The database is made and maintained by reporter Sean Lahman. I got the data from [Kaggle]( https://www.kaggle.com/freshrenzo/lahmanbaseballdatabase) It could also be sourced from the `Lahman` packge.  

To keep things down in size I'm just going to be looking at just players that have won awards.  

The `csv` files are all read in with `read_csv()` from `readr` the merged using `dplyr` join function, `inner_join()`. One column was removed. The first 50 obsevations are shown.  

```{r, warning=FALSE, message=FALSE}

# Players that have made the Hall of Fame
  awards <- read_csv("C:/RMIT/Data Preprocessing/Assignment 3/Data/AwardsPlayers.csv")
# Batting Stats of players for a season
  batting <- read_csv("C:/RMIT/Data Preprocessing/Assignment 3/Data/Batting.csv")
# Merging
  baseball <- batting %>% inner_join(awards, by = c("playerID", "yearID", "lgID"))
# Removing unwated stint
  baseball <- baseball[, -c(3)]

```

* List of Varibles:
    + `playerID`, Player ID code
    + `yearID`, Year
    + `teamID`, Team ID code
    + `lgID`, League
    + `G`, Games played that year
    + `G_batting`, Games played as a batter that year
    + `AB`, At bats that year
    + `R`, Runs
    + `H`, Hits
    + `2B`, Doubles
    + `3b`, Triples
    + `HR`, Homeruns
    + `RBI`, Runs Batted In
    + `SB`, Stolen Bases
    + `CS`, Caught Stealing
    + `BB`, Base on Balls
    + `SO`, Strikeouts
    + `IBB`, Intentional walks
    + `HBP`, Hit by pitch
    + `SH`, Sacrifice hits
    + `SF`, Sacrifice flies
    + `GIDP`, Grounded into double plays
    + `G_old`, Old version of games (deprecated)
    + `awardId`, Name of award won
    + `tie`, Award was a tie
    + `notes`, Notes about the award

<a href="#top">Back to top</a>


## Understand

To find the type of each varible `sapply()` and `class()` where used. A fucntion found on [Stack Overflow](https://stackoverflow.com/questions/11261399/function-for-converting-dataframe-column-type) is used to change the class of multiple variables at one time, to be more efficient.  

* Changed varibles  
    + Player Id (To Factor)  
    + teamID (To Factor)  
    + lgID (To Factor)  
    + mlb_name (To Factor)  
    + mlb_team (To Factor)  
    + mlb_team_long (To Factor)  
    + tie (To Logical)
  
Then some levels in `awardID` where changed to more simple names for better understanding of what they are for. 

```{r}

# Looking at class of varibles
  sapply(baseball, class)
# Changing class of varibles
  # Function (from stack overflow)
    convert.magic <- function(obj, type)
    {
      FUN1 <- switch(type,
                     character = as.character,
                     numeric = as.numeric,
                     factor = as.factor)
      out <- lapply(obj, FUN1)
      as.data.frame(out)
    }
  # Using function
    # chaning to factor
      baseball[, c(1:4,24)] <- 
        convert.magic(baseball[, c(1:4, 24)], "factor")
    # chaning to logical
      baseball$tie <- ifelse(is.na(baseball$tie), FALSE, TRUE)
  # checking
    sapply(baseball, class)

# relabeling 
  # Current level names 
    award.names <- levels(baseball$awardID)
  # Creating new names for some awards
    new.award.names <- award.names
    new.award.names[3] <- "Postseason MVP"
    new.award.names[6] <- "Best Pitcher"
    new.award.names[7] <- "Best Fielder At Postion"
    new.award.names[8] <- "Best Hitter"
    new.award.names[13] <- "Best Relief Pitcher"
    new.award.names[15] <- "Best Offensive Player At Position"
    
  # Renaming Levels
    baseball <- baseball %>% mutate(awardID = factor(awardID, levels = award.names, 
                                                     labels =  new.award.names))
  # check
    levels(baseball$awardID)

```

<a href="#top">Back to top</a>


##	Tidy & Manipulate Data I 

The three main tidy Data Principles:  

  1. Each variable must have its own column.  
  2. Each observation must have its own row.  
  3. Each value must have its own cell.  

All three of these principles are alredy meet. There for no changes need to be made.

<a href="#top">Back to top</a>


##	Tidy & Manipulate Data II 

Created 3 new varibles,
   
* `batting average (BA)`, is the number of hits dived by the number of at bats  
* `On Base Percentage (OBP)`, is how frequently a batter reaches base  
* `Slugging Percentage (SLP)`,  is a measure of a batters productivity  
Also added `1B` to use in `SLP` and if the player is a pitcher or not, for futer use.

```{r}

# creating batting average (BA), On Base Percentage (OBP) and Slugging Percentage (SLP)
  # BA (hits/At Bats) 
    baseball$BA <- baseball$H/baseball$AB
  # OBP ((H+BB+HBP)/(H+BB+HBP+SF))
    baseball$OBP <- (baseball$H + baseball$BB + baseball$HBP)/
      (baseball$H + baseball$BB + baseball$HBP + baseball$SF)
  # SLP ((1+2*2B+3*3B+4*HR)/AB)
    # Adding 1b hits
      baseball$`1B` <- baseball$H - baseball$`2B` - baseball$`3B` - baseball$HR
  
    baseball$SLP <- (baseball$`1B` + 2*baseball$`2B` + 3*baseball$`3B` + 4*baseball$HR)/baseball$AB
  # Adding Pitches
    pitcher.notes <- pitcher.notes <- c("P", "LHP", "RHP", "Rp", "SP")
    baseball$pitcher <- ifelse(grepl("Pitcher", baseball$awardID) == TRUE | baseball$G_batting == 0 |
                                 baseball$AB == 0 | baseball$notes %in% pitcher.notes,
                           TRUE, FALSE)
      
```

<a href="#top">Back to top</a>


##	Scan I 

An important note for baseball date is that not all stats have been reocred, as some stats are only being recored in more recent years.  
  
The first step was to look at the total Na by varibel. I then removed pitchers from the data set as they typically don't bat, or bat an insignificant amount. The remaining missing vaules are from the more moden day stats, apart from `SO`.

So with the missing `SO` where replaced with the mean and then a complete cases was taken.

```{r}

# Percent of Missing Vaules by Varible
  baseball %>% summarise_each(funs(sum(is.na(.))))
# Excluding Pitchers and NA
  baseball <- baseball %>% filter(pitcher == FALSE)
# Remaning Missing Vaules by Varible
  baseball %>% summarise_each(funs(sum(is.na(.))))
# SO mean
  baseball$SO <- ifelse(is.na(baseball$SO) == TRUE, mean(baseball$SO, na.rm = TRUE), baseball$SO)
# complte case
  baseball <- baseball[complete.cases(baseball), ]
# check
  sum(is.na(baseball))

```

<a href="#top">Back to top</a>


##	Scan II

once again with baseball stats due to how long the game has been played and the change of how its played and new rules over time, there are many abnormalitys. Also since we are looking at just award winning player these are pretty much the players that where outliers them self in a single season.  

To tackle the outliers, two methods where attempted, first `Tukey's method of outlier detection`, then `z-scores` where used.

The choice to use two methods came from the fact that neither method worked perfectly. As once a larger number of outliers where removed from the dataset, new outliers are created.


### Tukey's method of outlier detection

First we take a look at the outliers in a box plot, using `ggplot` and reshaping the data into long format, using `tidyr`, `gather()` to graph all varibles at one time. A function was also made to count the number of outliers, `sum.outlier()`.  

To remove the outliers a function, `outlier.qr()`, was created and put throw a for loop to be used on all numeric and integer variables. 

To See the results we repeat the earlier steps. 

```{r, fig.align = "center", out.width = "100%", fig.width = 7.5}

# First Looking at Outliers
  # Creating a Data Farme to Plot All Numrical Vaibles at Once
    plot.table <- baseball[, c(5:23, 27:30)] %>% gather(key = "variable", value = "value") 
  # Plot
    ggplot(data = plot.table, aes(x=variable, y=value)) + geom_boxplot() + 
      facet_wrap( ~ variable, scales="free")
  
  # Function to Sum Outlier
    sum.outlier <- function(x)
      {
        # IQR and Quantile Vaules
          iqr <- IQR(x, na.rm = TRUE)
          qr <- quantile(x, c(0.25,0.75))
          Q1 <- qr[[1]] - 1.5*iqr
          Q3 <- qr[[2]] + 1.5*iqr
        # Printing Number of Rows That are Varibles
          x[x > Q3 | x < Q1] %>% length()
      }
  
  # Using Function on Integer and Numeric Varibles
      sapply(baseball[, c(5:23, 27:30)], sum.outlier)

# Treatment      
  # Function to Remove Outliers Using Outlier Detection 
    outlier.qr <- function(data, var_name)
      {
        data$var_name <- eval(substitute(var_name), data) 
        iqr <- IQR(data$`var_name`, na.rm = TRUE)
        qr <- quantile(data$var_name, c(0.25,0.75))
        Q1 <- qr[[1]] - 1.5*iqr
        Q3 <- qr[[2]] + 1.5*iqr
        data <- data %>% filter(var_name < Q3 & var_name > Q1)
        return(data)
      }

  # List of Vraibles That are Having Outliers Removed
    varNames <- colnames(baseball[, c(5:23, 27:30)])
    varNames <- lapply(varNames, as.name)
    
  # For Loop to Remove Outliers
    # Duplicate of Baseball Data Frame
      baseball.qr <- baseball
    for (i in seq_along(varNames)) 
      {
        baseball.qr <- outlier.qr(baseball.qr, eval(varNames[[i]]))
      }

# Results
  # Creating a Data Farme to Plot All Numrical Vaibles at Once
    plot.table <- baseball.qr[, c(5:23, 27:30)] %>% gather(key = "variable", value = "value") 
  # Plot
    ggplot(data = plot.table, aes(x=variable, y=value)) + geom_boxplot() + 
      facet_wrap( ~ variable, scales="free")
  # Outlier Count
    # By Varible
      sapply(baseball.qr[, c(5:23, 27:30)], sum.outlier)
    # Total
      sum(sapply(baseball.qr[, c(5:23, 27:30)], sum.outlier))
```

### z-score

The method for z-score is almost identical as the above method. A function was used to find the total outliers by varible. The a function, `outlier.zs()`, was used to remove the outliers.

```{r}

# First Look at Outliers by Varible
  sapply(baseball[, c(5:23, 27:30)], function(x){sum(abs(scores(x, type = "z"))>3)})

# Function to Remove Outliers Using Z-score
  outlier.zs <- function(data, var_name)
    {
      data$var_name <- eval(substitute(var_name), data) 
      data$z.score <- data$`var_name` %>% scores(type = "z")
      data <- data %>% filter(abs(z.score) <3)
      return(data[, -c(32,33)])
    }

# For Loop to Remove Outliers
  # Duplicate of Baseball Data Frame
    baseball.zs <- baseball
  for (i in seq_along(varNames)) 
    {
      baseball.zs <- outlier.zs(baseball.zs, eval(varNames[[i]]))
    }  
  
# Results
  # By Varible
    sapply(baseball.zs[, c(5:23, 27:30)], function(x){sum(abs(scores(x, type = "z"))>3)})
  # Total
    sum(sapply(baseball.zs[, c(5:23, 27:30)], function(x){sum(abs(scores(x, type = "z"))>3)}))
    
    
```

***

Neither method produced great results as once several outliers are removed, new outliers are produced. Outliers could be treated as multivariate outliers rather than Univariate outliers, but this is a much lengthier process.

Due to the Tukey's Method having less total outliers that data set well be used from this point on.

<a href="#top">Back to top</a>


##	Transform 

To see the distubtion of each varible a `ggplot` was made with the same method that was used for the box plots above, this time for histograms and normal QQ plots. This is done to see which varibles need to be transformed.

```{r, fig.align = "center", out.width = "100%", out.height="100%"}

# Plot tabel
  plot.table <- baseball.qr[, c(5:23, 27:30)] %>% gather(key = "variable", value = "value")

# Histogram 
  ggplot(data = plot.table, aes(value)) + geom_histogram(bins = 10, colour = "orange") + 
    facet_wrap( ~ variable, scales = "free")

# qqnorm
  ggplot(data = plot.table, aes(sample  = value)) + stat_qq(color = "red", alpha = 0.4) + 
    stat_qq_line() + facet_wrap( ~ variable, scales = "free")
  
```

Due to how different each variable is only `G` will be changed.  
Due to how left skewed the variable is to correct this a large power transformation was used.  
  
```{r, out.width = "100%", out.height = "100%"}

# chanignig Games (G)
  baseball.qr$G2 <- baseball.qr$G^(8)
# Plot
  ggplot(baseball.qr, aes(x = G2)) + geom_histogram(bins = 10, colour = "orange") + 
    theme(legend.position='none')

```

<a href="#top">Back to top</a>


## Finale Data

```{r}

# Print
  baseball.qr

```

<br>
<br>
