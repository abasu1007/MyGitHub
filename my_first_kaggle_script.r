**RMarkdown** enables you to interleave Markdown text, blocks of code, and output.

If you're in the editor, the resulting notebook will be shown on the HTML tab to the right.

This R environment comes with all of CRAN preinstalled, as well as many other helpful packages

The environment is defined by the [kaggle/rstats docker image](https://github.com/kaggle/docker-rstats).

For example, here's several helpful packages to load in 

```{r}
# This is a code block
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
```

Input data files are available in the "../input/" directory.

For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

```{r}
system("ls ../input", intern=TRUE)
```

Any results you write to the current directory are saved as output.

---
title: "My First Kaggle Script"
output:
  html_document:
    fig_width: 12
    fig_height: 12
    theme: united
    highlight: tango
---

```{r include=FALSE, cache=FALSE,echo=FALSE}

# Packages Used
library("dplyr")
library("data.table")
library("ggplot2")

# Create AC_Survey_Subset from full ACS data set
reRead <- 1

if(reRead==1){
  colsToKeep <- c("PINCP", "SCHL", "ESR")
  popDataA <- fread("../input/pums/ss13pusa.csv", select=colsToKeep )  
  popDataB <- fread("../input/pums/ss13pusb.csv", select=colsToKeep )
  AC_Survey_Subset <- rbind(popDataA, popDataB)
  rm(popDataA, popDataB)
  save(AC_Survey_Subset, file="AC_Survey_Subset.RData")
}else{
  load("AC_Survey_Subset.RData")
} 

# Prepare degree codes
degreeCode = "SCHL,Degree
21,Bachelor
22,Masters
24,Doctorate"
degree_codes <- fread(degreeCode)

# Turn of scientific notation 
options(scipen = 999)

```

---
title: "My First Kaggle Script"
output:
  html_document:
    fig_width: 12
    fig_height: 12
    theme: united
    highlight: tango
---

```{r include=FALSE, cache=FALSE,echo=FALSE}

# Packages Used
library("dplyr")
library("data.table")
library("ggplot2")

# Create AC_Survey_Subset from full ACS data set
reRead <- 1

if(reRead==1){
  colsToKeep <- c("PINCP", "SCHL", "ESR")
  popDataA <- fread("../input/pums/ss13pusa.csv", select=colsToKeep )  
  popDataB <- fread("../input/pums/ss13pusb.csv", select=colsToKeep )
  AC_Survey_Subset <- rbind(popDataA, popDataB)
  rm(popDataA, popDataB)
  save(AC_Survey_Subset, file="AC_Survey_Subset.RData")
}else{
  load("AC_Survey_Subset.RData")
} 

# Prepare degree codes
degreeCode = "SCHL,Degree
21,Bachelor
22,Masters
24,Doctorate"
degree_codes <- fread(degreeCode)

# Turn of scientific notation 
options(scipen = 999)

```

### Welcome

Welcome to my first Kaggle script! This first script is the result of the [Data Exploration With Kaggle Scripts](https://www.datacamp.com/courses/data-exploration-with-kaggle-scripts) course I took at DataCamp. If you have any feedback or suggestions, feel free to share! Hope you will enjoy it.  

Part of the course was about analyzing the data of the 2013 American Community Survey (ACS) to figure out whether an employed PhD makes more money compared to a working bachelor or master degree holder. To do this, I concentrated on B.Sc, MSc, and PhD holders data in ACS.

### Total Number of BSc , MSc, and PhD holders
One of my first analysis was comparing the total number of BSc, MSc, and PhD holders in the US. I first created a clean data set `AC_Survey_Subset_Cleaned` (I got this cleaned data by removing NA values, and by extracting only the bachelors, masters & PhD's), and next I made use of the [dplyr package](http://www.rdocumentation.org/packages/dplyr) to calculate each number:

```{r, message=FALSE, warning=FALSE}
# Load in the dplyr package and convert to tbl_df
library(dplyr)
AC_Survey_Subset <- tbl_df(AC_Survey_Subset) 

# Use the pipe operator and chaining 
AC_Survey_Subset_Cleaned <-  AC_Survey_Subset %>%
  na.omit() %>%
  filter(SCHL %in%  c(21,22,24)) %>%
  group_by(SCHL)

# Count the number of Bachelor, Master and PhD holders   
degree_holders <-  summarize(AC_Survey_Subset_Cleaned, count = n())

# Join degree_codes with degree_holders, assign to degree_holders_2
degree_holders_2 <- inner_join(degree_holders, degree_codes, by.x = c("SCHL"))

# Number of bachelors, masters and phd's in the US
bachelors <- as.numeric(degree_holders_2[1,2])
masters <- as.numeric(degree_holders_2[2,2])
PhDs <- as.numeric(degree_holders_2[3,2])

```

We learnt that there are `bachelors` (individuals with a bachelor degree), `masters` (individuals with a masters degree), and `PhDs` (individuals with a PhD). Visually this gives: 

```{r, message=FALSE, warning=FALSE}
# Visualize the number of Bachelor, Master and PhD holders   
ggplot(degree_holders_2, aes(x = Degree, y = count, fill = Degree)) +                        
  geom_bar(stat = "identity") +
  xlab("Degree") + 
  ylab("No of People") + 
  ggtitle("Comparing Degree Holders in the US")
```

The visualization of the data was done using the ggplot2 package. (If you're not yet familiar with the ggplot2 syntax, you can alway check out this [interactive course](https://www.datacamp.com/courses/data-visualization-with-ggplot2-1).) 

### Do PhD's Earn more?

```{r include=FALSE, cache=FALSE,echo=FALSE}

# Take 5000 random sample of 1000 observations & calculate summary statistics
over_thousand <- AC_Survey_Subset_Cleaned %>% # Exclude those whom earn less than $1000
  filter(PINCP>1000) %>%
  group_by(SCHL) 
    
freq <- 5000 # 5000 samples

result <- NULL
for(i in 1:freq){
  sample <-  sample_n(over_thousand,1000) # Select 1000 observations
  sample_stats <- summarise(sample, MinIncome=min(PINCP), MaxIncome=max(PINCP),
                          MedianIncome=median(PINCP), IncomeRange=IQR(PINCP))   # Calculate stats
  result <- rbind(result, sample_stats)
}

income <- result %>% 
      arrange(SCHL) 

income <- left_join(income , degree_codes, by.x=c("SCHL"))  # Mapping the codes of the school level to the proper names of the degrees
degrees <- factor(income$Degree, levels = unique(income$Degree)) # School levels are factors and so are the mapped names
  
```

Next, I needed to figure out whether it's a smart career choice moneywise to pursue a PhD. To do this I created a new data set (named `income`). Income is created by taking 5000 times a random sample of 1000 observations from The American Community Survey. For each sample  `min()`, `max()`, `median()`, and `IQR()` is calculated ([see code](http://www.r-fiddle.org/#/fiddle?id=5DmjrIA8)). I used these 5000 samples to create 3 boxplots based on median income for each degree level:

```{r, message=FALSE, warning=FALSE}

# Create the boxplots
ggplot(income, aes(x = Degree, y = MedianIncome, fill = Degree)) +  
  geom_boxplot() +
  ggtitle("Comparing Income of Degrees Holders") 
```

The graph clearly shows it is a smart career move to pursue a PhD :-) 
