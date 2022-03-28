**Analysis of Mitochondrial Respiration:**
================
**Stephen Decker, MS, ACSM-CEP**  
Department of Kinesiology  
University of Massachusetts Amherst
Last Updated March 2022  
Built with R version 4.1.0 and RStudio

















# Disclaimer

        This is a template for the code to analyze mitochondrial
respiration rates collected from the Oroboros Oxygraph O2K. I have
created this file for public use, but please consider that I did this
out of my free time and will continue to update and change this document
as time goes on.  
        I ***do not*** intend to restrict access to this document.
However, my only request is that if others use this document,
appropriate referencing and citation to this project is greatly
appreciated when necessary. Furthermore, I fully intend to update this
document as my skills in R improve and more methods of analysis become
available. For comments, questions, or concerns, please email me at
<stdecker@umass.edu> or contact me via any of the links at the bottom of
each page. Also, much of my knowledge has come from a vast source of
publicly available sources – books, [Stack
Exchange](stats.stackexchange.com), [Data Novia](www.datanovia.com), and
others. If you wish to have any of this information, please contact me
and I will gladly share my resources. I will also include links to them
at [the end of this document](#Resources).  
        Please keep in mind that I am a physiologist by training. I try
to do my best with statistics and code, but neither of those are
something I would consider my trade. Thus, I am very open to hearing
comments and critiques of this document and suggestion for improvements
are much appreciated. My ultimate goal is to produce scientific
discovery in the correct way, and I will gladly take any feedback on how
to achieve that goal.

------------------------------------------------------------------------

# 1. Basics of this Document

I would like to outline some of the code I think is critical to
understand for this document:

## 1.1. Packages

``` r
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
...
```

        You’ll notice that I use several packages in this document. This
is simply because the analyses that I have below will call upon many
functions that are spread across several packages.You may not need all
of these packages to run this code, depending on what you are trying to
do with this analysis. However, I will include all of these in my
documentation for my use. Feel free to discard anything you think may be
unused. More importantly, I find that resources, such as
[RDocumentation](https://www.rdocumentation.org/) and the [CRAN package
search](https://cran.r-project.org/) are very useful guides to
understanding the capabilities and layout of each package.

        To install a package, you’ll simply use the `install.packages()`
function with the name of the package in parentheses. Then, you must
load the package from your R library using the `library()` function. To
install and load a package like the `'praise'` package, do the
following:

``` r
install.packages("praise")
```

``` r
library('praise')
```

        Now we can begin using the functions inside this package. The
‘praise’ package only has one function. If we use this function in our
code, we should see an output that gives us a compliment, like so:

``` r
praise()
```

    ## [1] "You are top-notch!"

## 1.2. Taking Notes

        Taking notes is a particularly useful skill to utilize in R,
especially when running long lines of code (like you will probably do in
graphing). To make a note, simply put a `'#'` followed with text, then
start a new line to continue the code. This note will not be analyzed by
R, but will remain in the interface. For an example:

``` r
#The 'praise' package gives me praise when I use it
praise()
```

    ## [1] "You are posh!"

``` r
#2 x 3 always equals 6
2*3
```

    ## [1] 6

## 1.3. Pulling data from Excel

``` r
df.name <- read_excel("c\\Users\\complete_file_path\\file_name.xlsx", 
      sheet = "Sheet Name", col_names = TRUE)
```

        This line of code will pull your data from an Excel sheet. I
have provided [an Excel template
file](https://github.com/sdecker4/Oroboros-O2K-Analysis/tree/master/Excel%20Analysis)
on my personal GitHub page. Please feel free to use this as needed with
the same stipulations mentioned earlier. The code I have made below is
made for the setup in this file, so keep that in mind as this data
becomes analyzed.  
        This script is fairly intuitive to understand — you will name
your data frame (by changing “df.name”) and your full file path
(including the path directory) goes into the first set of quotations. If
you have specific sheet names you wish to call upon, it goes into the
next set of quotation marks. Lastly, if you have column names, keep this
as TRUE.

## 1.4. Statistical Analyses

        You will notice that I have several different methods of
analyzing the data (t-tests, ANOVA) in this document. I have tried to
include several different types of analyses and ways to streamline these
analyses.  
        One **major** thing to consider in this process is the layout of
the data being analyzed. In order to run a proper ANOVA, you ***must***
have the data organized in [long format, not wide
format](https://www.theanalysisfactor.com/wide-and-long-data/). There
are [ways to do
that](http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/)
and I cover it [later in the document](#Wide-Long), but I will note that
my excel files come in both wide (in the t-test tab) and long format (in
the ANOVA tab) so that the analysis can be done without that conversion.
However, it may be useful to explore that, especially if you want to add
other variables of your own in this analysis.

### 1.4.1. T-tests

        Performing t-tests in these types of analyses are, in my
opinion, controversial at best. I’ve heard the argument that the
addition of different substrates to the respiration medium changes the
state in which the sample exists, and therefore these are independent
conditions. However, I think it could easily be argued that the
respiration of a sample is dependent on the individual sample and
therefore the respiration of a sample in any given state is still
dependent on the other states. Furthermore, there is always the issue of
a family-wise error in the p-value. Therefore, I still recommend
performing an ANOVA for all analyses, but it is the choice of each lab
to make that decision.

``` r
ttest_values <- lapply(df.name[,6:16], function(x) 
      t.test(x~ df.name$data_range, na.rm = TRUE))
ttest_pvalues <- data.frame(p.value = 
      sapply(ttest_values, getElement, name = "p.value"))
```

        For example, I have designed this code to perform several
t-tests (10 to be exact — from column 6 to 16) with one line of code,
then it will output a data frame with all of the p-values using the
second line of code. Again, we run into the issue of family-wise
p-values in this type of analysis, and I have not yet accounted for that
in this analysis. I will explain this in more detail later.

### 1.4.2. ANOVA

        The ANOVA code is pretty straightforward. With this code, you
can perform a one-way or a two-way ANOVA extremely easily. I would rely
more heavily on these results compared to the t-tests, following the
code for a one-way ANOVA:

``` r
ANOVA_model <- aov(df.name$dependent.variable ~ 
      df.name$factor1, data = df.name)
anova(ANOVA_model)
```

Or a two-way and/or factorial ANOVA:

``` r
ANOVA_model <- aov(df.name$dependent.variable ~ 
      df.name$factor1 * df.name$factor2, data = df.name)
anova(ANOVA_model)
```

        I find this process to be pretty easy to understand. The first
line creates a linear model of the data using the `'aov'` function. This
calls the ‘dependent variable’ and the grouping and condition
(independent) variables. Then, the `'anova'` function will run a Type I
ANOVA, completing sum of squares (Sum Sq), degrees of freedom (Df), F
values, probability (Pr), and denoting significance with asterisks.
Overall, this should give you everything you need for a fundamental
analysis of the ANOVA. Alternatively, you can use the `Anova` function
for a Type II and Type III ANOVA. For more information on this, [see
this
description](https://mcfromnz.wordpress.com/2011/03/02/anova-type-iiiiii-ss-explained/).
For example, the output for a two-way ANOVA will be a table similar to
the one as follows:

``` r
Analysis of Variance Table

Response: df.name$dependent.variable
                                              Sum Sq  Df F value  Pr(>F)    
df.name$condition                             335.9   1  3.4005 0.06729 .  
df.name$group                               29767.4   9 33.4822 < 2e-16 ***
Interaction                                   338.0   9  0.3802 0.94299    
Residuals                                   13829.7 140                    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

        This format is fairly simple to understand the overall
significance of the ANOVA. For further analyses (such as post-hoc
tests), there is a way to perform a Tukey HSD or pairwise t-tests.
Additionally, you can [analyze the linear model](#GLM) on these data.

## 1.5. Graphing

        I will also include code to [create plots](#Visualizing), for
example, a bar plot:

<img src="Manual_files/figure-gfm/resp-plot-1.png" title="Sample representation of graphing respirometry data with bar graphs." alt="Sample representation of graphing respirometry data with bar graphs." style="display: block; margin: auto;" />

Or, we can make box plots like so:
<img src="Manual_files/figure-gfm/box-plot-1.png" title="Sample representation of graphing respirometry data with boxplots." alt="Sample representation of graphing respirometry data with boxplots." style="display: block; margin: auto;" />

My aim is to cover most of the basics of these functions and give out
details to my code so that others can easily create graphical
representations and reports of their own. However, as I am not quite an
expert with all of this and I am learning as I go, I will not be able to
cover everything. Hopefully, though, I have enough covered in this
document to help guide most people through the process of analyzing and
visualizing respirometry data

------------------------------------------------------------------------

# 2. Preparing the Data

        Overall, this process of running an analysis is pretty simple
and straightforward. [As I mentioned earlier](#t-tests), it is best to
avoid running t-tests on data sets such as these because of the
family-wise error problem. I’m not saying you shouldn’t ever run
t-tests, but it certainly makes sense to run an ANOVA (or better yet, a
planned comparisons analysis) and then t-tests with appropriate post-hoc
corrections.

## 2.1. Pulling the Data

        I mentioned the steps for this process in the [Pulling data from
Excel](#Pulling) section of this document. Since I mostly use Excel
files for my processing and I have a fairly useful template file, I will
stick with pulling the data from Excel. If you have other means of
processing the data, feel free to skip this section. Last, since the
data in the ANOVA tab is usually the only data that needs to be cleaned
up, I will focus on this data. The data in the t-test tab usually
doesn’t need any cleanup, and any cleanup it needs will be done using
the methods outlined in this section.  
        For this step, we will need to load the `'readxl'` package from
our library by using the command below.[1]

``` r
library(readxl)
```

        This simply tells R that you will be calling some of the
functions within this package, which we will certainly do. Next, we run
the code to extract the data we need from the excel sheet using the
`'read_excel'` function. Notice that it important to use the full file
directory when extracting the Excel file. We should use the following:

``` r
df.name <- read_excel("c\\complete_file_path\\file_name.xlsx", 
      sheet = "Sheet Name", col_names = TRUE)
```

Or, something more realistic might be:

``` r
data_example_long <- read_excel(
  "C:\\Users\\Your_name\\Desktop\\Example_FFA_O2K_Analysis.xlsx", 
  sheet = "Final Data ANOVA", col_names = TRUE)
```

This function will give us a data frame from the sheet we have selected
(in this example the ‘Final Data ANOVA’ Sheet). If you have different
names for sheets, you can change this by editing the `function sheet =`
line. Simply replace the name in quotations with your own. However, it
is important to maintain the quotations around the sheet name.

        If we now begin to look to see what the data frame looks like by
typing the name of the data frame (in this case `'data_example_long'`)
we will get something that matches the data table we have from Excel. If
we look at the first few rows:

| Subject | Condition |  Leg  | Respiration State | Respiration Rate (pmol/sec/mg) |
|:-------:|:---------:|:-----:|:-----------------:|:------------------------------:|
|    1    | Baseline  | Right |       Basal       |              5.46              |
|    1    | Baseline  | Right |       OctM        |              6.69              |
|    1    | Baseline  | Right |     OctMD(25)     |             10.24              |
|    1    | Baseline  | Right |     OctMD(50)     |             10.12              |
|    1    | Baseline  | Right |    OctMD(250)     |             11.69              |
|    1    | Baseline  | Right |    OctMD(5000)    |             24.44              |
|    1    | Baseline  | Right |      OctMDGS      |             46.35              |
|    1    | Baseline  | Right |       CytC        |             44.69              |
|    1    | Baseline  | Right |     FCCP Peak     |             51.11              |
|    1    | Baseline  | Right |        Omy        |             19.93              |

First 10 rows of the data set.

You’ll notice in this data frame the columns have been identified and
transferred into the data frame. This can be modified by the
`col_names=` line – if you don’t want to have the column names, simply
change `TRUE` to `FALSE`. Furthermore, the data columns in this document
all have random values. Other than that that, any other cells that have
any sort of value in them will only appear as `"NA"`. Once missing
numbers have been filled in, you will see those numbers in their
respective column. We will get into some more of the details of the data
frame later. If we want to use the t-test tab, simply change the code
to:

``` r
data_example_wide <- read_excel(
  "C:\\Users\\Stephen\\OneDrive - University of Massachusetts\\Desktop\\O2K Analysis Files\\Example_FFA_O2K_Analysis.xlsx", 
  sheet = "Final Data t-test", col_names = TRUE)
```

That should have all of the same information, just in a different
layout, which we will discuss next.

## 2.1.1. Wide & Long Formatting

        Something that may be useful to have in your arsenal is being
able to change your data from [wide format to long (otherwise known as
narrow or vertical)
format](https://en.wikipedia.org/wiki/Wide_and_narrow_data). ***Long
format will be very useful when we [graph our data](#Visualizing), which
is why I will give you ways to calculate all of your variables in long
format as you will need to have the code in long format eventually.***
This is extremely critical, and some of the functions are just more
intuitive to perform in long format so I don’t always have the code done
in wide format.[2] I define these terms as follows

-   **Wide format** is the arrangement of data such that each variable
    is in its own column, and each row is assigned to only one
    subject.  
-   **Long format**, on the other hand, is the arrangement of data such
    that one column contains all of the values of a variable and another
    column contains the context of this given value.

If we refer to our [Excel sheet that I have provided](#Pulling), you
will notice that the tab titled “Final Data t-test” is in wide format,
where if I take the first 9 columns we should see the following:

| Subject | Condition | Mass |  Leg  | Basal | OctM  | OctMD(25) | OctMD(50) | OctMD(250) |
|:-------:|:---------:|:----:|:-----:|:-----:|:-----:|:---------:|:---------:|:----------:|
|    1    | Baseline  | 1.65 | Right | 5.46  | 6.69  |   10.24   |   10.12   |   11.69    |
|    2    | Baseline  | 1.46 | Left  | 6.78  | 4.94  |   10.73   |   10.92   |   10.72    |
|    3    | Baseline  | 1.57 | Right | 4.08  | 7.43  |   8.16    |   9.92    |   12.60    |
|    4    | Baseline  | 1.36 | Left  | 5.66  | 2.80  |   10.73   |   12.55   |   14.90    |
|    5    | Baseline  | 1.38 | Right | 6.73  | 4.05  |   4.91    |   13.79   |    8.18    |
|    6    | Baseline  | 1.42 | Left  | 2.56  | 10.46 |   8.00    |   11.56   |   15.50    |
|    7    | Baseline  | 1.31 | Right | 4.81  | 7.22  |   9.86    |   6.91    |   11.77    |
|    8    | Baseline  | 1.52 | Left  | 5.17  | 6.86  |   10.63   |   9.93    |   15.99    |
|    9    | Baseline  | 1.49 | Right | 5.05  | 10.38 |   7.37    |   13.34   |   17.25    |
|   10    | Baseline  | 1.44 | Left  | 6.28  | 5.16  |   14.68   |   13.15   |   16.76    |

Example of Wide Format

Long format would look like the data we get in the ANOVA tab, where we
have each column represented by some variable (such as ‘Respiration
State’) and a value (such as ‘Respiration Rate’)

| Subject | Condition |  Leg  | Respiration State | Respiration Rate (pmol/sec/mg) |
|:-------:|:---------:|:-----:|:-----------------:|:------------------------------:|
|    1    | Baseline  | Right |       Basal       |              5.46              |
|    1    | Baseline  | Right |       OctM        |              6.69              |
|    1    | Baseline  | Right |     OctMD(25)     |             10.24              |
|    1    | Baseline  | Right |     OctMD(50)     |             10.12              |
|    1    | Baseline  | Right |    OctMD(250)     |             11.69              |
|    1    | Baseline  | Right |    OctMD(5000)    |             24.44              |
|    1    | Baseline  | Right |      OctMDGS      |             46.35              |
|    1    | Baseline  | Right |       CytC        |             44.69              |
|    1    | Baseline  | Right |     FCCP Peak     |             51.11              |
|    1    | Baseline  | Right |        Omy        |             19.93              |
|    1    | Baseline  | Right |        AmA        |              3.83              |
|    2    | Baseline  | Left  |       Basal       |              6.78              |
|    2    | Baseline  | Left  |       OctM        |              4.94              |
|    2    | Baseline  | Left  |     OctMD(25)     |             10.73              |
|    2    | Baseline  | Left  |     OctMD(50)     |             10.92              |

Example of Long Format

        Sometimes it is more useful to look at the data in wide format,
then convert it to long format for further analysis. For example, it
would be fairly difficult to get a general idea of what the respiration
rates are during Basal respiration if the data are in *long* format.
However, *wide* format makes it much easier for us to see all of the
data that we would want to compare. Furthermore, some analyses (such as
t-tests) need to be done with data in wide format, while others (such as
ANOVAs) need to be done in long format.  
        Although I have already [created a basic Excel
template](#Pulling) that has a tab with the data in long format (which
was quite tedious to do in Excel), it may be useful to understand this
process in R as it is very simple to do and will save quite a bit of
time. The process to go from wide to long format is as simple as:

``` r
Wide_to_Long <- gather(Wide, "Respiration State", 
                          "Respiration Rate (pmol/sec/mg)", 
                          Basal:AmA, factor_key = TRUE)
```

**Gather is now considered out of date and is no longer updated by the
Tidyverse. A more up to date function, `pivot_longer()` will also work:
**

``` r
Wide %>% 
  pivot_longer(-c(Subject:Leg), names_to = "Respiration State", values_to = "Respiration Rate")
```

Both should give us an output of something like:

| Subject | Condition | Mass |  Leg  | Respiration State | Respiration Rate (pmol/sec/mg) |
|:-------:|:---------:|:----:|:-----:|:-----------------:|:------------------------------:|
|    1    | Baseline  | 1.65 | Right |       Basal       |              5.46              |
|    2    | Baseline  | 1.46 | Left  |       Basal       |              6.78              |
|    3    | Baseline  | 1.57 | Right |       Basal       |              4.08              |
|    4    | Baseline  | 1.36 | Left  |       Basal       |              5.66              |
|    5    | Baseline  | 1.38 | Right |       Basal       |              6.73              |
|    6    | Baseline  | 1.42 | Left  |       Basal       |              2.56              |
|    7    | Baseline  | 1.31 | Right |       Basal       |              4.81              |
|    8    | Baseline  | 1.52 | Left  |       Basal       |              5.17              |
|    9    | Baseline  | 1.49 | Right |       Basal       |              5.05              |
|   10    | Baseline  | 1.44 | Left  |       Basal       |              6.28              |
|   11    | Baseline  | 1.48 | Right |       Basal       |              3.76              |
|   12    | Baseline  | 1.31 | Left  |       Basal       |              4.97              |
|   13    | Baseline  | 1.64 | Right |       Basal       |              5.65              |
|   14    | Baseline  | 1.45 | Left  |       Basal       |              6.02              |
|   15    | Baseline  | 1.35 | Right |       Basal       |              6.61              |

Our New Table

## 2.2. Cleaning the Data

        Now that we have our data in R, we need to clean the data. This
includes removing all ‘NA’ values and Cytochrome C (because we usually
do not plot these data and only use it as a quality control variable),
and recoding the data into their proper data variable. These processes
are much simpler to do than they sound..

### 2.2.1. Removing unwanted data

        To remove the unwanted data in long format,[3] we will need to
load the ‘Tidyverse’ package using the command `'library(tidyverse)'`.
To remove all of the Cytochrome C and ‘NA’ values, we will use a couple
of different commands. I find these commands to be fairly intuitive to
follow. With the first command, we will just tell R to remove the
Cytochrome C data from the document. Again, we only do this because we
rarely report them in the actual data, but if you need them you can
ignore that line of code. The second command is very important, as it
will remove all ‘NA’ values from this sheet. This is extremely critical
as you cannot run any statistical analyses with ‘NA’ values in the data
frame.

``` r
#remove cytc and rows with NA (select number of rows to include)#

## Delete every 11th row starting from 8 to remove CytC
data_example_long <- data_example_long %>% dplyr::filter(
  row_number() %% 11 != 8) 

##Remove all NA values##
data_example_long <- na.omit(data_example_long)
```

The first command uses `'dplyr'` to remove every nth row starting at row
x. If we look at the way I have organized the Excel sheet, we will
notice that a value for Thus, in this case, we have chosen to remove
every 11th row starting at row number 8. If you have different rows for
the cytochrome c values, you can modify this accordingly.  
The second piece of code here uses the `'na.omit()'` function to remove
all `'NA'` values in the data frame. If we run the code now, we should
get:

| Subject | Condition | Leg   | Respiration State | Respiration Rate (pmol/sec/mg) |
|--------:|:----------|:------|:------------------|-------------------------------:|
|       1 | Baseline  | Right | Basal             |                       5.462766 |
|       1 | Baseline  | Right | OctM              |                       6.693608 |
|       1 | Baseline  | Right | OctMD(25)         |                      10.240721 |
|       1 | Baseline  | Right | OctMD(50)         |                      10.123791 |
|       1 | Baseline  | Right | OctMD(250)        |                      11.685426 |
|       1 | Baseline  | Right | OctMD(5000)       |                      24.441572 |

First Few Rows of the Data with CytC Removed

This new line now has all of the CytC and NA values removed, just like
we wanted!

### 2.2.2. Recoding the data as correct data variables

Now that we have all of our “unimportant” data removed from our data
frame, we need to gather basic information about our data, namely the
type of data variables we have — such as characters (chr), numeric
(num), logicals(logi), integers (int) and factors. These are important
to identify as each of these describes how R will handle the data during
an analysis. For example, we can make groups by identifying some data as
‘Factors’. When we pull our data from Excel into R, R cannot identify
these without us specifically identifying each type of data. We would
not be able to run any analysis without first doing this because R would
label them all as characters or something that wouldn’t be of use to us.
Thus, To show that our data are, in fact, not identified as correct
variables, we can use the ’ `glimpse()'` function (or we can similarly
use the `'str()'` function) as below:

``` r
#Take a look at the data
glimpse(data_example_long)
```

    ## Rows: 540
    ## Columns: 5
    ## $ Subject                          <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2~
    ## $ Condition                        <chr> "Baseline", "Baseline", "Baseline", "~
    ## $ Leg                              <chr> "Right", "Right", "Right", "Right", "~
    ## $ `Respiration State`              <chr> "Basal", "OctM", "OctMD(25)", "OctMD(~
    ## $ `Respiration Rate (pmol/sec/mg)` <dbl> 5.462766, 6.693608, 10.240721, 10.123~

It’s clear here that R does not identify these variables as the correct
data variables. For example, we would ideally want the variables
“Subject”, “Condition”, “Leg”, and “Respiration State” as ‘Factors’
because these are variables we use to identify certain characteristics
such as the specific subject, the baseline or post condition, which leg
we used, and the type of state the sample is placed in. Likewise,
“Respiration Rate” should be a numeric variable (which it is already,
but I will run through the code anyway). Therefore, we should probably
attempt to change these into usable variables for our later analyses.
Basically, we will simply recode these using functions such as
`'as.factor'` and `'as.numeric'`.

``` r
#Recode Subjects as factors#
data_example_long$Subject <- as.factor(data_example_long$Subject)

#Recode Condition as factors#
data_example_long$Condition <- as.factor(data_example_long$Condition)

#Recode Leg as factors#
data_example_long$Leg <- factor(data_example_long$Leg)

#Recode Respiration States as factors#
data_example_long$`Respiration State` <- factor(
  data_example_long$`Respiration State`, levels = 
    c("Basal", "OctM", "OctMD(25)", "OctMD(50)", "OctMD(250)", 
      "OctMD(5000)", "OctMDGS", "FCCP Peak", "Omy", "AmA"))

#Recode Respiration Rate as numeric#
data_example_long$`Respiration Rate (pmol/sec/mg)` <- as.numeric(
        data_example_long$`Respiration Rate (pmol/sec/mg)`)
```

Notice that with the ‘Respiration State’ factor, we must identify the
order of levels so that we can properly order these data in our graphs.
To check if these data are properly identified, we will rerun the
`'glimpse()'` function:

``` r
#Take a look at the data
glimpse(data_example_long)
```

    ## Rows: 540
    ## Columns: 5
    ## $ Subject                          <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2~
    ## $ Condition                        <fct> Baseline, Baseline, Baseline, Baselin~
    ## $ Leg                              <fct> Right, Right, Right, Right, Right, Ri~
    ## $ `Respiration State`              <fct> Basal, OctM, OctMD(25), OctMD(50), Oc~
    ## $ `Respiration Rate (pmol/sec/mg)` <dbl> 5.462766, 6.693608, 10.240721, 10.123~

### 2.2.3. Renaming variables

If you ever need to rename variables, which [happens from time to
time](#Density), you can do so quite simply. If we wanted to rename
‘FCCP Peak’ to just ‘FCCP’ in wide format, we would do:

``` r
#Rename "FCCP Peak" to "FCCP"
names(data_example_wide)[names(data_example_wide)=="FCCP Peak"] <- "FCCP"
```

Or with the long format:

``` r
#Rename FCCP Peak to FCCP
gsub("FCCP Peak", "FCCP", data_example_long$`Respiration State`)
```

Now that it looks like all of the data are properly structured, we can
continue with the rest of the analysis.

------------------------------------------------------------------------

# 3. Glancing at Descriptive Data

        First, I think it would be good to discuss how to get
descriptive data, since they are always reported in manuscripts.
Overall, this is going to be an easy process, and I will detail how to
get the descriptive data in both [wide ad long format](#Wide-Long).

## 3.1. Descriptives in Wide Format (for t-tests)

        First, we must load the data in wide format from the ‘Final Data
t-test’ tab in the Excel file, like so:

``` r
#load readxl
library(readxl)

# Put the excel file into R
data_example_wide <- read_excel(
  "C:\\Users\\Stephen\\OneDrive - University of Massachusetts\\Desktop\\O2K Analysis Files\\Example_FFA_O2K_Analysis.xlsx", 
  sheet = "Final Data t-test", col_names = TRUE)
```

And we should see something like this:

| Subject | Condition |     Mass | Leg   |    Basal |      OctM | OctMD(25) | OctMD(50) |
|--------:|:----------|---------:|:------|---------:|----------:|----------:|----------:|
|       1 | Baseline  | 1.645792 | Right | 5.462766 |  6.693608 | 10.240721 | 10.123791 |
|       2 | Baseline  | 1.460479 | Left  | 6.780756 |  4.944946 | 10.732114 | 10.920338 |
|       3 | Baseline  | 1.569463 | Right | 4.075887 |  7.431547 |  8.161259 |  9.923520 |
|       4 | Baseline  | 1.361509 | Left  | 5.655240 |  2.804040 | 10.725778 | 12.552231 |
|       5 | Baseline  | 1.379396 | Right | 6.733825 |  4.048261 |  4.912661 | 13.794053 |
|       6 | Baseline  | 1.416738 | Left  | 2.561933 | 10.463204 |  7.998249 | 11.559943 |
|       7 | Baseline  | 1.314504 | Right | 4.811316 |  7.220194 |  9.857231 |  6.911969 |
|       8 | Baseline  | 1.523469 | Left  | 5.172572 |  6.858851 | 10.630703 |  9.929866 |
|       9 | Baseline  | 1.489591 | Right | 5.048132 | 10.383764 |  7.372889 | 13.338647 |
|      10 | Baseline  | 1.435664 | Left  | 6.277008 |  5.160304 | 14.681964 | 13.146599 |

The First 10 Rows and the First 8 Columns of the T-test Data (Wide
Format)

Next, we will use the `'describe.by'` function in the `'psych'` package.

``` r
#Load the psych package
library(psych)

#Calculate descriptive data
stat_summary_ttest <- describe.by(data_example_wide[5:15], 
                        data_example_wide$Condition)
```

This will actually give us two (or more, if you have more groups)
tables. To extract the tables individually to look at clean data, we
only need to call each condition we have. For example, if I want to look
at the baseline condition, I only need to type
`'stat_summary_ttest$Baseline'` and the data will be shown as follows:

``` r
#Separate baseline and post data
baseline_desc <- stat_summary_ttest$Baseline
post_desc <- stat_summary_ttest$Post
```

When we look at the data, we should see a lot of information. Mean, SD,
skew, SE, etc. which should be about all of the descriptive information
we need. Below is an abbreviated table (only mean, SD, median, and SE)
with the baseline information:

|             |      mean |       sd |    median |        se |
|:------------|----------:|---------:|----------:|----------:|
| Basal       |  5.427053 | 1.138132 |  5.568316 | 0.2190336 |
| OctM        |  7.126207 | 1.840949 |  7.220194 | 0.3542908 |
| OctMD(25)   |  8.962943 | 2.411911 |  9.024156 | 0.4641726 |
| OctMD(50)   | 11.144782 | 1.756194 | 10.665820 | 0.3379798 |
| OctMD(250)  | 13.822201 | 2.422383 | 13.709489 | 0.4661878 |
| OctMD(5000) | 20.111828 | 3.087684 | 20.415917 | 0.5942251 |
| OctMDGS     | 46.095557 | 4.564437 | 45.376760 | 0.8784263 |
| CytC        | 45.785762 | 4.901915 | 45.288701 | 0.9433740 |
| FCCP Peak   | 54.966209 | 5.101753 | 56.643651 | 0.9818327 |
| Omy         | 20.217749 | 1.499707 | 19.925417 | 0.2886188 |
| AmA         |  4.869418 | 1.054093 |  4.729285 | 0.2028603 |

Baseline Descriptives

And the post intervention information:

|             |      mean |       sd |    median |        se |
|:------------|----------:|---------:|----------:|----------:|
| Basal       |  5.130259 | 1.022558 |  5.327279 | 0.1967913 |
| OctM        |  7.699387 | 1.566232 |  7.548970 | 0.3014215 |
| OctMD(25)   | 10.838484 | 2.018142 | 10.832285 | 0.3883917 |
| OctMD(50)   | 13.224316 | 2.075465 | 13.170326 | 0.3994234 |
| OctMD(250)  | 15.925601 | 2.602422 | 15.639613 | 0.5008364 |
| OctMD(5000) | 27.176415 | 2.856114 | 27.094806 | 0.5496593 |
| OctMDGS     | 57.405501 | 5.988608 | 56.823842 | 1.1525082 |
| CytC        | 57.407019 | 8.032694 | 55.282695 | 1.5458928 |
| FCCP Peak   | 60.094977 | 6.158332 | 60.218216 | 1.1851716 |
| Omy         | 19.818913 | 2.257293 | 19.396050 | 0.4344161 |
| AmA         |  4.928159 | 1.017303 |  5.039913 | 0.1957801 |

Post Intervention Descriptives

## 3.2. Descriptives in Long Format (for ANOVAs)

        Just like I did with the t-test, I will first go over the
process to get descriptive data. Now, for this step your data should be
[in long format](#Wide-Long), which you will need for the main part of
the ANOVA anyway (this data format is the same that I used in the
[preparing section](#Preparing)). We can get all of the same descriptive
information as we did in the previous section by doing the following:

``` r
#Calculate descriptive data
by(data_example_long$`Respiration Rate (pmol/sec/mg)`, 
   list(data_example_long$`Respiration State`, 
        data_example_long$Condition), stat.desc, basic = FALSE)
```

This output is very large, so I won’t include it in this document, but
it does contain all of the same information as the descriptive data we
did in [the t-test section](#DescWide), so you can choose whatever
method is easiest (I prefer the nice tables, so I normally use the other
way).

------------------------------------------------------------------------

# 4. Testing for the Assumptions of Normality & Homogeneity of Variance

        Now, if we remember from our statistics class (or possibly not),
we should recognize that the t-test and ANOVA both have certain
assumptions in order to be properly run. These are:

-   Independence of cases
-   Normality
-   Homogeneity of Variance (or in the case of an ANOVA, sphericity)

If you want to read more on this, [you can do so
here](https://en.wikipedia.org/wiki/Analysis_of_variance#Assumptions).
If either of the last two of these are violated, we must use a
non-parametric test such as a Wilcoxon test (paired samples t-test),
Mann-Whitney test (for independent sample t-tests), Kruskal-Wallis test
(for one-way or two-way ANOVA), or Friedman test (for repeated-measures
ANOVA) instead[(more on this in the t-test section)](#nonparattest). As
I did in the [Preparing the Data](#Preparing) section, I will use the
ANOVA table for simplicity. It seems like running these tests in long
format is smoother than running it in wide format, however, you can use
wide format if you want to use the `'aggregate'` and `'merge'`
functions.

## 4.1. Testing for Normality

        In order to determine the Normality via the Shapiro-Wilk test,
we will need to run something like this:

``` r
#Shapiro Test for Normality#

#Load the data.table package
library(data.table)

#place the data frame into the data.table package format
DT <- data.table(data_example_long)

#perform the Shapiro Test with the new data
shapiro_results <- DT[,
   .(Statistic = shapiro.test(
     `Respiration Rate (pmol/sec/mg)`)$statistic, 
     P.value = shapiro.test(
       `Respiration Rate (pmol/sec/mg)`)$p.value),
   by = .(`Respiration State`)]
```

| Respiration State | Statistic |   P.value |
|:------------------|----------:|----------:|
| Basal             | 0.9794478 | 0.4770998 |
| OctM              | 0.9797068 | 0.4879272 |
| OctMD(25)         | 0.9770911 | 0.3859082 |
| OctMD(50)         | 0.9750888 | 0.3193547 |
| OctMD(250)        | 0.9911574 | 0.9598852 |
| OctMD(5000)       | 0.9776252 | 0.4053726 |
| OctMDGS           | 0.9784966 | 0.4386628 |
| FCCP Peak         | 0.9605528 | 0.0729456 |
| Omy               | 0.9426270 | 0.0120126 |
| AmA               | 0.9789059 | 0.4549408 |

Normality Test Results

From here we now have the table:  
Where the first column is the factor (‘Respiration State’ in our data)
and the second column is the p-value of the Shapiro test. Furthermore,
if we have more than one grouping factor (as we do in this case), we can
simply adjust the code by adding a `` '+ `Factor2`' `` like below:

``` r
#Perform the Shapiro test
shapiro_results2 <- DT[,
   .(Statistic = shapiro.test(
     `Respiration Rate (pmol/sec/mg)`)$statistic, 
     P.value = shapiro.test(
       `Respiration Rate (pmol/sec/mg)`)$p.value),
   by = .(`Respiration State`, `Condition`)]
```

| Respiration State | Condition | Statistic |   P.value |
|:------------------|:----------|----------:|----------:|
| Basal             | Baseline  | 0.9761663 | 0.7675169 |
| OctM              | Baseline  | 0.9810854 | 0.8860044 |
| OctMD(25)         | Baseline  | 0.9559210 | 0.2972321 |
| OctMD(50)         | Baseline  | 0.9662441 | 0.5062776 |
| OctMD(250)        | Baseline  | 0.9867098 | 0.9732267 |
| OctMD(5000)       | Baseline  | 0.9539389 | 0.2667315 |
| OctMDGS           | Baseline  | 0.9596402 | 0.3626799 |
| FCCP Peak         | Baseline  | 0.8693738 | 0.0028554 |
| Omy               | Baseline  | 0.9578791 | 0.3303175 |
| AmA               | Baseline  | 0.9751437 | 0.7403421 |
| Basal             | Post      | 0.9457374 | 0.1686941 |
| OctM              | Post      | 0.9617252 | 0.4042570 |
| OctMD(25)         | Post      | 0.9764865 | 0.7759189 |
| OctMD(50)         | Post      | 0.9776089 | 0.8048172 |
| OctMD(250)        | Post      | 0.9730145 | 0.6828093 |
| OctMD(5000)       | Post      | 0.9656432 | 0.4918167 |
| OctMDGS           | Post      | 0.9712287 | 0.6345559 |
| FCCP Peak         | Post      | 0.9536497 | 0.2625228 |
| Omy               | Post      | 0.9115035 | 0.0247599 |
| AmA               | Post      | 0.9595841 | 0.3616117 |

Normality Test Results by Group

And the output:

Where the first column is the first factor (‘Condition’ in our data),
the second column is the second factor (‘Respiration State’ in our
data), and the third column is the p-value of the Shapiro test. And so
on.

        Now, once we have this data, we can determine if the normality
assumption has been violated by looking at the p-values. If any of the
p-values are significant (as we see in this data), we must assume that
the assumption of normality has been violated and you cannot use an
ANOVA for this test (instead use a non-parametric test).

I will also note that the following function may be used to get the same
results. You can choose either one, but I found the previous code to
also work with the tests for Variance below. I have only included it for
reference.

``` r
#Load rstatix
library(rstatix)

shapiro_aggregate <- aggregate(formula = `Respiration Rate (pmol/sec/mg)` ~ 
            `Respiration State` + `Condition`,
          data = data_example_long,
          FUN = function(x) {y <- shapiro.test(x); c(y$p.value)})
```

| Respiration State | Condition | Respiration Rate (pmol/sec/mg) |
|:------------------|:----------|-------------------------------:|
| Basal             | Baseline  |                      0.7675169 |
| OctM              | Baseline  |                      0.8860044 |
| OctMD(25)         | Baseline  |                      0.2972321 |
| OctMD(50)         | Baseline  |                      0.5062776 |
| OctMD(250)        | Baseline  |                      0.9732267 |
| OctMD(5000)       | Baseline  |                      0.2667315 |
| OctMDGS           | Baseline  |                      0.3626799 |
| FCCP Peak         | Baseline  |                      0.0028554 |
| Omy               | Baseline  |                      0.3303175 |
| AmA               | Baseline  |                      0.7403421 |
| Basal             | Post      |                      0.1686941 |
| OctM              | Post      |                      0.4042570 |
| OctMD(25)         | Post      |                      0.7759189 |
| OctMD(50)         | Post      |                      0.8048172 |
| OctMD(250)        | Post      |                      0.6828093 |
| OctMD(5000)       | Post      |                      0.4918167 |
| OctMDGS           | Post      |                      0.6345559 |
| FCCP Peak         | Post      |                      0.2625228 |
| Omy               | Post      |                      0.0247599 |
| AmA               | Post      |                      0.3616117 |

Normality Test Results Using the ‘aggregate()’

## 4.2. Testing for Variance

        For testing the homogeneity of variance among the samples using
the F-test, we will use this line of code similar to the one above:

``` r
#Perform the F-test
variance_results <- DT[,
   .(Statistic = var.test(
     `Respiration Rate (pmol/sec/mg)` ~ `Condition`)$statistic, 
     P.value = var.test(
       `Respiration Rate (pmol/sec/mg)`~ `Condition`)$p.value),
   by = .(`Respiration State`)]
```

| Respiration State | Statistic |   P.value |
|:------------------|----------:|----------:|
| Basal             | 1.2388242 | 0.5890068 |
| OctM              | 1.3815650 | 0.4154088 |
| OctMD(25)         | 1.4282990 | 0.3692806 |
| OctMD(50)         | 0.7160024 | 0.3999891 |
| OctMD(250)        | 0.8664231 | 0.7174246 |
| OctMD(5000)       | 1.1687315 | 0.6939379 |
| OctMDGS           | 0.5809284 | 0.1728032 |
| FCCP Peak         | 0.6862978 | 0.3432272 |
| Omy               | 0.4414051 | 0.0415944 |
| AmA               | 1.0736354 | 0.8576298 |

Variance Test Results

Or we can use Levene’s test[4], which is more robust to deviations of
normality, as follows:

``` r
#Load rstatix
library(rstatix)

#Perform Levene's test
levene_results <- DT %>%
  group_by(`Respiration State`) %>%
  levene_test(`Respiration Rate (pmol/sec/mg)` ~ Condition)
```

| Respiration State | df1 | df2 | statistic |         p |
|:------------------|----:|----:|----------:|----------:|
| Basal             |   1 |  52 | 0.1984678 | 0.6578097 |
| OctM              |   1 |  52 | 0.1339030 | 0.7159036 |
| OctMD(25)         |   1 |  52 | 0.9405234 | 0.3366327 |
| OctMD(50)         |   1 |  52 | 0.4194379 | 0.5200669 |
| OctMD(250)        |   1 |  52 | 0.0090460 | 0.9245925 |
| OctMD(5000)       |   1 |  52 | 0.0046024 | 0.9461725 |
| OctMDGS           |   1 |  52 | 0.7460983 | 0.3916812 |
| FCCP Peak         |   1 |  52 | 0.0667484 | 0.7971512 |
| Omy               |   1 |  52 | 1.5100906 | 0.2246575 |
| AmA               |   1 |  52 | 0.1271492 | 0.7228493 |

Levene’s Test Results

        We only need to run this code to compare the two conditions
(Baseline vs Post) within each Respiration State because the goal of the
F-statistic is to make sure that the variance between two variables is
equal. Since, in this case, the two variables we would compare would be
the Baseline and Post values, we only need to compare those.  
        Ideally, we should not see any significant values in our tests
we have run above. Though, as you can see, we have a few variables that
have kicked back a couple of significant p-values. There are a few
options, in my opinion, that can be done at this point:

-   You can perform a non-parametric test. This can be done, however,
    you will most likely lose some power in your further analyses.
-   You can ignore the significant values. There are several reasons for
    coming to this conclusion based on these data, but the biggest
    reason is that we have performed several comparisons, and thus have
    accumulated a greater chance of a type-I error, or, a false
    positive. If we consider that the probability of a variable becoming
    significant due to random chance increases as we perform more
    comparisons, then we must consider that some of these p-values must
    be subject to the same randomness. Therefore, if we apply a
    correction for this randomness (such as a Bonferroni or Tukey
    correction), our values are clearly no longer significant.

        I would argue for the second case in these random data I have
created for the sake of this manuscript (and that the data that have
been identified are not of extreme importance to these data, but that
itself is a weak argument). However, I strongly encourage each
individual data set be properly analyzed for these parameters.

## 4.3. Further Considerations

        It is important to acknowledge the limitations above. First, I
am assuming you will only be comparing 2 factors (Baseline vs Post),
rather than multiple factors. If you wish to compare more than 2 factors
(e.g. Young, Middle-Age, and Elderly), you can perform Levene’s test
like above (though I will add I haven’t tested the code yet, but it may
be done in one of my projects soon).

### 4.3.1. Borwn-Forsythe Test

        Furthermore, if you wish to change the center of the Levene Test
from ‘mean’ (the default parameter) to ‘median’ (also known as a
Brown-Forsythe Test), simply add `center = median` to the code as
follows:

``` r
library(rstatix)

BrownForsythe_results <- DT %>%
  group_by(`Respiration State`) %>%
  levene_test(`Respiration Rate (pmol/sec/mg)` ~ Condition, center = median)
```

| Respiration State | df1 | df2 | statistic |         p |
|:------------------|----:|----:|----------:|----------:|
| Basal             |   1 |  52 | 0.1984678 | 0.6578097 |
| OctM              |   1 |  52 | 0.1339030 | 0.7159036 |
| OctMD(25)         |   1 |  52 | 0.9405234 | 0.3366327 |
| OctMD(50)         |   1 |  52 | 0.4194379 | 0.5200669 |
| OctMD(250)        |   1 |  52 | 0.0090460 | 0.9245925 |
| OctMD(5000)       |   1 |  52 | 0.0046024 | 0.9461725 |
| OctMDGS           |   1 |  52 | 0.7460983 | 0.3916812 |
| FCCP Peak         |   1 |  52 | 0.0667484 | 0.7971512 |
| Omy               |   1 |  52 | 1.5100906 | 0.2246575 |
| AmA               |   1 |  52 | 0.1271492 | 0.7228493 |

Brown-Forsythe Test Results

### 4.3.2. Mauchly’s Test for Sphericity

        Similarly, if you are running a [repeated-measures ANOVA
(discussed later)](#Repeated), you should use Mauchly’s test in the as
follows:

``` r
#Load the package ez
library('ez')

#Set factor levels for Respiration State
DT$State<- factor(
  data_example_long$`Respiration State`, levels = 
    c("Basal", "OctM", "OctMD(25)", "OctMD(50)", "OctMD(250)", 
      "OctMD(5000)", "OctMDGS", "FCCP Peak", "Omy", "AmA"))

#Recode Respiration Rate as numeric#
DT$Rate <- as.numeric(
        data_example_long$`Respiration Rate (pmol/sec/mg)`)

#Run the ANOVA to get Mauchly's Test
ezANOVA(data = DT, dv = .(Rate), wid = .(Subject), within = .(State), 
        between = .(Condition), detailed = TRUE, type = 3)
```

Please noice that in order to get this done, I had to recode the
variables into single word variables (`State` and `Rate` from
`Respiration State` and `Respiration Rate (pmol/sec/mg)`, respectively).
I don’t know why the code won’t recognize the longer strings, but this
is the fix.

### 4.3.3. Fligner-Killeen Test

        Lastly, if your data are not normally distributed, you cannot
perform an F-test or Levene’s test; instead you must perform the
Fligner-Killeen test as follows:

``` r
FlingerKilleen_results <- DT[,
   .(Statistic = fligner.test(
     `Respiration Rate (pmol/sec/mg)` ~ `Condition`)$statistic, 
     P.value = fligner.test(
       `Respiration Rate (pmol/sec/mg)`~ `Condition`)$p.value),
   by = .(`Respiration State`)]
```

| Respiration State | Statistic |   P.value |
|:------------------|----------:|----------:|
| Basal             | 0.3124012 | 0.5762104 |
| OctM              | 0.1609559 | 0.6882780 |
| OctMD(25)         | 1.0642464 | 0.3022487 |
| OctMD(50)         | 0.4215301 | 0.5161746 |
| OctMD(250)        | 0.0000060 | 0.9980419 |
| OctMD(5000)       | 0.0012104 | 0.9722465 |
| OctMDGS           | 0.6881606 | 0.4067907 |
| FCCP Peak         | 0.0317386 | 0.8586027 |
| Omy               | 1.1303701 | 0.2876964 |
| AmA               | 0.2485103 | 0.6181260 |

Fligner-Killeen Test Results

This should be about all of the tests you will need to properly test
assumptions for your data. Once you determine the proper test, we can
move on to further analysis.

------------------------------------------------------------------------

# 5. Hypothesis Testing

        Now that we have looked into the assumptions, we can move on to
the next step: running comparisons. These should all be fairly
straightforward, but can get tricky depending on what test you want to
perform. Overall, doing a basic t-test and ANOVA will be simple, but
things will get more complex if you wish to do a planned comparison
ANOVA or something a little more complex than the basic tests (I will
hopefully try to cover that at some point). For now, let’s dive into the
t-test.

## 5.1. Hypothesis Testing with Two Variables

### 5.1.1. The t-test

        The t-test analysis is fairly easy to run.[5] Lucky for us, R
comes with a t-test function and an output that is easy to understand:

``` r
t.test(y ~ x)
```

Where y is a numeric value and x is a binary (group) value. Pretty
simple, right? You can also do:

``` r
t.test(y1,y2)
```

Where each y is a numeric value. For paired samples (data with the same
subjects tested twice, as in a pre- & post-measurement), you simply need
to add a `'paired = TRUE'` line like this:

``` r
t.test(y ~ x, paired = TRUE)
```

Now, since we are going to run multiple paired t-tests (remember this is
paired data) to analyze each respiration state, we can simply use the
`'lapply'` and `'sapply'` functions in the `'tidyr'` package, and we
will place this information in new data frames so we can have a much
simpler time looking at the data as so:

``` r
#load dplyr
library(dplyr)

# Run multiple paired t-tests
FFA_ttest_values <- lapply(data_example_wide[,5:15], 
  function(x) t.test(x ~ data_example_wide$Condition, 
  paired = TRUE, na.rm = TRUE))

# Place p-values into columns
FFA_ttest_pvalues <- data.frame("P.value" = sapply(
  FFA_ttest_values, getElement, name = "p.value"))
```

The `'lapply'` function serves as a sort of loop that takes the
specified columns (in this case, columns 5-15) and runs the function of
choice (in this case, the t-test). The `'~'` symbol, when using it in a
function of, can be thought to mean “as a function of.” Essentially, we
are taking columns 5-15 of our data, and applying a t-test to the data
in every column ‘x’ as a function of the Condition. These are paired
samples, and we want to remove all `'NA'` data points. Furthermore, we
have placed the analysis in a new data frame ‘FFA_ttest_values,’ which
is slightly messy. To clean this up, we will take the data frame
‘FFA_ttest_values’ and extract the p-values using `'sapply'` and place
them in the new data frame ‘FFA_ttest_pvalues.’[6] The output will give
us the new data frame `'FFA_ttest_pvalues'` which should look something
like:

|             |   P.value |
|:------------|----------:|
| Basal       | 0.3584644 |
| OctM        | 0.2050634 |
| OctMD(25)   | 0.0003312 |
| OctMD(50)   | 0.0002692 |
| OctMD(250)  | 0.0043414 |
| OctMD(5000) | 0.0000000 |
| OctMDGS     | 0.0000000 |
| CytC        | 0.0000003 |
| FCCP Peak   | 0.0013976 |
| Omy         | 0.4659901 |
| AmA         | 0.8056960 |

A Table of the P-Values from the T-test

Short and sweet, if you ask me. Really, you don’t need to create a data
frame with only the p-values, but I find that this really helps keep the
data organized and put into place. If you want to do a correction for
multiple comparisons, the use the following:

``` r
#Correct for multiple comparisons
FFA_ttest_pvalues$`Adjusted p` <- p.adjust(FFA_ttest_pvalues$P.value, 
  method = "holm")
```

I have chosen to use the Holm adjustment, which will adjust for the p
value and give us:

|             |   P.value | Adjusted p |
|:------------|----------:|-----------:|
| Basal       | 0.3584644 |  1.0000000 |
| OctM        | 0.2050634 |  0.8202536 |
| OctMD(25)   | 0.0003312 |  0.0023182 |
| OctMD(50)   | 0.0002692 |  0.0021538 |
| OctMD(250)  | 0.0043414 |  0.0217070 |
| OctMD(5000) | 0.0000000 |  0.0000000 |
| OctMDGS     | 0.0000000 |  0.0000002 |
| CytC        | 0.0000003 |  0.0000023 |
| FCCP Peak   | 0.0013976 |  0.0083854 |
| Omy         | 0.4659901 |  1.0000000 |
| AmA         | 0.8056960 |  1.0000000 |

Adjusted P-Values Using the Holm Method

For more information on this code or for other adjustments that can be
made, [check out the R Companion book (also in the book section
below)](https://rcompanion.org/rcompanion/f_01.html).

### 5.1.2. Non-parametric tests for two variables

        Below are non-parametric tests that can be used to analyze data
that violate the [assumptions discussed prior](#AssumptionTests).

**Mann-Whitney U Tests**

        The Mann-Whitney U test (also known as the Wilcoxon rank-sum
test, not to be confused with the Wilcoxon signed-rank test discussed
next) is the non-parametric equivalent of an independent samples t-test.
This test follows the basic code outline of the t-test, instead you will
use:

``` r
wilcox.test()
```

So, following our example above, we would do:

``` r
# Run multiple Mann-Whitney tests
FFA_mannwhit_values <- lapply(data_example_wide[,5:15], 
  function(x) wilcox.test(x ~ data_example_wide$Condition, 
  na.rm = TRUE))

# Place p-values into columns
FFA_mannwhit_pvalues <- data.frame("P value" = sapply(
  FFA_mannwhit_values, getElement, name = "p.value"))
```

And here, we see:

|             |   P.value |
|:------------|----------:|
| Basal       | 0.3019111 |
| OctM        | 0.2859841 |
| OctMD(25)   | 0.0052874 |
| OctMD(50)   | 0.0002554 |
| OctMD(250)  | 0.0077971 |
| OctMD(5000) | 0.0000000 |
| OctMDGS     | 0.0000000 |
| CytC        | 0.0000000 |
| FCCP Peak   | 0.0021634 |
| Omy         | 0.2706253 |
| AmA         | 0.7574251 |

P-Values from the Mann-Whitney Test

**Wilcoxon Signed-Rank Test**

        The Wilcoxon Signed-Rank Test is the non-parametric equivalent
of the paired t-test. As we did with the t-test, all we need to do to
make this happen is add `'paired = TRUE'` to the Mann-Whitney code
above, like so:

``` r
# Run multiple Mann-Whitney tests
FFA_wilcoxon_values <- lapply(data_example_wide[,5:15], 
  function(x) wilcox.test(x ~ data_example_wide$Condition, 
  paired = TRUE, na.rm = TRUE))

# Place p-values into columns
FFA_wilcoxon_pvalues <- data.frame("P value" = sapply(
  FFA_wilcoxon_values, getElement, name = "p.value"))
```

And here, we see:

|             |   P.value |
|:------------|----------:|
| Basal       | 0.4553177 |
| OctM        | 0.2483380 |
| OctMD(25)   | 0.0003807 |
| OctMD(50)   | 0.0004787 |
| OctMD(250)  | 0.0082151 |
| OctMD(5000) | 0.0000000 |
| OctMDGS     | 0.0000004 |
| CytC        | 0.0000020 |
| FCCP Peak   | 0.0012526 |
| Omy         | 0.1854744 |
| AmA         | 0.9528932 |

P-Values from the Wilcoxon Test

## 5.2. Hypothesis Testing with More Than Two Variables

        Now, as [I mentioned before](#Stats), I think the ANOVA is the
most appropriate test to run for these data. To me, it is pretty clear
you would want an F-value to control for Type I (false positive) errors
by performing too many tests. As [shown above](#Ttestmain), we would be
performing 10 different t-tests. If we assume that
![FWE \\le 1 - (1 - \\alpha\_{IT})^{c}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;FWE%20%5Cle%201%20-%20%281%20-%20%5Calpha_%7BIT%7D%29%5E%7Bc%7D "FWE \le 1 - (1 - \alpha_{IT})^{c}"),
where
*![\\alpha\_{IT}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Calpha_%7BIT%7D "\alpha_{IT}")*
is the alpha level of a given test and *c* is the number of comparisons,
we would get the following
![FWE \\le 1 - (1 - 0.05)^{10}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;FWE%20%5Cle%201%20-%20%281%20-%200.05%29%5E%7B10%7D "FWE \le 1 - (1 - 0.05)^{10}")
which gives us a final value of
![0.401](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;0.401 "0.401").
**This means that the probability of a type I error is 0.401, or just
over 40%.** I think it would go without saying that this would be a very
high chance of a type I error in these data, hence the need for an ANOVA
in our analysis if we want to maintain confidence in our results.

### 5.2.1. The ANOVA

        I’m going to preface this analysis by saying that, under normal
circumstances, the ANOVA should be performed using independent samples;
meaning the ANOVA is an extension of the independent samples t-test and
the subjects being compared should not be the same in any two groups
[(this is covered in the next section)](#Repeated). The point of this
statement is to mention that the example data I will use in this section
should be analyzed via a repeated-measures ANOVA,[7] not the traditional
ANOVA. Therefore, this section is meant to merely demonstrate the basics
of how to run a traditional ANOVA.

        If you remember, I briefly covered the code [in a previous
section](#ANOVA1), but I will go into more details here. Simply put, the
ANOVA analysis has two steps:  
        1. Create a linear model of the data  
        2. Run the ANOVA on the linear model

While there is the second step, it isn’t hard to implement in this
process and is extremely straightforward to code. First, though, your
data ***must be in long format.*** I cannot stress this enough, simply
because this is how most other statistical software does ANOVAs and R is
no different in that respect. Fortunately, the data in the ANOVA tab
within the [Excel file I have created](#Pulling) is in long format.
However, if you are using your own set of data and you need to do this
or you need more explanation on what I mean by long and wide format, you
can [read more about it here.](#Wide-Long)

        By now, you’ve hopefully [identified all of your variables
properly](#Correcting) in their proper columns. If you haven’t, please
do so first before running the ANOVA. Once we have that, we should have
a table that looks something like this:

| Subject | Condition | Leg   | Respiration State | Respiration Rate (pmol/sec/mg) |
|:--------|:----------|:------|:------------------|-------------------------------:|
| 1       | Baseline  | Right | Basal             |                       5.462766 |
| 1       | Baseline  | Right | OctM              |                       6.693608 |
| 1       | Baseline  | Right | OctMD(25)         |                      10.240721 |
| 1       | Baseline  | Right | OctMD(50)         |                      10.123791 |
| 1       | Baseline  | Right | OctMD(250)        |                      11.685426 |
| 1       | Baseline  | Right | OctMD(5000)       |                      24.441572 |
| 1       | Baseline  | Right | OctMDGS           |                      46.353204 |
| 1       | Baseline  | Right | FCCP Peak         |                      51.111261 |
| 1       | Baseline  | Right | Omy               |                      19.925417 |
| 1       | Baseline  | Right | AmA               |                       3.831640 |
| 2       | Baseline  | Left  | Basal             |                       6.780756 |
| 2       | Baseline  | Left  | OctM              |                       4.944946 |
| 2       | Baseline  | Left  | OctMD(25)         |                      10.732114 |
| 2       | Baseline  | Left  | OctMD(50)         |                      10.920338 |
| 2       | Baseline  | Left  | OctMD(250)        |                      10.722428 |

The First 15 Rows of Data in Long Format for the ANOVA

Now, as I mentioned before, we need to first create a linear model of
the data using the `'aov()'` function, then we can analyze the linear
model using the `'anova()'` function. Let’s start by performing an ANOVA
examining the effect of the Condition (baseline vs post intervention)
and Respiration State on Respiration Rate,[8] as so:

``` r
#Create the linear model
ANOVA_model<- aov(`Respiration Rate (pmol/sec/mg)` ~ 
        Condition + `Respiration State`, data = data_example_long, type = "II")

#Analyze the linear model
ANOVA_table <- as.data.frame(anova(ANOVA_model))
```

For context, We should see an output similar to something like this:

|                     |  Df |    Sum Sq |  Mean Sq | F value | Pr(>F) |
|:--------------------|----:|----------:|---------:|--------:|-------:|
| Condition           |   1 |   1174.68 |  1174.68 |   94.31 |      0 |
| `Respiration State` |   9 | 173710.60 | 19301.18 | 1549.64 |      0 |
| Residuals           | 529 |   6588.84 |    12.46 |      NA |     NA |

This table is fairly intuitive to grasp.[9] In the columns, you have
your degrees of freedom, sum of squares, mean squares, F values, and
lastly p-values. The rows represent the values for Factor A, Factor B,
(in this case, Condition and Respiration State, respectively),, and the
Residuals. You’ll notice that in my example, the p-values are labeled as
0. This is because the p values are so low (probably due to the way I
have arranged the random data I created from thin air) that the tables
just rounded them to 0. If I inspect the column by itself, the p-values
are actually:

    ##                        Pr(>F)    
    ## Condition           < 2.2e-16 ***
    ## `Respiration State` < 2.2e-16 ***
    ## Residuals                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

        Based on this analysis, we can see that our results are indeed
very significant. It appears that there is a ‘Condition’ effect, meaning
that the post-intervention had some effect on our subjects. Personally,
I don’t care much about the p-value from the Respiration State effect,
since we would expect respiration rates to be different when we add
different substrates. I didn’t include an interaction in this analysis
because it wouldn’t add any meaning to the data, but if you wanted to do
so, simply change the `'+'` to a `'*'` in the equation like so:

``` r
aov(`Respiration Rate (pmol/sec/mg)` ~ 
        Condition * `Respiration State`, data = data_example_long, type = "II")
```

        Lastly, I should mention the ability to run different types of
sum of squares analyses in these functions. There are 3 types of sum of
squares analyses [(more on that
here)](https://towardsdatascience.com/anovas-three-types-of-estimating-sums-of-squares-don-t-make-the-wrong-choice-91107c77a27a):

1.  **Type I Sum of Squares** performs the sum of squares analysis in
    sequential order, assigning a maximum value to Factor A, then the
    remaining variation to Factor B, then the interaction (if present),
    then the residual. In other words, this test gradually adds in
    factors with each analysis. Annotated, it looks like this:  
    SS(A) for Factor A.  
    SS(B \| A) for Factor B.  
    SS(AB \| A, B) for interaction AB.  
    In this case, the ordering of the model makes a big difference in
    the result. In this case, it is unwise to use this unless the main
    effects are all completely independent of each other, which is not
    the case for our data.

2.  **Type II Sum of Squares** is not sequential like the Type I Sum of
    Squares, but it also does not take the interaction into effect and
    tests each main effect after the other. Annotated, it looks like:  
    SS(A \| B) for Factor A.  
    SS(B \| A) for Factor B.  
    This type or analysis is beneficial if you care most about the main
    effects (which we do here) rather than the interaction effects, or
    if there is no interaction effect.

3.  **Type III Sum of Squares** are not sequential, like Type II Sum of
    Squares, but *do* take into account the interaction effects. The
    annotation looks like:  
    SS(A \| B, AB) for Factor A.  
    SS(B \| A, AB) for Factor B.  
    This type is useful if you don’t want an ordering effect, and expect
    an interaction effect. Also, this type is ***the only one that
    should be used if you have unequal sample sizes.*** Types I and II
    should not be used if your groups have unequal sample sizes.

        To change the type of Sum of Squares analysis, simply change the
`'type = '` line to the desired test. Overall, this process is all
fairly easy to comprehend.

### 5.2.2. Repeated-Measures and Mixed Design ANOVAs

        The Repeated-Measures and Mixed Design ANOVAs are slightly more
complicated to understand and implement. The best way I try to remember
this is:

-   **ANOVAs** (factorial or not) are used with *independent samples,*
    and are extensions of *independent t-tests.*
-   **Repeated-Measures ANOVAs** are used with *paired samples,* and are
    extensions of *paired t-tests.*
-   **Mixed Design ANOVAs** are for *any combination of independent and
    dependent samples.*

In essence, each ANOVA is designed to answer a different question, just
like the independent and paired samples t-tests. In [the last
section](#ANOVAmain), the data we would analyze would be considered to
be a part of a measurement of independent samples. Now, with the
repeated-measures ANOVA, we will analyze data from the same subjects
across conditions, like time. To do this, I found that we first need to
actually change the name of some of our variables so that the variable
names are a single word[10], like so:

``` r
#Set the rate as a numeric and the state as a factor variable
data_example_long$Rate <- as.numeric(
  data_example_long$`Respiration Rate (pmol/sec/mg)`)

data_example_long$State <- as.factor(
  data_example_long$`Respiration State`)
```

I have changed the names of the ‘Respiration Rate’ and ‘Respiration
State’ variables to ‘Rate’ and ‘State’ respectively. Now that we have
that, we can use the `'ezANOVA'` function in the `'ez'` package as so:

``` r
#Load the ez package
library(ez)

#Run the repeated measures ANOVA
repeated_measures <- ezANOVA(data = data_example_long, dv = .(Rate), wid = .(Subject), 
        within = .(Condition, State), type = "III", detailed = TRUE)
```

This function is a little different than the others. Essentially, we are
choosing the data frame ‘data_example_long’, our dependent variable (dv)
is ‘Rate,’ the within-subjects identifier (wid) is ‘Subject,’ and the
two within-subjects conditions are ‘Condition’ and ‘State.’ ***It is
important to have the ‘.()’ in the code with each parameter.*** The code
will not work if those are missing. Lastly, I have chosen a type III sum
of squares analysis and a detailed output. When we run this, we will see
an output like so:

| Effect          | DFn | DFd |        SSn |      SSd |         F |   p | p\<.05 |   ges |
|:----------------|----:|----:|-----------:|---------:|----------:|----:|:-------|------:|
| (Intercept)     |   1 |  26 | 232488.017 |  284.264 | 21264.336 |   0 | \*     | 0.980 |
| Condition       |   1 |  26 |   1174.683 |  234.874 |   130.035 |   0 | \*     | 0.195 |
| State           |   9 | 234 | 173710.599 | 2334.535 |  1934.636 |   0 | \*     | 0.973 |
| Condition:State |   9 | 234 |   1754.453 | 1980.710 |    23.030 |   0 | \*     | 0.266 |

Repeated-Measures ANOVA Results

|     | Effect          |     W |   p | p\<.05 |
|:----|:----------------|------:|----:|:-------|
| 3   | State           | 0.003 |   0 | \*     |
| 4   | Condition:State | 0.001 |   0 | \*     |

Sphericity Results (Repeated-Measures)

|     | Effect          |   GGe | p\[GG\] | p\[GG\]\<.05 |   HFe | p\[HF\] | p\[HF\]\<.05 |
|:----|:----------------|------:|--------:|:-------------|------:|--------:|:-------------|
| 3   | State           | 0.438 |       0 | \*           | 0.526 |       0 | \*           |
| 4   | Condition:State | 0.401 |       0 | \*           | 0.474 |       0 | \*           |

Sphericity Corrections (Repeated-Measures)

Now, we have a much different layout than we did before, but it follows
the same pattern. We first have the degrees of freedom values, sums of
squares, F-value, p-value, a column with asterisks representing
significance, and finally we have a general eta-squared [(more on this
in the Effect Size section)](#EffectSize). The next set of rows are the
results of Mauchly’s Test for Sphericity, followed by the corrections if
sphericity has been violated. In this case, if sphericity has been
violated (indicated by significance in Mauchly’s test), then it is best
to use the Greenhouse-Geisser correction (GGe column) and the p-values
from the p\[GG\] column. Here, we see that there was a significant
difference between baseline and post-intervention, a difference
somewhere in the respiration state, and an interaction effect.

        Overall this is fairly straightforward, but it is different than
the past tests we did. Luckily, the `'ezANOVA'` function is fairly
robust and we don’t need to change much if we want to do a mixed designs
ANOVA. All we would have to add is a between factor, as so:

``` r
#Run the mixed design ANOVA
mixed_design <- ezANOVA(data = data_example_long, dv = .(Rate), wid = .(Subject), 
        between = .(Leg), within = .(Condition, State), 
        type = "III", detailed = TRUE)
```

|     | Effect              | DFn | DFd |        SSn |      SSd |         F |     p | p\<.05 |   ges |
|:----|:--------------------|----:|----:|-----------:|---------:|----------:|------:|:-------|------:|
| 1   | (Intercept)         |   1 |  25 | 232228.755 |  281.471 | 20626.326 | 0.000 | \*     | 0.980 |
| 2   | Leg                 |   1 |  25 |      2.793 |  281.471 |     0.248 | 0.623 |        | 0.001 |
| 3   | Condition           |   1 |  25 |   1164.276 |  222.809 |   130.636 | 0.000 | \*     | 0.197 |
| 5   | State               |   9 | 225 | 173494.190 | 2318.108 |  1871.076 | 0.000 | \*     | 0.973 |
| 4   | Leg:Condition       |   1 |  25 |     12.065 |  222.809 |     1.354 | 0.256 |        | 0.003 |
| 6   | Leg:State           |   9 | 225 |     16.427 | 2318.108 |     0.177 | 0.996 |        | 0.003 |
| 7   | Condition:State     |   9 | 225 |   1735.376 | 1916.122 |    22.642 | 0.000 | \*     | 0.268 |
| 8   | Leg:Condition:State |   9 | 225 |     64.588 | 1916.122 |     0.843 | 0.578 |        | 0.013 |

Mixed Design ANOVA Results

|     | Effect              |     W |   p | p\<.05 |
|:----|:--------------------|------:|----:|:-------|
| 5   | State               | 0.003 |   0 | \*     |
| 6   | Leg:State           | 0.003 |   0 | \*     |
| 7   | Condition:State     | 0.001 |   0 | \*     |
| 8   | Leg:Condition:State | 0.001 |   0 | \*     |

Sphericity Results (Mixed Design)

|     | Effect              |   GGe | p\[GG\] | p\[GG\]\<.05 |   HFe | p\[HF\] | p\[HF\]\<.05 |
|:----|:--------------------|------:|--------:|:-------------|------:|--------:|:-------------|
| 5   | State               | 0.436 |   0.000 | \*           | 0.527 |   0.000 | \*           |
| 6   | Leg:State           | 0.436 |   0.947 |              | 0.527 |   0.966 |              |
| 7   | Condition:State     | 0.396 |   0.000 | \*           | 0.470 |   0.000 | \*           |
| 8   | Leg:Condition:State | 0.396 |   0.491 |              | 0.470 |   0.506 |              |

Sphericity Corrections (Mixed Design)

Of course, here we will have a much larger set of tables because we have
introduced another factor (Leg) to our analysis, but everything is the
same as the repeated-measures ANOVA. Here, our results show the same
outcomes as before (significant differences in condition and
respiration), but the leg that the sample came from seemed to have no
impact on respiration rate.

### 5.2.3. Non-parametric tests for more than two samples

        When performing non-parametric equivalents for data with
multiple groups, there are two tests we can do: the Kruskal-Wallis test
(the equivalent to a one-way ANOVA). **These two tests do not perform
two-way tests. For two-way non-parametric analyses, there are some tests
available in the `WRS2` package, but I am much less familiar with
these.** The overall processes are very similar to what we did for the
parametric tests. For the Kruskal-Wallis test, we will simply use the
following:

``` r
kruskal.test(data = data_example_long, `Respiration Rate (pmol/sec/mg)` ~ `Condition`)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  Respiration Rate (pmol/sec/mg) by Condition
    ## Kruskal-Wallis chi-squared = 3.2178, df = 1, p-value = 0.07284

### 5.2.4. *Post-hoc* Methods

        Once you’re done with the ANOVA analysis, you’ll probably want
to perform a *post hoc* analysis to determine **where** exactly the
significant differences are (remember, ANOVA only tells you that there
is a good chance that a significant difference exists in the data, but
does not tell you which values are significant). Fortunately, it’s also
pretty simple to run all of these.

**Pairwise Tests**

        To perform Tukey’s HSD for the factorial ANOVA, all we need to
do is take the ANOVA model we made previously and put it into the
`'lsmeans'` function from the `'lsmeans'` package, as so:

``` r
#Load the package lsmeans
library(lsmeans)

#Calculate the Tukey corrections
tukey_results<- lsmeans(ANOVA_model, pairwise ~ `Respiration State`:Condition, 
        adjust = "tukey", paired = TRUE)

#Show the first 10 results of each section
head(tukey_results$lsmeans)
```

    ##  Respiration State Condition lsmean    SE  df lower.CL upper.CL
    ##  Basal             Baseline     3.8 0.504 529     2.81     4.79
    ## 
    ## Confidence level used: 0.95

``` r
head(tukey_results$contrasts)
```

    ##  contrast                       estimate    SE  df t.ratio p.value
    ##  Basal Baseline - OctM Baseline    -2.13 0.679 529  -3.142  0.0018

This table is extremely long, especially considering the huge number of
contrasts we have performed. Therefore, I will not include the output,
but it should be fairly self-explanatory if you’ve ever done a
*post-hoc* analysis in the past. You can also do other adjustments, such
as the Scheffe or Sidak adjustments by typing in `"scheffe"` or
`"sidak"` instead of `"tukey"`. The nice thing about this method is that
it gives you the lower and upper confidence levels. However, it is
slightly more difficult to interpret. If you don’t care about the
confidence levels and only want the adjusted p-values, you can use the
following for both factorial and repeated measures ANOVAs:

``` r
#Calculate the Holm correction
holm_results <- pairwise.t.test(x = data_example_long$`Rate`, 
      g = data_example_long$State:data_example_long$Condition, 
      paired = TRUE, p.adjust.method = "holm")
```

As with the other adjustment method, this test can use the Holm,
Hochberg, Hommel, Bonferroni, Benjamini-Hochberg or its alias FDR, or
the Benjamini-Yekutieili adjustment by using `"holm"`, `"hochberg"`,
`"hommel"`, `"bonferroni"`, `"BH"`, `"FDR"`, or `"BY"` respectively.
This code can also be done for Wilcoxon tests as so:

``` r
#Wilcoxon Comparison
wilcoxon_results <- pairwise.wilcox.test(x = data_example_long$`Rate`, 
      g = data_example_long$State:data_example_long$Condition, 
      paired = TRUE, p.adjust.method = "holm")
```

### 5.2.5. Planned Comparisons

        Now, running *post-hoc* tests can be slightly tedious and
difficult, especially when there are so many data points that don’t
matter to our story. For example, we would probably not care about the
difference between basal respiration at baseline and respiration with
oligomycin post-intervention; but we would certainly care a lot about
the difference in maximal respiration at baseline compared to
post-intervention. In fact, it would probably be best to compare the
baseline and post-intervention values for all of the respiration states
(like we did with the t-tests). In all probability, it is probably best
to just run the t-tests with a p-value adjustment, but I will try to
show contrasts here. Be warned: they can be complicated.

------------------------------------------------------------------------

# 6. Effect Sizes

        Effect sizes are often useful counterparts to your p-values for
when you want to add more information to your data. Since effect sizes
are best done in long format after doing t-tests, I’ll probably just
write about those for now and add in something about ANOVAs later. The
reason for that is that effect sizes from ANOVAs usually require
contrasts, which I haven’t yet developed.  
       To calculate Cohen’s *d* from the t-test data, simply use the
`'effectsize'` package as so:

``` r
#Load the package effectsize
library(effectsize)

#Calculate ES
effect_sizes <- data.frame(t(sapply(data_example_wide[,5:15], 
        function(x) effectsize::cohens_d(x, y = "Condition", 
        data = data_example_wide, pooled = TRUE))))
```

This is essentially the same function used to do the t-tests, just with
a different function applied to the loop.[11] This gives us the output
as follows:

|             | Cohens_d    | CI   | CI_low     | CI_high    |
|:------------|:------------|:-----|:-----------|:-----------|
| Basal       | 0.2743294   | 0.95 | -0.2629954 | 0.8090471  |
| OctM        | -0.3353657  | 0.95 | -0.871075  | 0.2035165  |
| OctMD(25)   | -0.8434099  | 0.95 | -1.397096  | -0.2822868 |
| OctMD(50)   | -1.0817     | 0.95 | -1.649495  | -0.504836  |
| OctMD(250)  | -0.8366697  | 0.95 | -1.390007  | -0.2759454 |
| OctMD(5000) | -2.375327   | 0.95 | -3.069896  | -1.667193  |
| OctMDGS     | -2.12419    | 0.95 | -2.788705  | -1.4465    |
| CytC        | -1.746492   | 0.95 | -2.370197  | -1.110578  |
| FCCP Peak   | -0.9069802  | 0.95 | -1.464092  | -0.3419698 |
| Omy         | 0.2081269   | 0.95 | -0.3277909 | 0.7420591  |
| AmA         | -0.05670699 | 0.95 | -0.5899806 | 0.4771102  |

The only thing I have with these results is that the effect size values
are the negative values of what they should be. If you need to change
this, just do the following:

``` r
effect_sizes$Cohens_d <- as.numeric(effect_sizes$Cohens_d)

effect_sizes$Cohens_d_new <- effect_sizes$Cohens_d[] * -1
```

Or if you want to have the absolute values:

``` r
effect_sizes$Cohens_d_all_pos <- abs(effect_sizes$Cohens_d)
```

        If you care to do the effect sizes as a pairwise comparison, you
can create contrasts fairly simply and run a quick analysis to get the
effect sizes for each pairwise comparison like so:

``` r
#Create a function to calculate contrasts
rcontrast <- function(t, df)
{r <- sqrt(t^2/(t^2 + df))
    paste("r = ", r)}

#Create the data frame of variables to be analyzed
tukey_contrasts <- data.frame(tukey_results$contrasts)

#Calculate pairwise effect sizes using the created contrasts function above
tukey_contrasts$`Effect Size` <- t(data.frame(lapply(
  tukey_contrasts$t.ratio, function(x) rcontrast(x, 529))))
```

| contrast                              |    estimate |        SE |  df |     t.ratio |   p.value | Effect Size            |
|:--------------------------------------|------------:|----------:|----:|------------:|----------:|:-----------------------|
| Basal Baseline - OctM Baseline        |  -2.1341405 | 0.6791952 | 529 |  -3.1421607 | 0.1671855 | r = 0.135358367295924  |
| Basal Baseline - OctMD(25) Baseline   |  -4.6220572 | 0.6791952 | 529 |  -6.8051969 | 0.0000000 | r = 0.283719683406489  |
| Basal Baseline - OctMD(50) Baseline   |  -6.9058928 | 0.6791952 | 529 | -10.1677582 | 0.0000000 | r = 0.404329110906618  |
| Basal Baseline - OctMD(250) Baseline  |  -9.5952450 | 0.6791952 | 529 | -14.1273741 | 0.0000000 | r = 0.523385900124361  |
| Basal Baseline - OctMD(5000) Baseline | -18.3654651 | 0.6791952 | 529 | -27.0400388 | 0.0000000 | r = 0.76171731489302   |
| Basal Baseline - OctMDGS Baseline     | -46.4718724 | 0.6791952 | 529 | -68.4219660 | 0.0000000 | r = 0.947879480568246  |
| Basal Baseline - FCCP Peak Baseline   | -52.2519367 | 0.6791952 | 529 | -76.9321323 | 0.0000000 | r = 0.958098830493787  |
| Basal Baseline - Omy Baseline         | -14.7396747 | 0.6791952 | 529 | -21.7016761 | 0.0000000 | r = 0.686280038763894  |
| Basal Baseline - AmA Baseline         |   0.3798678 | 0.6791952 | 529 |   0.5592911 | 1.0000000 | r = 0.0243098159135197 |
| Basal Baseline - Basal Post           |  -2.9498066 | 0.3037453 | 529 |  -9.7114464 | 0.0000000 | r = 0.388983434095933  |

------------------------------------------------------------------------

# 7. Visualizing the Data: The Simple Guide to ggplot2

## 7.1. The Basics

### 7.1.1. The Importance of Visualization

         I think the importance of data visualization goes without
saying: seeing the data is much more telling than reporting the numbers.
Thus, painting an accurate picture of the data at hand is imperative for
the communication of scientific data. Not only do we want readers to
visualize our results in a meaningful way, we also want to provide an
explaination of our results in a manner that compliments our manuscript.
In this section, I till try to detail a few different plots (mostly
using the [packages `'ggplot2'` and `'ggpubr'`](#PlotPackages)) that can
be very helpful for the interpretation and communication of respirometry
data.

### 7.1.2. Understanding Plotting in R

        Plotting in R can be fairly simple, once the environment of R
(and ggplot2) is well understood. Essentially, graphing in R works by
creating a base graph with the data you will be using, then creating
layers upon layers of the data you want to visualize. The process is
analogous to building a cake: you start with the inner cake layers
(data), then add icing (data points), some color, a message like “Happy
Birthday!” (titles, legends, etc.), maybe a few candles (significance
asterisks), and *Voila*! Your graph is made.  
        Each layer we place to our code will add another detail to the
graph. However, while this is a very powerful feature in R, we must
always remember that the simplest graphs are often the best graphs. It
is important to not get carried away when graphing, such as adding too
many points or details that confuse the reader, rather than compliment
the story. When making graphs, it is best to always be efficient with
your space and [colors](#Color) — guiding the reader to the most
important information using proper colors and visuals while having
sufficient negative space.

### 7.1.3. ggplot2 and ggpubr: The two graphing packages of choice

        The two most popular graphing tools that are used in R are
`'ggplot2'` and `'ggpubr'`, both of which are important tools to create
great visualizations. The ggplot2 package is the most well-known
visualization package used in R. It was developed by Hadley Wickham (the
same person who developed the Tidyverse) based on [the Grammar of
Graphics](https://www.amazon.com/Grammar-Graphics-Statistics-Computing/dp/0387245448).
Logically,`'ggplot2'` is based on the idea that you can build layer upon
layer to get the final graphical outcome of choice.  
        Similarly, `'ggpubr'` is founded on the same principles, but is
created to “generate publication-ready graphics.” Both packages are
relatively similar in how they operate, so choose the package you like
best. Generally, I think `'ggpubr'` is easier to work with, but
`'ggplot2'` has the advantage of being more flexible in many aspects.
Another thing to note is that `'ggpubr'` *does not* work well with
variable names that have more than one string of characters in it
(i.e. more than one word), meaning you can only use factors that contain
a single word in `'ggpubr'`. It isn’t the greatest, but it still works
fine — you can also just [simply rename the factor](#Renaming).

### 7.1.4. Color Palettes

        Choosing color is an important aspect of graphing. Not only does
one need to consider the audience and message (colors are associated
with feelings, but are largely dependent on sociocultural norms).
Furthermore, a fair portion of the population is colorblind. Therefore,
to convey an appropriate message, one must consider strongly the
audience and message of the manuscript. That said, color is not
something one wants to mess up when preparing figures. R is great in
that you can create many different color combinations to make awesome
graphs. A useful cheatsheet to this [can be found
here](https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf).
However, choosing colors on your own can still be a tricky task.  
        Luckily for us, there are several packages that contain color
palettes [(and a more comprehensive list can be found
here)](https://github.com/EmilHvitfeldt/r-color-palettes#color-blindness).
Three common packages are `'RColorBrewer'`, `'ggsci'`, and
`'wesanderson'` (yes, [like the film
director](https://en.wikipedia.org/wiki/Wes_Anderson)). Let’s take a
look at these, starting with `'RColorBrewer'`:

``` r
#Load RColorBrewer
library(RColorBrewer)

#Show palettes
display.brewer.all()
```

![](Manual_files/figure-gfm/unnamed-chunk-77-1.png)<!-- --> The nice
thing about `'RColorBrewer'` is that there are specific palettes that
are colorblind, which we can identify by:

``` r
display.brewer.all(colorblindFriendly = TRUE)
```

![](Manual_files/figure-gfm/unnamed-chunk-78-1.png)<!-- --> \| The
`'ggsci'` package contains several palettes that are used in major
journals (Nature, Lancet, others). We can only [view these online for
now](https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html).

        For the `'wesanderson'` palette, we can’t see all of the
palettes unless you go to [the GitHub
site](https://github.com/karthik/wesanderson), but all of the palette
names can be listed by:

``` r
library(wesanderson)

names(wes_palettes)
```

    ##  [1] "BottleRocket1"  "BottleRocket2"  "Rushmore1"      "Rushmore"      
    ##  [5] "Royal1"         "Royal2"         "Zissou1"        "Darjeeling1"   
    ##  [9] "Darjeeling2"    "Chevalier1"     "FantasticFox1"  "Moonrise1"     
    ## [13] "Moonrise2"      "Moonrise3"      "Cavalcanti1"    "GrandBudapest1"
    ## [17] "GrandBudapest2" "IsleofDogs1"    "IsleofDogs2"

Which you can then view each palette individually by:

``` r
wes_palette("FantasticFox1")
```

## 7.2. Density Plots

        Density plots are useful to understand the distribution of the
data, and to visualize the spread of data points. In my experience, I
have found that `'ggpubr'` is much easier to work with in creating
simple density plots compared to `'ggplot'`, but this is mostly because
of my personal preference. Now, when we use `'ggpubr'` for this, we need
to remember that we must input all of the variables we need into the
function. So, when we call `ggdensity()` to create the plot, we need to
call the data, call the `x` variable, and input whatever else we need.
Functions like `color` and `fill` will indicate how to separate the
color of the lines and the fill color of the variables you have. If we
want to show where the mean is, we can also do so by `'add = "mean"'`.
So, if I want to look at the distribution of State III respiration data
(OctMGDS), I could do something like:

``` r
#Load ggpubr
library(ggpubr)

ggdensity(data = data_example_wide, x = "OctMDGS", color = "Condition", 
          fill = "Condition", add = "mean", 
          xlab = "Respiration Rate (pmol/sec/mg)",
          title = "Density Plot of OctMDGS Data", subtitle = "Distribution of the Frequency of Respiration Rates")
```

![](Manual_files/figure-gfm/unnamed-chunk-81-1.png)<!-- -->

Likewise, if I wanted to look at leak respiration (OctM), I could also
do:

``` r
ggdensity(data = data_example_wide, x = "OctM", color = "Condition", 
          fill = "Condition", add = "mean", 
          xlab = "Respiration Rate (pmol/sec/mg)",
          title = "Density Plot of OctM Data", subtitle = "Distribution of the Frequency of Respiration Rates")
```

![](Manual_files/figure-gfm/unnamed-chunk-82-1.png)<!-- --> Similarly,
we can show the distribution of all of the Baseline data by using the
`'subset'` and `'select()'` functions, and the `'%in%'` operator to
select only the baseline respiration data like so:

``` r
ggdensity(data  = subset(data_example_long, Condition %in% c("Baseline")), 
          x = "Rate", color = "State", add = "mean") + 
  theme(legend.text = element_text(size = 8))
```

![](Manual_files/figure-gfm/unnamed-chunk-83-1.png)<!-- -->

And we can run it on the post-interverntion:

``` r
ggdensity(subset(data_example_long, Condition %in% c("Post")), 
          x = "Rate", color = "State", add = "mean") + 
  theme(legend.text = element_text(size = 8))
```

![](Manual_files/figure-gfm/unnamed-chunk-84-1.png)<!-- -->

## 7.3. Bar Plots

        Bar plots are perhaps the most commonly used type of graph in
the literature, so it only seems appropriate to cover them in this
document. This is really the part of the section where layers become
important. Furthermore, I will try to outline as much code as I can, as
well as explain any notes. As before, `'ggpubr'` is the simplest
function to use. If we use `'ggpubr'`, we can make a simple plot by the
following code:

``` r
#Create the basic plot
ggpubr_bar <- ggbarplot(data = data_example_long, x = "State", y = "Rate", fill = "Condition", 
                        position = position_dodge(0.7))

ggpubr_bar
```

![](Manual_files/figure-gfm/unnamed-chunk-85-1.png)<!-- --> Each time we
create a plot, we must, at the very least, tell R the dataset we want to
use (via `'data = '`), and the x and y variables. If you have a grouping
variable (as we do with the ‘Condition’), you can designate that with
the `'fill = '` line — however, you also need to have the position of
the bars (via `'position = position_dodge(0.7)'`) specifically stated as
well or else you will be given a stacked bar chart. Now, as you can
probably tell, our graph is very messy and is missing a lot of
components. To begin cleaning it up, we will add a mean and error bars,
like so:

``` r
ggpubr_bar <- ggbarplot(data = data_example_long, x = "State", y = "Rate", fill = "Condition", 
                        position = position_dodge(0.7), add = "mean_se", error.plot = "upper_errorbar")

ggpubr_bar
```

We can also change the color by inputting a `'palette ='` line like so
(this is where you can use a palette or your own color choices, as I
have):

``` r
ggpubr_bar <- ggbarplot(data = data_example_long, x = "State", y = "Rate", 
                        fill = "Condition", 
                        position = position_dodge(0.7), 
                        add = "mean_se", error.plot = "upper_errorbar", 
                        palette = c("white", "dark red"))
```

Much better. Now, we should probably begin working out the aesthetics
with this by rotating the text and starting the y axis at 0, which we
will do by adding layers (via `'+'`) using `rotate_x_test()` and
`scale_y_continuous()`, like so

``` r
ggpubr_bar <- ggbarplot(data = data_example_long, x = "State", y = "Rate", 
                        fill = "Condition", 
                        position = position_dodge(0.7), 
                        add = "mean_se", error.plot = "upper_errorbar",
                        palette = c("white", "dark red")) +
  rotate_x_text(45) +
  scale_y_continuous(expand = c(0,0))
```

We can add a title and change the labels, by using the `ggtitle()` and
`labs()` functions

``` r
ggpubr_bar <- ggbarplot(data = data_example_long, x = "State", y = "Rate", 
                        fill = "Condition", 
                        position = position_dodge(0.7), 
                        add = "mean_se", error.plot = "upper_errorbar",
                        palette = c("white", "dark red")) +
  rotate_x_text(45) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle("Made Up Respiration Data") +
  labs(x = "", y = "Respiration Rate (pmol/mg/min)", 
       fill = "Study Timepoint")
```

And we can change many things in the `theme()` function, which alters
the basic layout such as background color, line/text thickness, and
legend position

``` r
#Set up basic plot (data, colors, layout)
ggpubr_bar <- ggbarplot(data = data_example_long, 
                        x = "State", y = "Rate", 
                        fill = "Condition", 
                        position = position_dodge(0.75), 
                        add = "mean_se", error.plot = "upper_errorbar",
                        palette = c("white", "dark red")) +
  #Rotate text
  rotate_x_text(45) +
  #Adjust axis position
  scale_y_continuous(expand = c(0,0)) +
  #Add title
  ggtitle("Made Up Respiration Data") +
  #Change axis labels
  labs(x = "", y = "Respiration Rate (pmol/mg/min)", 
       fill = "Study Timepoint") +
  #Move legend
  theme(legend.position = "right")
  

ggpubr_bar
```

![](Manual_files/figure-gfm/unnamed-chunk-90-1.png)<!-- --> ## 7.4. Box
Plots {#BoxPlots} Box plots follow a similar pattern, but we will use
the `ggboxplot` function and remove some of the lines of code that are
specific to barplots (such as the positioning and mean/SE), like this:

``` r
#Set up basic plot (data, colors, layout)
ggpubr_box <- ggboxplot(data = data_example_long, 
                        x = "State", y = "Rate", 
                        fill = "Condition", 
                        palette = c("white", "dark red")) +
  #Rotate text
  rotate_x_text(45) +
  #Adjust axis position
  scale_y_continuous(expand = c(0,0)) +
  #Add title
  ggtitle("Made Up Respiration Data") +
  #Change axis labels
  labs(x = "", y = "Respiration Rate (pmol/mg/min)", 
       fill = "Study Timepoint") +
  #Move legend
  theme(legend.position = "right")
  

ggpubr_box
```

![](Manual_files/figure-gfm/unnamed-chunk-91-1.png)<!-- -->

## 7.5. Adding Significance

Significance in R can be pretty tricky. To be frank, I don’t think there
is a great way to do this in R. You can place in significance symbols
manually, if needed, but that takes a lot of time. You can use ggsignif
as well, but you can’t really change much about the output of the plot.
I will show a way to do both.

### 7.5.1. Significance with ggsignif

GGsignif is by far the easiest method to display significance in graphs,
however, you will only get one output in the graphs (you cannot
customize the asterisks).

If we take our pervious graph (either the barplot or the boxplot), all
we would need to do is load the `ggsignif` package and run the
`stat_compare_means`. Doing this, we will also specify that we want
asterisks by using `label = "p.signif"` and hiding non-significant
valuies by stating `hide.ns = TRUE` function like so:

``` r
ggpubr_bar + stat_compare_means(aes(group = `Condition`), 
                                label = "p.signif", 
                                hide.ns = TRUE, 
                                label.y = 75) + 
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0,80))
```

![](Manual_files/figure-gfm/unnamed-chunk-92-1.png)<!-- --> Note that
you may have to change the scaling, as I did, by using the `limits`
command `scale_y_continuous`.

### 7.5.2. Manual Significance

Adding significance manually can be a pain, but I think it looks better
than the `stat_compare_means()` function because you can place special
asterisks or other marks wherever you need them to go.
`stat_compare_means()` only places the asterisks in their default
placement, to my knowledge. This may change in the future, though.

Fortunately, these significance markers aren’t too challenging to place,
but it does require *some* effort. To add special characters (or even
simple letters) to the graph, we will simply use the `annotate()`
function in ggplot like so:

``` r
ggpubr_bar + 
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0,80)) + 
  #Add stars
  annotate('text', x = c(3,4,5,6,7,8,9), y = 75, 
           label = "*", size = 5)
```

![](Manual_files/figure-gfm/unnamed-chunk-93-1.png)<!-- --> To add
brackets, we will use `geom_bracket()` like so:

``` r
ggpubr_bar + 
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0,80)) +
 #Add brackets
  geom_bracket(inherit.aes = TRUE, 
               #Set left edge for each bracket
               xmin = c(2.8, 3.8, 4.8, 5.8, 6.8, 7.8, 8.8),
               #set right edge for each bracket
               xmax = c(3.2, 4.2, 5.2, 6.2, 7.2, 8.2, 9.2),
               #position for each bracket
               y.position = c(20, 25, 26, 35, 65, 68, 35),
               #adjust tip length
               tip.length = 0.015,
               #Place asterisk on each bracket
               label = "*", 
               size = .65)
```

![](Manual_files/figure-gfm/unnamed-chunk-94-1.png)<!-- -->

## 7.6. Adding Individual Data

It may be useful to place individual data points on a plot. Depending on
how the data look, though, it can make the graphs appear messy. To do so
is very simple, though. We will simply add another layer to our plot by
using the `geom_point()` or `geom_jitter()`[12] function to make
something like this

``` r
ggpubr_bar +
  geom_point(aes(x = State, y = Rate, fill = Condition), 
               binaxis = 'y', stackdir = "center", dotsize = .5, 
               position = position_dodge(0.7), show.legend = F)
```

![](Manual_files/figure-gfm/unnamed-chunk-95-1.png)<!-- -->

You can also use the `geom_dotplot()` function when you have fewer
points (not shown), like so:

``` r
ggpubr_bar +
  geom_dotplot(aes(x = State, y = Rate, fill = Condition), 
               binaxis = 'y', stackdir = "center", dotsize = .5, 
               position = position_dodge(0.7))
```

## 7.7. My Graphs

You’ll notice that I have added some details to my original graphs that
are not present in the graphs above. I’ve included the raw code below to
list those changes:

``` r
final_graph <- ggplot(data_example_long, aes(x=`Respiration State`, 
                              y=`Respiration Rate (pmol/sec/mg)`, 
                              fill = `Condition`)) +
  #Adds standard error bars#
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width=0.75), 
               width = 0.3, size = 1.25) +
  #Adds Average bars#
  stat_summary(fun = mean, geom = "bar", width = 0.75, 
               position = position_dodge2(width = 5), 
               colour = "black", size = 1.25) +
  #add significance
  stat_compare_means(aes(group = `Condition`), 
                     label = "p.signif", label.y = 65, hide.ns = TRUE, 
                     method = "t.test", size = 8) +
  #Graph labels (\n creates a new line)#
  labs(x = "", 
       y = "Respiration Rate\n(pmol/sec/mg)", fill = "Condition") +
  #Removes Data point from legend#
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  #removes gridlines and top/left borders#
  theme_classic() +
  #Make y-axis start at 0, set limits#
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0,70), breaks=seq(0,70,10)) + 
  #Make bars different colors#
  scale_fill_manual(values = c("white", "dark red")) + 
  #Adjust fonts#
  theme(axis.line = element_line(size = 1), 
        axis.ticks = element_line(size = 1), 
        axis.text.x = element_text(size = 14, 
                                   color = "black"), 
        axis.text.y = element_text(color = "black", 
                                   size = 14),
        axis.title.y = element_text(size = 18, 
                                    margin = margin(t = 0, r = 15, 
                                                    b = 0, l = 0)),
        legend.text = element_text(size = 14),
        legend.title = element_blank()) +
  #Create dashed lines between Respiration States#
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5), 
             color = "grey", linetype = "longdash") +
  #Change names#
  scale_x_discrete(labels = c("OctMD(25)" = "OctMD\n(25)", 
                              "OctMD(50)" = "OctMD\n(50)", 
                              "OctMD(250)" = "OctMD\n(250)", 
                              "OctMD(5000)" = "OctMD\n(5000)",
                              "FCCP Peak" = "FCCP"))

final_graph
```

<img src="Manual_files/figure-gfm/unnamed-chunk-97-1.png" style="display: block; margin: auto;" />

## 7.8. Saving Graphs

Plots are easily saved in many formats including .png and .svg formats
using the `ggsave()` function. To do this, you need to save the plot as
an object and signify the file name, and you can customize the size and
the background color, too. Here’s an example:

``` r
#Save as a .png
ggsave("RespBoxplot.png", ggpubr_box)

#Save as a .svg with transparent background
ggsave("RespBar.svg", ggpubr_bar, bg = "transparent", width = 4, height = 3)
```

------------------------------------------------------------------------

# 8. Helpful Resources

Below are the resources I have used to compile this document

## 8.1. Books

### 8.1.1. For Statistics

-   **[Discovering Statistics Using
    R](https://www.discoveringstatistics.com/books/discovering-statistics-using-r/)**
    by Andy Field, Jeremy Miles, and Zoë Field
-   **[R for Data Science](https://r4ds.had.co.nz/)** by Garett
    Grolemund & Hadley Wickham
-   **[R Cookbook, 2nd Edition](https://rc2e.com/)** by JD Long & Paul
    Teetor
-   **[An R Companion for the Handbook of Biological
    Statistics](https://rcompanion.org/rcompanion/index.html)** by
    Salvatore S. Mangiafico
-   **[Handbook of Biological
    Statistics](http://www.biostathandbook.com/)** by John H. McDonald
-   **[Learning Statistics with R: A tutorial for psychology students
    and other beginners](https://learningstatisticswithr.com/book/)** by
    Danielle Navarro
-   **[R Programming for Data
    Science](https://bookdown.org/rdpeng/rprogdatascience/)** by
    Roger D. Peng
-   **[Advanced R](http://adv-r.had.co.nz/)** by Hadley Wickham

### 8.1.2. For Graphing

-   **[R Graphics Cookbook, 2nd edition](https://r-graphics.org/)** by
    Winston Chang
-   **[R Cookbook, 2nd Edition](https://rc2e.com/)** by JD Long & Paul
    Teetor
-   **[ggplot2: Elegant Graphics for Data
    Analysis](https://ggplot2-book.org/)** by Hadley Wickham
-   **[ggplot2 Cheat
    Sheet](https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf)**

## 8.2. Blogs

-   **[Towards Data Science](https://towardsdatascience.com/)**

## 8.3. Forums

-   **[Data Novia](www.datanovia.com)**
-   **[Stack Exchange](stats.stackexchange.com)**
-   **[Stack Overflow](https://stackoverflow.com/)**
-   **[STHDA](http://www.sthda.com/english/)**

## 8.4. Videos

-   **[University of Texas Statistics Online Support
    (SOS)](http://sites.utexas.edu/sos/)**

## 8.5. Resources for Packages

-   **[RDocumentation](https://www.rdocumentation.org/)**
-   **[CRAN package search](https://cran.r-project.org/)**

------------------------------------------------------------------------

# 9. Afterthoughts & Updates

        This section is dedicated to other lines of code that may be
useful for the exploration of data outside of the Excel files that I
have created and mentioned in the [first section](#Pulling) of the
document. Some of these may be more useful if you have other ways of
analyzing the data than what I have outlined above. Though I think that
the above sections are useful as well.

## 9.1. Categorizing Continuous Variables

Sometimes it is very useful to take numeric data and group them into
ranges or categories. We can easily do this using the `data.table`
package. For example, say we have a variable with age ranges, and we
want to divy them up into ‘Young’, ‘Middle Age’, and ‘Elderly’. For
this, we need the `'data.table'` package, and we will run this code:

``` r
setDT(data_example_wide)[ , "Age Group" := cut(Age, 
                                breaks = c(0,45,65,500), 
                                right = FALSE, 
                                labels = c("Young","Middle Age", "Elderly"))]
```

This line takes our data frame (‘data_example_wide’) and creates a new
column titled ‘Age Group’ based on values in the ‘Age’ column with the
ranges of (0-45, 45-65, 65+), and categorizes them into ‘Young’, ‘Middle
Age’, and ‘Elderly’ age groups. Furthermore, we can do something similar
if we wish to have more details in our grouping categories, such as
combining physical activity and age to get a category such as ‘Young
Active’ by doing the following;

``` r
data_example_wide <- data_example_wide %>%
  mutate(ActGroup = case_when((Age < 45 & Steps < 5000) ~ "Young Sedentary",
                           (Age < 45 & Steps > 5000) ~ "Young Active",
         (Age > 45 & Steps < 5000) ~ "Elderly Sedentary", 
(Age > 45 & Steps > 5000) ~ "Elderly Active"))
```

## 9.2. Calculations

        Sometimes it is easier to perform calculations (such as RCR and
sensitivity calculations) directly in R compared to doing it in Excel. I
will include some examples that I think are common and useful for that

### 9.2.1. Respiratory Control Ratio (RCR)

        RCR is probably one of the more common calculations used in
respirometry analysis. Classicaly defined by Chance, RCR is the ratio of
State 3 to State 4 respiration; or, the ratio of maximal respiration to
leak respiration. The purpose of the RCR is to determine the
“efficiency” of the mitochondria, and how much respiration is “wasted”
by proton leak. To create this column, we will simply create a new
column based of a mathematical function:

``` r
data_example_wide$RCR <- (data_example_wide$`OctMD(5000)`/data_example_wide$OctM)
```

The same steps can be used to calculate substrate sensitivity, if you
have that data available.

## 9.3. Enzyme Kinetics

        We can also calculate the enzyme kinetics of oxidative
phosphorylation, assuming you have performed some sort of titration with
difference concentration values of some substrate (such as ADP, like I
have in this example). Now, there are a couple of ways to do this. You
can, of course, use the Michaelis-Menten equation:

![v = \\frac{V\_{max} \[S\]}{K_M + \[S\]}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;v%20%3D%20%5Cfrac%7BV_%7Bmax%7D%20%5BS%5D%7D%7BK_M%20%2B%20%5BS%5D%7D "v = \frac{V_{max} [S]}{K_M + [S]}")

This method can be fairly complex and you typically need a dozen or so
data points, but I have found [an article written by Cathy Huitema
(Waterloo Centre for Microbial Research) and Geoff Horsman (Wilfrid
Laurier University) that explains how to do this in great
detail](https://www.biorxiv.org/content/10.1101/316588v1.full.pdf).
There is also the [`drc` package developed by Christian Ritz, Florent
Baty, Jens Stribig, and Daniel Gerhard and explained in this PLOSOne
article](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0146021)
that I have found very useful. I’m starting with the
[Lineweaver-Burke](#LineweaverBurke) plot simply because it’s an easy
test that requires fewer titration steps compared to the
[Michaelis-Menten](#MichaelisMenten) fitting. I’ll also cover [enzyme
inhibition](#DoseResponse) later.

### 9.3.1. Lineweaver-Burke Plot

To perform the Lineweaver-Burk Method, we will create a new data frame
with the data that we want to calculate the enzyme kinetics from. Don’t
forget to also include the subject number and conditon in this

``` r
library(tidyverse)
kinetics <- gather(Wide[c(1,2,6:10)], "Respiration State", 
                          "Respiration Rate (pmol/sec/mg)",
                          `OctMD(25)`:`OctMD(5000)`)
```

Now, we need to create a table with our substrate concentrations based
on the data we have collected thus far. Essentially, we will use the
`ifelse` function to recode the values ‘OctMD(x)’ to simply ‘x’, like
so:

``` r
#Create the kinetics data
kinetics$Concentration <- ifelse((
  kinetics$`Respiration State` == "OctM"), 0,
  ifelse((
    kinetics$`Respiration State` == "OctMD(25)"), 25, 
    ifelse((
     kinetics$`Respiration State` == "OctMD(50)"), 50,
      ifelse((
        kinetics$`Respiration State` == "OctMD(250)"), 250,
        5000))))
```

Now that we have the concentration, we will take the reciprocal of the
substrate concentration and the respiration rates

``` r
#Calculate the reciprocals
kinetics$r.concentration <- 1/kinetics$Concentration
kinetics$r.rate <- 1/kinetics$`Respiration Rate (pmol/sec/mg)`
```

We can plot the resulting data just to visually inspect the data points

``` r
#Visual inspection
ggplot(kinetics, aes(r.concentration, r.rate, color = Condition)) + 
  geom_point() + facet_wrap(~Subject) + stat_smooth(method='lm')
```

![](Manual_files/figure-gfm/unnamed-chunk-105-1.png)<!-- -->

Next, we need to perform the linear model using the `lm()` function. The
result will give us the linear model for each subject in each condition.
Keep in mind that the results we get from this model only gives us the
y-intercept and the slope. The y-intercept will be used to calculate the
![V\_{max}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;V_%7Bmax%7D "V_{max}"),
but the Km will need to be calculated by dividing the
![V\_{max}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;V_%7Bmax%7D "V_{max}")
(or, y-intercept) by the slope.

``` r
#Perform linear modeling on the regression (calculates y-intercept and slope)
lb_model <- (ddply(kinetics, .(Subject, Condition)
                   ,summarise,r.Vmax=lm(r.rate~r.concentration)$coef[1],
                   slope=lm(r.rate~r.concentration)$coef[2]))

#Calculate x-intercept
lb_model$r.Km <- -(lb_model$r.Vmax/lb_model$slope)
```

And this should give us results that look like this:

| Subject | Condition | r.Vmax  | slope  |   r.Km   |
|:-------:|:---------:|:-------:|:------:|:--------:|
|    1    | Baseline  | 0.06364 | 1.064  | -0.05979 |
|    1    |   Post    | 0.06003 | 0.8114 | -0.07398 |
|    2    | Baseline  | 0.06873 | 0.758  | -0.09067 |
|    2    |   Post    | 0.05422 | 0.7796 | -0.06954 |
|    3    | Baseline  | 0.06165 |  1.63  | -0.03782 |
|    3    |   Post    | 0.04419 | 1.121  | -0.0394  |

First Few Rows of lb_model

We will then get the reciprocal of the values once again.

``` r
lb_model$Vmax <- (1/lb_model$r.Vmax)
lb_model$Km <- -(1/lb_model$r.Km)
```

And then we can recode some of the values and perform our statistical
tests:

``` r
#Recode Condition to Factor

lb_model$Condition <- factor(lb_model$Condition, 
                             levels = c("Baseline", "Post"))

#Calculate means
lb_means <- by(list(lb_model$Vmax, lb_model$Km), 
        lb_model$Condition, stat.desc, basic = TRUE)


#T-test
vmax_ttest <- t.test(lb_model$Vmax ~ lb_model$Condition, paired = TRUE)

km_ttest <- t.test(lb_model$Km ~ lb_model$Condition, paired = TRUE)
```

And to graph the results:

``` r
ggplot(data = kinetics, aes(x = Concentration, 
                            y = `Respiration Rate (pmol/sec/mg)`)) + 
  geom_point() + facet_wrap(~Condition) + 
  geom_smooth(method = "nls", 
              method.args = list(formula = y ~ Vmax * x / (Km + x), 
                                 start = list(Vmax = 30, Km = 0.2)))
```

![](Manual_files/figure-gfm/unnamed-chunk-110-1.png)<!-- -->

### 9.3.2. Using the `drc` Package for Michaelis Menten Kinetics

        The `drc` package is extremely useful in analyzing dose response
curves in R. I’ve found it extremely useful in my own work and practice
in establishing both Michaelis-Menten kinetics and [dose response
curves](#DoseResponse) when I study OXPHOS inhibition.

To look at Michaelis-Menten curves, I will use a dummy set that I’ve
created below:

``` r
#Generate data frame (MMK) with Concentration (S) and Rate (V)
S <- c(0.005, 0.05, 0.5, 5, 50, 500, 2500, 5000)
V <- c(1, 2, 4, 8, 16, 32, 45, 50)

MMK <- data.frame(S, V)
```

Once I have my data, I will ise the `drm` function and fit these to the
equation `MM.2`, or the [Michaelis-Menten equation](#Kinetics)

``` r
library(drc)

#Create a model from the data
MMK_model <- drm(data = MMK, formula = V ~ S, 
                 fct = MM.2(names = c("Vmax", "Km")))

#Display summary parameters
summary(MMK_model)
```

    ## 
    ## Model fitted: Michaelis-Menten (2 parms)
    ## 
    ## Parameter estimates:
    ## 
    ##                  Estimate Std. Error t-value   p-value    
    ## Vmax:(Intercept)  47.6658     3.8763 12.2966 1.763e-05 ***
    ## Km:(Intercept)   134.9220    77.3802  1.7436    0.1318    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error:
    ## 
    ##  4.329198 (6 degrees of freedom)

Then, when we quickly plot the kinetics we get:

``` r
plot(MMK_model, log = "")
```

![](Manual_files/figure-gfm/unnamed-chunk-113-1.png)<!-- -->

### 9.3.3. Dose Response Curves for Inhibitors

## 9.4. Using Loops to Make Many Graphs

        Loops can come in handy if the goal is to create many different
graphs that all look alike, but you don’t want to go through the hassle
of changing every graph individually. In order to do this, it is
probably best to create a `for` loop, like so:

``` r
#Create loop parameters
for(y in names(Wide[5:15])){
#Create plot here
loop_plot <- ggplot(data = subset(Long, `Respiration State` %in% paste(y)), 
                    aes(x = Condition, 
                        y = `Respiration Rate (pmol/sec/mg)`, 
                        fill = Condition)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", 
               position = position_dodge(width = 0.9), width = 0.3, 
               na.rm = TRUE, size = 1, color = "black")+
  stat_summary(fun = mean, geom = "bar", 
               position = position_dodge(width = 0.9), 
               size = 1.1, color = "black") +
  stat_compare_means(aes(group = `Condition`), 
                     label = "p.signif", label.y = 65, hide.ns = TRUE, 
                     method = "t.test", size = 10, label.x = 1.5) +
  theme_classic() +
  scale_fill_manual(values = c("white", "dark red")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,80), 
                     breaks = seq(0,70,10)) +
  theme(axis.line = element_line(size = 1.1), 
        axis.ticks = element_line(size = 1.1, color = "black"), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, 
                                    margin = margin(t = 0, r = 20, b = 0, l = 1), 
                                    colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 11, color = "black"),
        legend.background = element_rect("NA"),
        plot.title = element_text(hjust = 0.5, size = 16)
  ) +
  ggtitle(paste(y)) +
  ylab("Respiration Rate (pmol/sec/mg)")

#Assign the plots as objects
assign(paste(y, "plot", sep = "_"), loop_plot)

}
```

The `subset()` function subsets the data in long format to only select
the values listed in `y`. Then, the `assign` function assigns each
individual object (in this case the graph for variable `y`) a unique
name. Your plots will be assigned to objects with names corresponding to
their state, such as “OctM_plot”,

``` r
`OctM_plot`
```

<img src="Manual_files/figure-gfm/unnamed-chunk-115-1.png" style="display: block; margin: auto;" />

“OctMDGS_plot”,

``` r
`OctMDGS_plot`
```

<img src="Manual_files/figure-gfm/unnamed-chunk-116-1.png" style="display: block; margin: auto;" />

and so on…

## 9.6. More Wide and Long Formatting

There are many other ways to convert between long and wide format. The
functions I discussed earler, `gather()` and `pivot_longer()` are both
functions from the Tidyverse. Another package, reshape2, has the
function `melt()` which can be used in a similar manner, and can work
with less information. You can also convert to wide format from long
format by using the `spread()` or `pivot_wider()` functions in the
Tidyverse, or by using the `dcast()` function in reshape2.

## 9.7. Prism-like Plots Made Easy

One package I highly recommend using if you like the aesthetics of
Graphpad Prism graphs is the package
**[ggprism](https://csdaw.github.io/ggprism/articles/ggprism.html)**. I
love this package simply because it has all of the wonderful, pleasing
aesthetics of a graph made in Prism but with no cost and so much ease.

The simple way to use the package is to use the `theme_prism()` function
in your ggplot command like so:

``` r
library('ggprism')

final_graph +
  theme_prism()
```

<img src="Manual_files/figure-gfm/unnamed-chunk-117-1.png" style="display: block; margin: auto;" />

You can also adjust the color palette to the Prism palette by using the
`scale_fill_prism()` function like so:

``` r
final_graph +
  theme_prism() +
  scale_fill_prism()
```

<img src="Manual_files/figure-gfm/unnamed-chunk-118-1.png" style="display: block; margin: auto;" />

## 9.8. Piecewise Linear Regression

## 9.9. Tukey Letters for Significance in Multiple Groups

------------------------------------------------------------------------

# 10. Functions

After completing the bulk of this document, I think I will try to create
functions (and possibly a package) to fulfill many of these analyses
automated and much easier to perform than using individual lines of
code.

[1] *If you haven’t installed this or other packages, do so by using the
`'install.packages()'` command; for example,
`install.packages('readxl')` would be used to install this package.*

[2] There are many different methods of converting between wide and long
format, which I do not cover in this section. For more, see [section
9.6](#MoreWideLong)

[3] *I recommend doing this step in long format, as the na.omit or na.rm
functions will remove entire rows of data, which is not very desirable
in most situations. A partial data set is sometimes better than a null
data set.*

[4] *I’m not sure why I can’t get all of these functions to work under
one package, which is why I have all of these options to make the same
code. Use what works best for you.*

[5] *It’s best to make sure that all of your [variables are identified
properly](#Correcting) (e.g. as integers, characters, etc.) before
running the t-test, but isn’t always necessary since R should identify
them properly. Still, you will need to do this for the ANOVA.*

[6] *If you want to extract other variables with `'sapply'` simply
change the two “p.value” characters to the column name you want to
extract (such as confidence interval, standard error, etc.).*

[7] \*I am starting off with the repeated-measures ANOVA instead of a
one-way ANOVA because the code is essentially the same, as [I explained
earlier](#ANOVA1) and gives the same result. Furthermore, it is more
likely that a factorial ANOVA, rather than a one-way ANOVA, will be run
on data like these.

[8] *This is just an example for reference. Normally, since we are
comparing the same subjects over time, we should be using a
[repeated-measures or a mixed design ANOVA,](#Repeated) which will be
covered in the next section.*

[9] *This is the way to analyze a two-way (or factorial) ANOVA, which is
what we need to do for these data. If you want to do a one-way ANOVA for
any reason, simply exclude the `'+ FactorB'` part (in this case
\``'+`Respiration State\`\`\`).*

[10] *It appears that the package we use to run is test, `'ez'`, doesn’t
like it when I have variable names containing more than one word. I’m
not sure why that’s the case, but I found a way around it all. This is
still the easiest way to go about performing the repeated-measures
ANOVA.*

[11] *I added the t() function which transposes the data, only to make
it easier to read.*

[12] The two functions essentially perform the same task. However,
`geom_jitter()` automatically offsets (jitters) the points so they don’t
overlay each other. You can do this manually in `geom_points()` by using
the `position = "jitter"`. Furthermore, you can change the jitter
position with the `height =` and `width =` commands in `geom_jitter()`
