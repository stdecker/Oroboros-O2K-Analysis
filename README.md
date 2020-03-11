# Oroboros-O2K-Analysis
  Analysis Files for Oroboros Oxygraph O2K. I hold no stake in company and have independently created these files for research purposes.

These files are publicly available for use and feedback. I intend to post these files to streamline the analysis and visualization of O2K data. I welcome any feedback and intend to maintain these files as living documents.

# O2K Excel Files
These Excel files are intended for initial processing of data from the negative slope data acquired from DatLab. From the DatLab file, select the 'Marks' tab and open the 'Slope Uncorrected + All Info' window (or press F2). From there, select the appropriate chamber (A or B) and statistic (our lab uses an average, but some use median). From here, select 'Copy to Clipboard' and past into the Excel sheet under the 'Raw File' tab. Repeat with the second chamber.

![](Images/Excel_Instructions.png)

The information and averages should automatically be calculated into formats that can be pulled for further analysis.

# R code
This R code is for fast processing and visualization of Oroboros data. I have only made code that will analyze the protocols I frequently use, however I will be developing more as I develop new protocols.
