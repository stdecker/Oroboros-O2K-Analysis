# Instructions for the Excel files

## O2K Excel Files
These Excel files are intended for initial processing of data from the negative slope data acquired from DatLab. From the DatLab file, select the 'Marks' tab and open the 'Slope Uncorrected + All Info' window (or press F2). From there, select the appropriate chamber (A or B) and statistic (our lab uses an average, but some use median). From here, select 'Copy to Clipboard' and past into the Excel sheet under the 'Raw File' tab. Repeat with the second chamber.

![](Images/Excel_Instructions.png)

The information and averages should automatically be calculated into formats that can be pulled for further analysis.

Once the data has been copied from DatLab, paste the data into the Excel template 
(either [CHO](
/Excel%20Analysis/Template_CHO_O2K_Analysis.xlsx) or 
[Free Fatty Acid](/Excel%20Analysis/Template_FFA_O2K_Analysis.xlsx)) in the 'Paste Here' section highlighted.

![](/Images/Excel_paste.png)

The respiration rates and averaged data should be automatically calculated, along with mass (if input into DatLab).

From here, you will have two tabs that automatically average the values and put them into tables that can be exported for analysis via 
t-tests or ANOVA. This process will be explained in the [R files](/R%20Files), but should also be ready to be copied and pasted into most
available stats packages.
