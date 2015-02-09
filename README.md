Ignition data script
==
This is an R script for the analysis of exothermic samples. It interprets three imported .log files by highlighting critical datapoints and basing calculations from those datapoints. Because the critical points are highlighted one can easily determine what level of trust to put in the calculated results, whether to tweak certain formulae (to acquire more accurate critical points), or to simply omit the datapoint all together.

The critical points are named thusly:
 - **dropTime**: *when sample is dropped into furnace, also determines baseline furnace temperature*
 - **exoTime**: *when sample ignites*
 - **maxTemp**: *maximum temperature*
 - **exoDuration**: *when sample temperature drops below baseline temperature*

Are used in determining these calculated results:
 - **Time to ignition**
    - exoTime - dropTime
 - **Max burn temperature**
    - Is simply the result of a max() function call
 - **Duration of exothermic reaction**
    - exoDuration - exoTime

Use
-----
Use of this script requires the statistical program [R] [1] and preferably the IDE [RStudio] [2]. R packages called upon are: grid, gglot2, gridExtra, & reshape.

Two variables require definition before use:

- working directory
- filename prefix

####*Working directory* 
This is the folder where the .log files are stored & read from. For example, if your working directory is found in "C:/Ignition/logfiles", you would enter the following in R: 

```
setwd("C://Ignition//logfiles")
```

####*Filename prefix* 
This requires a specific naming convention be applied, you must have a "prefix", followed by a *hyphen* and then the numbers 1 through 3. For example, if your files were named thusly:
* this_is_my_filename-1
* this_is_my_filename-2
* this_is_my_filename-3

Then you would simply enter the following code to assign the variable"prefix":
```
prefix <- "this_is_my_filename"
```

####*Remove Row Function*
If standard deviations are too high there may be a good reason to remove an outlier sample from the results table. In comment code near the top you will see:
```
# REMOVE ROW FUNCTION
#   round(removeRow(1),1)  # removes pyro1
#   round(removeRow(2),1)  # removes pyro2
#   round(removeRow(3),1)  # removes pyro3
```
This allows for easy highlighting and hitting *Cntrl+Enter* to quickly test how a result might change if one sample value is removed. If `pyro2` needs removal, for example, scroll down to the OPTIONS section near the top and, reassign the appropriate variable, in this case `remove2` will need to be reassigned to `TRUE`:
```
remove1 <- FALSE
remove2 <- TRUE
remove3 <- FALSE # set to TRUE if removal desired
```
Simply run the script again for an updated calculations table.

####*Exporting*
When you wish to export the visual data, simply call one or both of the functions: `exportPng()`, `exportPdf()`. The default PNG size is 1200 x 900px; PDF is 11 x 8.5in They are conveniently located near the top in comment code for easy highlighting and hitting *Cntrl+Enter* to export.




***
Future changes
----
Features that may or may not be added in the future:
- Wrap script in a loop to allow analysis & comparison of multiple sets of samples

[1]:http://cran.us.r-project.org/
[2]:https://www.rstudio.com/
