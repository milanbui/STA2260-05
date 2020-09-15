#-------------------------------------------------------------------------------
# Name       : Milan Bui
# Class      : STA 2260.05
# Date       : 2020.09.03
# Assignment : Homework 2
# Description: Given recorded reaction times (in ms) random sample of players of 
#              osu! and Beatmania IIDX. Variance, standard deviation, & boxplots
#-------------------------------------------------------------------------------

# Given data -------------------------------------------------------------------

osu_data <- c(450, 420, 469, 360, 450, 390, 250, 415, 410, 480, 444, 461, 260, 
              440, 345, 435, 449)

iidx_data <- c(350, 369, 275, 215, 249, 210, 360, 320, 215, 233, 280, 274, 290, 
               310, 320, 290, 304)



# Part (a) ---------------------------------------------------------------------

# Get variance and sd of osu! data
osu_var <- var(osu_data); osu_var
osu_sd  <- sd(osu_data); osu_sd

# Create box plot for data
osu_boxplot <- boxplot(osu_data, horizontal = T)

# [Q] Do you think the data is skewed or symmetric? I skewed, what kind? Explain
#     what made you come to you conclusion.
# [A] This data is negatively skewed. The reason for this, is that the median,
#     which should be the peak is closer to the right.

osu_fl <- osu_boxplot$stats[2]; osu_fl
osu_fu <- osu_boxplot$stats[4]; osu_fu
osu_fs <- osu_fu - osu_fl; osu_fs

# [Q] What is the spread of fourths for this data? (use $)
# [A] The spread of fourths for this data is 60.




# Part (b) ---------------------------------------------------------------------

# Get the variance and sd of IIDX data
iidx_var <- var(iidx_data); iidx_var
iidx_sd  <- sd(iidx_data); iidx_sd

# Create box plot
iidx_boxplot <- boxplot(iidx_data, horizontal = T)

# [Q] Do you think the data is skewed or symmetric? I skewed, what kind? Explain
#     what made you come to you conclusion.
# [A] This data is symmetric as the median is in the center between the lowest
#     and highest value in the bar plot.

iidx_fl <- iidx_boxplot$stats[2]; iidx_fl
iidx_fu <- iidx_boxplot$stats[4]; iidx_fu
iidx_fs <- iidx_fu - iidx_fl; iidx_fs

# [Q] What is the spread of fourths for this data? (use $)
# [A] The spread of fourths for this data is 71.




# Part (c) ---------------------------------------------------------------------

# [Q] Based on the sd from part (a) and (b), which data set do you think is more
#     spread?
# [A] Based on the sd from part (a) and (b), the osu! data set is more spread as
#     its sd (67.837) is are larger number than the iidx data set's sd (50.026). 

# Construct side-by-side boxplots (multiple boxplots in the same window) in R 
# using the boxplot(...) command. You can throw multiple data sets in there.
boxplot(osu_data, iidx_data, horizontal = T)

# [Q] Do you think these side-by-side boxplots agree with your answer about 
#     which one is more spread?
# [A] No, as the osu! data values on the box plot are more condensed compared
#     to the iidx data values. The reason why the spread apeared larger was 
#     because of the outliers. If ignoring the outliers, iidx is more spread than
#     osu. 

# Calculate CV (CV = s / xbar) for both osu! and IIDX data sets.
osu_CV  <- osu_sd / (mean(osu_data)); osu_CV
iidx_CV <- iidx_sd / (mean(iidx_data)); iidx_CV

# [Q] Do these results still agree with what you said earlier about which data 
#     set may be more spread?
# [A] These results do not agree with my initial statement, but they do agree
#     with my statement after looking at the box plots.



# Part (d) ---------------------------------------------------------------------

# Create new variable that removes the outliers from the osu! data. You can just 
# copy your code above and manually remove them.
osu_data_new <- c(450, 420, 469, 360, 450, 390, 415, 410, 480, 444, 461, 440,
                  345, 435, 449)

# Get the sd, variance, and CV of this new data set
osu_data_new_sd  <- sd(osu_data_new); osu_data_new_sd
osu_data_new_var <- var(osu_data_new); osu_data_new_var
osu_data_new_CV  <- osu_data_new_sd / (mean(osu_data_new)); osu_data_new_CV

# [Q] Is it more or less spread than the other two datasets?
# [A] It is less spread than the other two data sets. 

# Create side-by-side boxplots using all three of the data sets. 
boxplot(osu_data, iidx_data, osu_data_new, horizontal = T)


# Comment on shape of the new data set
# This data set is still negatively skewed like the original data set.

# [Q] Notice anything off about the new data set? Comment on anything that stuck
#     out to you.
# [A] This data set had an outlier which was the orginial osu! data set's 
#     smallest xi. All of the five points in the five number summary changed
#     except for the largest xi and the median of the upper half (fu). 


