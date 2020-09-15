#-------------------------------------------------------------------------------
# Name       : Milan Bui
# Class      : STA 2260.05
# Date       : 2020.08.28
# Assignment : Homework 1
# Description: Create 3 histograms (discrete, continuous, & categorical data)
#-------------------------------------------------------------------------------

#--Given code-------------------------------------------------------------------
set.seed(1) # Controls rand results. Run BEFORE simulating data.

# ?sample and ?seq for documentation
?sample
?seq

# seq(...) generates a sequence of nums "from" some num "to" another, inclusive, 
# "by" is the increment argument, so "by = 2" will have  sequence of nums 
# incrementing by 2
# replace = T means once a num is picked, it can be used again. replace = F is
# "without replacement", so a num won't appear more than once.

# Gets 420 #s 0-5, incrementing by 1 that can have repeating numbers.
discrete_data <- sample(x=seq(from=0, to=75, by=1), size=420, replace=T)


#rnorm(...) generate n rand nums from a normal distribution w/ mean and standard
# deviation as specified. (just to get truly continuous data)
# mean = sum of terms / # of terms >> sum / 400
# sd   =  sqrt((sum of all (diff of each # from mean)^2) / # of terms)
# generate 400 rand nums with mean 300 and standard deviation 5
continuous_data <- rnorm(n=400, mean=300, sd=5)


# LETTERS is a built in set of capital letters, "A", "B", "C", etc. Individual 
# letters are accessed by array notation, such as LETTERS[1] returning "A".
# prob=... is a list of probabilities corresponding to the letters, so LETTERS[1]
# has a 50% chance of being selected, for example.
ordinal_data <- sample(LETTERS[c(1:4, 6)], size=420, replace=T, prob=c(0.5, 0.3,
                       0.15, 0.1, 0.05))



#--Part (a)---------------------------------------------------------------------

# Construct separate histograms (right-opened) for the discrete and continuous 
# data which have a num of bins according to the rule # bins = sqrt(size of data)
discrete_num_bins <- ceiling(sqrt(length(discrete_data)))
hist(discrete_data, breaks=seq(from=min(discrete_data), to=max(discrete_data), length.out = discrete_num_bins + 1),
     right=F)

continuous_num_bins <- ceiling(sqrt(length(continuous_data)))
hist(continuous_data, breaks=seq(min(continuous_data), max(continuous_data),
                                 length.out = continuous_num_bins + 1), right=F)


# [Q] What type of shape does the continuous data have? What are the features 
#     that made you come to your conclusion?
# [A] The continuous has symmetric unimodal shape as the highest frequencies are
#     towards the middle. As you move right, the data increases, reaches a peak,
#     then decreases, not coming up again.


# Create barplot for ordinal data. This type of barplot is a Pareto Chart 
# (highest freq to lowest freq)

# Formats into table to get frequency distribution
ordinal_data_table <- table(ordinal_data)


# Makes barplot
barplot(ordinal_data_table)



#--Part (b)---------------------------------------------------------------------

# Copy and paste ordinal data's line to a new line and change the prob=c(...) arg
# around to cause barplot to no longer be a pareto chart. Set another seed before
# doing this (any num can be inside) so results are reproducible. Should have two 
# lines assigning something to ordinal_data.

set.seed(1) # Controls rand results.

ordinal_data <- sample(LETTERS[c(1:4, 6)], size=420, replace=T, prob=c(0.1, 0.5,
                       0.5, 0.3, 0.015))

# Formats into table to get frequency distribution
ordinal_data_table <- table(ordinal_data)

# Makes barplot
barplot(ordinal_data_table)


#--Part (c)---------------------------------------------------------------------

# Create a function that takes an argument for the size of the data set
# It should:
#   Simulate a random sample of data from 10 to 40 in increments of 0.25
#      - size of data is determined by the argument you pass to the function
#   Draw a histogram with the appropriate num of bins (left-closed, right-open)
#      - remember how breaks=seq(...) interacts with length.out
#      - command sqrt(...) may be useful
generate_data <- function(n){
  data <- sample(x=seq(from=10, to=40, by=0.25), size = n, replace = T)
  
  num_bins <- ceiling(sqrt(n))
  hist(data, breaks=seq(from=10, to=40,length.out = num_bins + 1), 
       right=F)
}


# Call function a few times by passing in n = 25 and n = 300, comment on some of 
# the results you see. 

# n = 25: The highest frequency is 9 while the lowest is 4. The histogram appears
#         to be negatively skewed.
#         Changed to highest frequency 10, lowest 2. Symmetric bimodal.
#         Changed to highest 8, lowest 5. Positively skewed and pareto chart
#         Changed to highest 8, lowest 4. Symmetric bimodal
#         Changed to highest 11, lowest 3. Bimodal w/ higher peak on left.
#             (positively skewed)
#         Changed to highest 8, lowest 5. Negatively skewed
#         Changed to highest 7, lowest 5. Negatively skewed
#         Changed to highest 12, lowest 3. Negatively skewed
# Overall, the highest seemed to more frequently be between 8 and 10. Though rare,
# the highest frequency would be as low as 7 or as high as 12. I did not see
# any of the highest frequencies go below 7 or higher than 12. 0 also sometimes
# appeared as the lowest frequency. Though the frequencies were random, I did 
# notice that pattern.
generate_data(25)

# n = 300: The highest frequency is approx 30 while the lowest is approx 8. The
#          histogram appears to be bimodal.
#          As I ran this line, it became increasingly difficult to determine
#          the shape of the histogram as there were so many varying frequencies
generate_data(300)


# [Q] What kind of variation is there between the two aside from the number of bins?
# [A] There is a difference in frequencies. Because of how the bins are divided,
#     the larger the size, the higher the frequency goes.

