## Assignment 7

# 1.	Load the music data into a sparse matrix. Since each user may mention one artist multiple times,
# we set "rm.duplicates=TRUE" when we load the data into a sparse matrix.

library(arules)
music <- read.transactions("assignment_dataset.csv", sep = ",", rm.duplicates = TRUE)

# 2.	Use summary () function to inspect the sparse matrix. How many users and columns do this matrix have?
# What does density mean? Who is the most frequent artist/item? How many rows/users have 30 items/artists? 

summary(music)
# matrix has 14593 users (rows) and 1004 columns
# density means the proportion of elements with non-0 values out of the total number of elements. The density of this matrix is 0.02
# most frequent artist is radiohead
# 340 rows have 30 items

# 3.	Inspect the first 5 users' artists. Does the records match the first five rows of the original csv file?
# Use itemFrequency to check the proportion of users liking certain artists.
# What are the proportions of users' favorite artists in column from 4 to 7? 

inspect(music[1:5])
# the records do match the first five rows of the original csv file

itemFrequency(music[, 1:8])
# the proportion of users liking the first 8 artists is somewhat low

itemFrequency(music[, 4:7])
# proportion of users whose favorite artists are listed in columns 4-7 is 0.04 for 3 doors down, 0.03 for 30 seconds to mars, 0.008 for 311, and 0.008 for 36 crazyfists

# 4.	Create a histogram plotting the artists have more than 10 percent support.
# How many artists in the matrix have at least 10 percent support? Who are these artists?
# Plot the first 20 artists with highest support. Which artist has the 15th highest support?

itemFrequencyPlot(music, support = 0.1)
# 10 artists have at least 10 percent support
# the 10 artists are coldplay, linkin park, metallica, muse, nirvana, pink floyd, radiohead, red hot chili peppers, the beatles, and the killers

itemFrequencyPlot(music, topN = 20)
# artist with 15th highest support is placebo

# 5.	Generate a visualization of the sparse matrix for the first 100 users' preference.
# Then generate a visualization of a random sample of 500 users' music favorite. 

image(music[1:100])
image(sample(music, 500))

# 6.	Use apriori() function to train the association rules on the music data.
# How many rules do we have when we use the default setting?
# In order to learn more rules, we adjust support level to 0.01, minlen =2 and confidence level to 0.25.
# How many rules do we have then?

basic.model <- apriori(music)
inspect(basic.model)
# there are 0 rules under default settings

model <- apriori(music, parameter = list(support = 0.01, confidence = 0.25, minlen = 2))
inspect(model)
# we now have 788 rules

# 7.	Summarize the rules generated from adjusted parameters. How many rules have size 3 among all the rules?
# Check the first ten rules. If a user likes james blunt, which artist else should online radio recommend to this user?
# Sort the rule list by lift and inspect the first five rules with highest lift. Which rule has the fourth highest lift?

summary(model)
# 224 rules have a size of 3

inspect(model[1:10])
# inspecting the first 10 rules does not tell us the answer to this question.
jamesblunt <- subset(model, items %in% "james blunt")
inspect(jamesblunt)
# if a user likes James Blunt then last.fm should recommend coldplay (or that they reconsider their music choices)

inspect(sort(model, by = "lift")[1:5])
# rule with 4th highest lift is: if (left-hand side) = "{beyonce}", then (right-hand side) "{rihanna}"

# 8.	Find subsets of rules containing any cold play. How many rules have cold play?
# Sort the rules by support and inspect the first five cold play rules with highest support.
# What rule has the 2nd highest support?

coldplay <- subset(model, items %in% "coldplay")
inspect(coldplay)
# 172 rules contain coldplay

inspect(sort(coldplay, by = "support")[1:5])
# rule with 2nd highest support is: if (left-hand side) = "{radiohead}", then (right-hand side) "{coldplay}"

# 9.	You can write these rules to a file and save the rules into a data frame. How many variables are in this saved data frame?

musicrules_df <- as(model, "data.frame")
str(musicrules_df)
# there are 6 variables in the data frame