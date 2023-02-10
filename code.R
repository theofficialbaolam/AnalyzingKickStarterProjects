library("ggplot2")
ks <- read.csv("Kickstarter2018.csv")


# Checking which specific rows have NA values and total NA values
sum(is.na(ks)) # 3801 NA entries
sum(is.na(ks$ID))
sum(is.na(ks$name)) # 4 NA entries
sum(is.na(ks$category))
sum(is.na(ks$main_category))
sum(is.na(ks$currency))
sum(is.na(ks$deadline))
sum(is.na(ks$goal))
sum(is.na(ks$launched))
sum(is.na(ks$pledged))
sum(is.na(ks$state))
sum(is.na(ks$backers))
sum(is.na(ks$country))
sum(is.na(ks$`usd pledged`)) # 3797 NA entries
sum(is.na(ks$usd_pledged_real))
sum(is.na(ks$usd_goal_real))

# Remove specific corrupted/nonsensical entries
# For example, NA pledged amount AND 0 backers AND pledged amount in USD >= 0
ks <-ks[!(ks$backers == 0 & is.na(ks$usd.pledged) & ks$usd_pledged_real >= 0), ]

# Remove variables/columns that we will not use
ks <- subset(ks, select = c(ID, name, category, main_category,
                            currency, deadline, goal,
                            launched, pledged,state, backers,
                            country, ratio, pledged_thousands))

# Eliminate the entries with 1970 launch dates
ks <- ks[!ks$ID %in%
           c(1014746686, 1245461087, 1384087152, 1480763647,
             330942060, 462917959, 69489148), ]

# Assign a variable to show which rows contain null values
# in the specified column
# Create a variable to print the rows from the name column
DF <- read.csv("2018.csv", na.strings=c("NA", "NULL"))
new_DF <- subset(ks, is.na(ks$name))

# Changing the NA rows found using previous code
ks$name[is.na(ks$name)] <- "Unknown"

# Create a new variable for ratio of pledged dollar amount
# over project goal
ks$ratio <- (ks$pledged/ks$goal)

# Create an additional column to make the graph more compact
# for pledged dollars in thousands of dollars
ks$pledged_thousands <- (ks$pledged/1000)





# Factor so as to order the bar plot
# in decreasing success rate
ks$state <- factor(ks$state, levels = c("suspended", "canceled",
                                        "failed", "successful", "live"))
ks$main_category <- factor(ks$main_category,
                           levels = c("Dance", "Theater",
                                      "Comics", "Music",
                                      "Art", "Film & Video",
                                      "Games", "Design",
                                      "Publishing", "Photography",
                                      "Fashion", "Food",
                                      "Crafts", "Journalism",
                                      "Technology"))

# Graph 1 (Bar plot): Help visualize success/failure rate per category
# Initial version
ggplot(data = ks) +
  geom_bar(mapping = aes(x = main_category, fill = state), position = "dodge") +
  xlab("Main Category") +
  ylab("Proportion") +
  ggtitle("State of Each Kickstarter Project Category (Normalized)") +
  theme(axis.text.x = element_text(angle = -45),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        plot.title = element_text(size = 16),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12))

# Graph 1 (Bar plot): Help visualize success/failure rate per category
# Final version
ggplot(data = ks) +
  xlab("Main Category") +
  ylab("Proportion") +
  ggtitle("State of Each Kickstarter Project Category (Normalized)") +
  geom_bar(mapping = aes(x = main_category, fill = state), position = "fill") +
  theme(axis.text.x = element_text(angle = -45),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        plot.title = element_text(size = 16),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12))

# Indices of ks of the six categories
# with the most expensive goals by median
most_expensive_category <- which((ks$main_category == "Technology" |
                                    ks$main_category == "Publishing" |
                                    ks$main_category == "Journalism" |
                                    ks$main_category == "Games" |
                                    ks$main_category == "Film & Video" |
                                    ks$main_category == "Design"))

# Subset those entries
most_expensive <- ks[most_expensive_category, ]

# Factor these categories so that
# box plot is listed in decreasing median goal values
most_expensive$main_category <- factor(most_expensive$main_category,
                                       levels = c("Technology", "Design",
                                                  "Games",
                                                  "Film & Video",
                                                  "Publishing",
                                                  "Journalism"))


# Get the quantile and IQR of goal
Q <- quantile(ks$goal, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(ks$goal)

# Entries of ks without extreme outliers
no_outliers <- subset(most_expensive, (most_expensive$goal > (Q[1] - 3 * iqr)) &
                        (most_expensive$goal < (Q[2] + 3 * iqr)))

# Graph 2 (Box plot): Helps analyze relationship
# between goal, state, and main_category
# No outliers
ggplot(data = no_outliers,
       mapping = aes(x = main_category,
                     y = goal, color = state)) +
  geom_boxplot ()+
  xlab("Category") +
  ylab("Goal") +
  ggtitle("Distribution of Goal for the Six Most Expensive Categories") +
  ylim(0, 60000) +
  theme(axis.text.x = element_text(angle = -45),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        plot.title = element_text(size = 16),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12))

# Graph 2 (Box plot): Helps analyze relationship
# between goal, state, and main_category
# Final iteration
ggplot(data = most_expensive,
       mapping = aes(x = main_category,
                     y = goal, color = state)) +
  geom_boxplot ()+
  xlab("Category") +
  ylab("Goal") +
  ggtitle("Distribution of Goal for the Six Most
          Expensive Categories") +
  theme(axis.text.x = element_text(angle = -45),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        plot.title = element_text(size = 16),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12))


# Graph 2 (Box plot): Helps analyze relationship
# between goal, state, and main_category
# Final iteration (range changed)
ggplot(data = most_expensive,
       mapping = aes(x = main_category,
                     y = goal, color = state)) +
  geom_boxplot ()+
  xlab("Category") +
  ylab("Goal") +
  ggtitle("Distribution of Goal for the Six Most Expensive Categories") +
  ylim(0, 800000) +
  theme(axis.text.x = element_text(angle = -45),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        plot.title = element_text(size = 16),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12))


# Barplot of only the successful projects for each category
# Helpful in visualizing data
successful_subset <- subset(ks, state == "successful",
                            select = (c(ID, name, category,
                                        main_category, goal, pledged,
                                        state, backers)))
ggplot(successful_subset, aes(x = factor(main_category),
                              fill = main_category))+
  geom_bar() +
  labs(x = "Main Category", y = "Number of Successful Projects",
       title = "Number of Successful Projects Per Main Category")

# Plot covering all main categories in relation to
# pledged dollars and backers
ggplot(ks, aes(x = backers,
               y = pledged_thousands,
               color = main_category)) +
  geom_smooth()

# Making subsets of all the 4 largest categories based on a test plot
# A plot covering only the top 4 categories in terms
# of pledged dollars and backers
big_main_category <- subset(ks, main_category == "Technology" |
                              main_category == "Design" |
                              main_category == "Games" |
                              main_category == "Film & Video",
                            select = (c(ID, name, category,
                                        main_category, goal,
                                        pledged, state, backers,
                                        pledged_thousands)))

ggplot(big_main_category, aes(x = backers, y = pledged_thousands,
                              color = main_category)) + geom_smooth() +
  labs(x = "Number of Backers", y = "Dollars Pledged (in thousands)",
       title = "Backers vs. Pledged Dollars (Largest Four Categories)")


# Create subset of all smaller categories to compliment the other plot
# Plot covering the rest of the main categories
small_main_category <- subset(ks, main_category != "Technology" &
                                main_category != "Design" &
                                main_category != "Games" &
                                main_category != "Film & Video",
                              select = (c(ID, name, category,
                                          main_category, goal, pledged,
                                          state, backers, pledged_thousands)))

ggplot(small_main_category, aes(x = backers,
                                y = pledged_thousands,
                                color = main_category)) +
  geom_smooth() +
  labs(x = "Number of Backers",
       y = "Dollars Pledged (in thousands)",
       title = "Backers vs. Pledged Dollars (Smaller Categories)")

# Obtaining the quartiles and IQR data
Q <- quantile(ks$goal, probs = c(.25, .75), na.rm = FALSE)
iqr_ks <- IQR(ks$goal)

# Removing outliers from the dataset
no_outliers <- subset(ks, ks$goal > (Q[1] - 3*iqr_ks) &
                        ks$goal < (Q[2] + 3*iqr_ks))

# Creating a dataset of only Technology projects without outliers
# Plot for tech only set for testing non outlier application
no_outliers_tech <- subset(no_outliers, main_category == "Technology")
ggplot(no_outliers_tech, aes(x = launched,
                             y = goal,
                             color = main_category)) +
  geom_smooth() +
  facet_wrap(main_category ~ ., ncol = 3) +
  labs(x = "Date Launched", y = "Fundraising Goal",
       title = "Fundraising Goals of Categories Over Time")


# Plot with outliers included
# Used for comparison between using outliers and not using outliers
# for this specific analysis
ggplot(ks, aes(x = launched, y = goal, color = main_category))
+ geom_smooth() + facet_wrap(main_category ~ ., ncol = 3)
+ labs(x = "Date Launched", y = "Fundraising Goal",
       title = "Fundraising Goals of Categories Over Time")

# Plot without outliers
ggplot(no_outliers, aes(x = launched, y = goal, color = main_category))
+ geom_smooth() + facet_wrap(main_category ~ ., ncol = 3)
+ labs(x = "Date Launched", y = "Fundraising Goal",
       title = "Fundraising Goals of Categories Over Time")
