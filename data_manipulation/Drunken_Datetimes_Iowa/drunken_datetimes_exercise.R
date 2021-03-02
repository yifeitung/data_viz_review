# Look at breathalyzer test data from the State of Iowa. 
# You will manipulate the dates and times in the breath alcohol data to answer questions such as "What day has the most tests?. at which hour of the day are breath alcohol tests most common? and "are blood alcohol content (BAC) results higher on days when Iowa State University's football team plays?"

# 1556 observations from breath alcohol tests collected by the Ames and Iowa State University Police Department

# DateTime: date & time of tests (datetime, "America/Chicago")
# Location: who administered the test, Ames PD or ISU PD? (character)
# Gender: gender of person being tested (character)
# Res1: first breath alcohol reading (nums)
# Res2: second breath alcohol reading (nums)

# Load necessary packages
library(tidyverse)
library(lubridate)

# read in the data from breath_alcohol_datetimes.csv
ba_dates <- read_csv("datasets/breath_alcohol_datetimes.csv")
str(ba_dates)

# change DateTime column to America/Chicago with force_tz
ba_dates <- ba_dates %>%
  mutate(DateTime = force_tz(DateTime, tzone = "America/Chicago"))

head(ba_dates$DateTime)

# create a wkday column in the ba_dates
ba_dates <- ba_dates %>%
  mutate(wkday = wday(DateTime, label = TRUE))

head(ba_dates)

# Create a bar chart of # tests by day of week
ggplot(ba_dates)+
  geom_bar(aes(x = wkday))

# 2. What makes Sunday so fun-day?
# Well, that wasn't terribly surprising: Friday and Saturday are two of the most common days of the week for breathalyzer tests in a college town. But what might be somewhat surprising is that more tests occur on Sunday than on Friday. But when on Sunday are these tests being administered? To investigate, we look at the hour of the test and compare this data for Friday, Saturady and Sunday.

# Create hour variable
ba_dates <- ba_dates %>%
  mutate(hr = hour(DateTime))

head(ba_dates)

# Make a new data frame, weekend, by filtering ba_dates to include only tests from Friday, Saturday, and Sunday.
weekend <- ba_dates %>%
  filter(wkday %in% c("Sun", "Fri", "Sat"))

head(weekend)

# Plot side by side bar charts counting hour of the day of tests for each weekend day 
ggplot(data = weekend)+
  geom_bar(aes(x = hr))+
  facet_wrap(~wkday)+
  scale_x_continuous(breaks = 1:12*2-1) # for ease of readability

# 3. Trends in testing over time
# We learned that most of the tests administered on Sundays are during early morning, from midnight to 5am. Strangely, the same pattern also exists on Friday mornings. (This is likely because Thursdays are "Mug Nights" in Ames, where you can get discounted drinks if you bring in the designated reusable mug.) Returning to the full dataset, we now explore the pattern of alcohol tests over the years. To look at the "bigger picture", let's count up the number of tests per day, and visualize the resulting time series using a line plot.

# Create a date column rounded to the nearest day. as.Date() is for the plot later
ba_dates <- ba_dates %>%
  mutate(date = as.Date(round_date(DateTime, unit = "day")))

# Count number of tests per day
ba_summary <- ba_dates %>%
  group_by(date) %>%
  tally()

# pip the result from above into a ggplot command that uses geom_line() to make a time series plot
ba_summary %>%
  ggplot(aes(x = date, y = n))+
  geom_line(alpha = 0.7)+ # change alpha for readability
  scale_x_date(date_breaks = "6 months")+
  theme(axis.text.x = element_text(angle = 30)) # make x-axis more readable

# 4. College football
# In the time series, we see many days that have zero breathalyzer tests administered. In the entire five year period, there were at most 8 tests in a day. There are many days with 3 or more tests in a day, and we wonder if the Iowa State football schedule may match up with some of those high test days. We next explore the Iowa State footabll schedule for 2013-2017. 

# read in the football data
isu_fb <- read_csv("datasets/isu_football.csv")
str(isu_fb)

# make Date a date variable
isu_fb <- isu_fb %>%
  mutate(Date = parse_date(Date, format = "%b %d, %Y"))

# Filter the ba_summary data to include only the dates in isu_fb
ba_fb <- ba_summary %>%
  filter(date %in% isu_fb$Date)

head(ba_fb)

# arrange ba_fb by number of tests from high to low and print first six rows
ba_fb %>%
  arrange(desc(n)) %>%
  top_n(6)

# 5. Home vs. away? Win vs. Lose?
# The most breathalyzer tests given on a football game day was on September 24, 2016. This was a home game against San Jose State that Iowa State won 44-10. The win/loss information is in the Res column in isu_fb. Could the home game win have led to some excessive celebrations that resulted in more breathalyzer tests than an away win or a home loss?

# Join ba_summary to isu_fb 
isu_fb2 <- isu_fb %>%
  left_join(ba_summary, by = c("Date" = "date"))

# change NAs to 0s
# There are several game dates with no breathalyzer test data. Use mutate and ifelse to change the NAs to 0s.
isu_fb2 <- isu_fb2 %>%
  mutate(n = ifelse(is.na(n)==TRUE, 
                    0, 
                    n))

# Create a bar char with n on the x-axis. Fill by Home and facet by Res (the game result) to see the conditions with the most breathalyzer tests
isu_fb2 %>%
  ggplot(aes(x = n)) +
  geom_bar(aes(fill = Home))+
  facet_grid(.~Res)

# 6. Monthly counts
# The football season typically lasts from September through November. As we just saw, Iowa State football has more losses than wins in the last few years. The men's basketball team, however, has traditionally been very successful. The basketball season usually lasts from November through March. We now investigate the number of breathalyzer tests by month to see if the basketball months have more tests than the football months. 

# Create a mo and a yr column in ba_dates
ba_dates <- ba_dates %>%
  mutate(mo = month(DateTime, label = TRUE), 
         yr = year(DateTime))

# Create a bar chart of number of tests per month.
ba_dates %>%
  ggplot(aes(x = mo))+
  geom_bar()

# Create the same bar chart again but color the bars according to year. 
ba_dates %>%
  ggplot(aes(x = mo, fill = as.factor(yr)))+
  geom_bar()

# 7. VEISHEA: an old tradition
# The monthly bar chars show that the months with the most test per day are August and April. April is a surprise because there are no major college sports in April, and students are busy studying for finals and finishing semester projects. Well, at Iowa State, there was a historical weeklong festival known as VEISHEA held in April every year. It was canceled in 2014 due to the many drining related arrests, violence and vandalism that occured yearly. Looking at the VEISHEA weeks and subsequent non-VEISHEA weeks, can we see the effect of the cancellation in the breathalyzer data?

# In 2013, VEISHEA was held from April 15-21. In 2014, it was held from April 7-13
v13 <- interval(make_date(year = 2013, month = 4, day = 15), 
                make_date(year = 2013, month = 4, day = 21), 
                tzone = "America/Chicago")
v14 <- interval(make_date(2014, 4, 7) , make_date(2014, 4, 13), tzone = "America/Chicago")
# Other comparable VEISHEA weeks in 2015-2017
v15 <- interval(make_date(2015, 4, 13) , make_date(2015, 4, 19), tzone = "America/Chicago")
v16 <- interval(make_date(2016, 4, 11) , make_date(2016, 4, 17), tzone = "America/Chicago")
v17 <- interval(make_date(2017, 4, 10) , make_date(2017, 4, 16), tzone = "America/Chicago")

# Filter ba_dates for only the 5 VEISHEA intervals
veishea <- ba_dates %>%
  filter(date %within% v13 | date %within% v14 | date %within% v15 | 
           date %within% v16 | date %within% v17)

# Count up years: Using count(), count the number of breathalyzer tests during each VEISHEA week
veishea %>%
  count(yr)

# 8. Looking at BAC
# Finally, let's look at the actual results of the breathalyzer tests, Based on our knowledge from Section 2, we suspect that the highest BAC resuts occur late and night and in the early morning, since those times are most common for tests on the weekends.

# Take a mean of res1 and res2
ba_dates <- ba_dates %>%
  mutate(res = (Res1+Res2)/2)

# Library the ggridges package
library(ggridges)

# Draw the ridgeline plot so that res is on the x-axis, and one density ridge is drawn for each hour
ggplot(data = ba_dates, aes(x = res, y = hr, group = hr))+
  geom_density_ridges(alpha = 0.7, fill = "steelblue", 
                      bandwidth = 0.01, rel_min_height = 0.0001)+
  scale_y_continuous(breaks = 0:23)

# 9. A more honest plot
# In the previous ridgeline plot, there are values below zero. This is impossible given the context: you cannot have negative alcohol concentration in your blood. We examine the zeros below and make a more honest ridgeline plot.

# Create a zero indicator variable
ba_dates <- ba_dates %>%
  mutate(zero = res == 0)

# Tabulate the data by the zero column
ba_dates %>%
  count(zero)

# redo ridge with no 0s
ba_dates %>% filter(zero == FALSE) %>% 
  ggplot(aes(x = res, y = hr, group = hr)) +
  geom_density_ridges(alpha = 0.7, fill = "steelblue", 
                      bandwidth = .01, rel_min_height = 0.005) + 
  scale_y_continuous(breaks = 0:23)

# 10. The dangers of binge drinking
# BAC of 0.31 or above is life-threatening with "significant risk of death"
# Create a new data frame, danger, that contains res of at least 0.31
danger <- ba_dates %>%
  filter(res >= 0.31)

# print danger
print(danger)





















