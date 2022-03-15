library(foreign)

mydata = read.dta("http://dss.princeton.edu/training/Panel101.dta")

# create pre and post intervention periods
mydata$time = ifelse(mydata$year >= 1994, 1, 0)

# create treatment and control groups
# treatment countries: E, F, G
# control countries: A, B, C, D 
mydata$treated = ifelse(mydata$country == "E" |
                            mydata$country == "F" |
                            mydata$country == "G", 1, 0)

# interaction term for DID
mydata$did = mydata$time * mydata$treated

mydata <- mydata %>%
            select(country, year, y, treated, time, did)

# plotting for DID using A and E
Rctmd::util_plt_line(df = mydata %>% filter(country %in% c("A","E")), 
                     x = year, y = y, color = country, x_refline = c(1994)
                    )

# estimating DID using full-regression
did_model1 <- lm(data = mydata, formula = y ~ treated + time + did)
summary(did_model1)

# estimating DID using mean values. Is this right?
mydata %>%
        group_by(treated, time) %>%
        summarize(mean = mean(y))
(1904056644 - 2134113623) - (2647598601 - 358143950)
# -2519511630, same as estimate obtained using regression

# treatment after experiment
mydata %>%
    filter(treated == 1, time == 1)
# treatment before experiment
mydata %>%
    filter(treated == 1, time == 0)
# control after experiment
mydata %>%
    filtertreated == 0, time == 1)
# control before experiment
mydata %>%
    filter(treated == 0, time == 0)

mydata %>%
    group_by(treated) %>%
    summarise(mean = mean(y))

mydata %>%
    group_by(time) %>%
    summarise(mean = mean(y))

