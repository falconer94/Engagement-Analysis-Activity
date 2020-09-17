library(tidyverse)


df <- read.csv("showwcase_sessions.csv")

# Engagement: Number of projects, likes, and comments over active time
  # number of sessions per customer

names(df)

# As factor login data, customer id, session id
df$session_id <- df$session_id %>% as.factor()
df$customer_id <- df$customer_id %>% as.factor()
df$login_date <- df$login_date %>% as.factor()


# Remove NA values
sum(is.na(df))
df <- na.omit(df)





# Select customer_id, session duration, number of projects, likes, comments
df2 <- 
  df %>% 
    select(customer_id, session_duration, session_projects_added, session_likes_given, session_comments_given)



### Create active duration column
#df2 <- df2 %>% 
 #mutate(active_duration = session_duration - inactive_duration)
#sum(df2$active_duration < 0)
### Since there are multiple values less than 0, session_duration must already be active time of session.



anyDuplicated(df$customer_id)
# some customers have more than one session;

# Group by customer id to get sums 

df3 <- df2 %>% 
  group_by(customer_id) %>% 
  summarize_each(funs = sum)

anyDuplicated(df3$customer_id)


# session duration seconds to minutes
df3 <- df3 %>% 
  mutate(session_duration_min = session_duration / 60)
  

# Mean of each customer
means <- df3 %>% select(!customer_id) %>% 
  colMeans()




#### Boxplots
# session duration
jpeg("duration.jpeg")
boxplot(df3$session_duration_min, main = "Average duration of each customer (min)")
dev.off()

# projects added
jpeg("projects.jpeg")
boxplot(df3$session_projects_added, main = "Average number of projects added by each customer")
dev.off()

# likes
jpeg("likes.jpeg")
boxplot(df3$session_likes_given, main = "Average number of likes given by each customer")
dev.off()

# comments
jpeg("comments.jpeg")
boxplot(df3$session_comments_given, "Average number of comments given by each customer")
dev.off()











