library(dplyr)
library(ggplot2)

df = data.frame(
  group = c("A", "A", "B", "B"),
  value = c(10, 15,20,25)
)

summary_df = df %>%
  group_by(group) %>%
  summarise(mean_value = mean(value))

print(summary_df)

ggplot(df, aes(x = group, y = value)) + 
  geom_boxplot()

renv::snapshot() #helps to detect which pkg are used and save the details

library(tidyr)

renv::snapshot() #every time you add, remove, or update a pkg, you should re-run this

renv::restore()
