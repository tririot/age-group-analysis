races <- getData('/Users/gould/LX/src/repositories/Race-Analysis-Project/data/raw.Rdata')
races <- cleanCategories(races)
races <- cleanGenders(races)

x <- races %>% 
  filter(sport == 'overall',
         stime != 0)

mylevel <- 'sex'

y <- x %>% 
  group_by_at(mylevel) %>%
  summarize(n = n(),
            m_stime = mean(stime))

rows <- c(1,2)

levelsSelected = as.character(pull(y[rows,1]))
colName <- mylevel

#browser()
x <- x %>%
  filter_at(colName, all_vars(. %in% levelsSelected)) %>%
  mutate(ryear = year(rdate)) %>%
  group_by_at(vars('ryear', colName)) %>%
  summarize(n = n(),
            m_stime = mean(stime))
names(x)[2] = 'factor.level'
x

x.var <- 'ryear'
y.var <- 'n'

ggplot(x,
       aes(.data[[x.var]], .data[[y.var]])) +
  geom_point() +
  geom_line(aes(color = factor.level))
