# https://www.kaggle.com/headsortails/tidy-titarnic

library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('grid') # visualisation

#install.packages('gridExtra')
library('gridExtra') # visualisation

#install.packages('corrplot')
library('corrplot') # visualisation

#install.packages('VIM')
library('VIM') # missing values
#suppressPackageStartupMessages(library(heatmaply)) # visualisation
library('dplyr') # data manipulation

#install.packages('readr')
library('readr') # data input

library('stringr') # string manipulation

#install.packages('forcats')
library('forcats') # factor manipulation

# install.packages('modelr')
library('modelr') # factor manipulation

library('randomForest') # classification

# install.packages('xgboost')
library('xgboost') # classification

# install.packages('ROCR')
library('ROCR') # model validation

# install.packages('eponymous')
library('eponymous')

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# ===================== Libraries uploaded=================

setwd('/Users/songminseok/R/titanic')

train <- read_csv('datasets/train.csv')
test <- read_csv('datasets/test.csv')

# Factor - changing String values to int.
train <- train %>% mutate(
  Survived = factor(Survived),
  Pclass = factor(Pclass),
  Embarked = factor(Embarked),
  Sex = factor(Sex)
)

test <- test %>% mutate(
  Pclass = factor(Pclass),
  Embarked = factor(Embarked),
  Sex = factor(Sex)
)

combine <- bind_rows(train, test)

summary(combine)
glimpse(combine)

# %>% is a pipe like '|' in Linux. Pass the output as an input to the next command.
train %>%
  count(Survived)

# Count and rank variables based on the number of the missed.
aggr(combine, prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE)

# For the text features, Cabin and Ticket, we use a boolean vectors.
sum(is.na(combine$Ticket))
sum(is.na(combine$Cabin))


# plotting
p_age = ggplot(train) +
  geom_freqpoly(mapping = aes(x = Age, color = Survived), binwidth = 1) +
  guides(fill=FALSE) +
  theme(legend.position = "none")

p_sex = ggplot(train, mapping = aes(x = Sex, fill = Survived)) +
  geom_bar(stat='count', position='fill') + 
  labs(x = 'Sex') +
  scale_fill_discrete(name="Surv") +
  theme_few()

p_class = ggplot(train, mapping = aes(x = Pclass, fill = Survived, colour = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Pclass') + 
  theme(legend.position = "none")

p_emb = ggplot(train, aes(Embarked, fill = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Embarked') +
  theme(legend.position = "none")

p_sib = ggplot(train, aes(SibSp, fill = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'SibSp') +
  theme(legend.position = "none")

p_par = ggplot(train, aes(Parch, fill = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Parch') +
  theme(legend.position = "none")

p_fare = ggplot(train) +
  geom_freqpoly(mapping = aes(Fare, color = Survived), binwidth = 0.05) +
  scale_x_log10() +
  theme(legend.position = "none")

layout <- matrix(c(1,1,2,3,3,4,5,6,7),3,3,byrow = TRUE)
multiplot(p_age, p_sex, p_fare, p_class, p_emb, p_sib, p_par, layout=layout)

train %>%
  group_by(Survived) %>%
  summarise(median_age = median(Age, na.rm=TRUE))

train %>%
  group_by(Survived, Sex) %>%
  count(Sex)

train %>% mutate(single = SibSp==0) %>% count(single) %>% group_by(single) %>% mutate(freq = n/nrow(train)) 

train %>% mutate(single = Parch==0) %>% count(single) %>% group_by(single) %>% mutate(freq = n/nrow(train))

train %>%
  select(-PassengerId, -Name, -Cabin, -Ticket) %>%
  mutate(Sex = fct_recode(Sex,
                          "0" = "male",
                          "1" = "female")
  ) %>% mutate(Sex = as.integer(Sex),
         Pclass = as.integer(Pclass),
         Survived = as.integer(Survived),
         Embarked = as.integer(Embarked)) %>% cor(use="complete.obs") %>%
  corrplot(type="lower", diag=FALSE)

ggplot(train, aes(Pclass, Fare, colour = Survived)) +
  geom_boxplot() +
  scale_y_log10()

train %>%
  ggplot(aes(Fare, fill=Pclass)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  facet_wrap(~ Survived, ncol = 1)

train %>%
  filter(Embarked %in% c("S","C","Q")) %>%
  ggplot() +
  geom_bar(aes(Embarked, fill = Pclass), position = "dodge") +
  facet_grid(~ Survived)

train %>%
  filter(Embarked %in% c("S","C","Q")) %>%
  ggplot(mapping = aes(Age, Fare, color = Survived, shape = Sex)) +
  geom_point() +
  scale_y_log10() +
  facet_grid(Pclass ~ Embarked)

ggplot(train, aes(x=Age)) +
  geom_density(aes(fill = Survived), alpha = 0.5) +
  facet_wrap(~Sex)

train %>%
  ggplot(aes(Parch, SibSp, color = Survived)) +
  geom_count()

train %>%
  ggplot() +
  geom_bar(aes(Parch, fill = Sex), position = 'dodge') +
  scale_y_log10()

train %>%
  group_by(Parch, Sex) %>%
  count()

binom.test(1, 5, p = 577/(577+314))
dbinom(4, size=12, prob=0.2)

train %>%
  mutate(SibSp = factor(SibSp)) %>%
  ggplot(aes(x=Age, color = SibSp)) +
  geom_density(size = 1.5)

combine %>% filter(is.na(Embarked))

combine %>%
  filter(Embarked != "Q" & Pclass == 1 & Sex == "female") %>%
  group_by(Embarked, Pclass, Sex, Pclass, Parch, SibSp) %>%
  summarise(count = n())

combine <- combine %>%
  mutate(Embarked = as.character(Embarked)) %>%
  mutate(Embarked = case_when(
    is.na(Embarked) ~ "C",
    TRUE ~ Embarked
  )) %>% mutate(Embarked = as.factor(Embarked))

print(filter(combine, is.na(Fare)), width = Inf)
combine[with(combine, is.na(Embarked)),]$Embarked <- as.character('C')
filter(combine, PassengerId %in% c(62, 830))

med_fare_3 <- combine %>%
  filter(!is.na(Fare)) %>%
  group_by(Pclass) %>%
  summarise(med_fare = median(Fare)) %>%
  filter(Pclass == 3) %>%
  .$med_fare

combine[with(combine, PassengerId==1044),]$Fare <- med_fare_3
combine <- combine %>% mutate(case_when(is.na(combine$Fare) ~ med_fare_3,TRUE ~ combine$Fare))

combine <- mutate(combine,
                  fclass = factor(log10(Fare+1) %/% 1),
                  age_known = factor(!is.na(Age)),
                  cabin_known = factor(!is.na(Cabin)),
                  title_orig = factor(str_extract(Name, "[A-Z][a-z]*\\.")),
                  young = factor( if_else(Age<=30, 1, 0, missing = 0) | (title_orig %in% c('Master.','Miss.','Mlle.')) ),
                  child = Age<10,
                  family = SibSp + Parch,
                  alone = (SibSp == 0) & (Parch == 0),
                  large_family = (SibSp > 2) | (Parch > 3),
                  deck = if_else(is.na(Cabin),"U",str_sub(Cabin,1,1)),
                  ttype = str_sub(Ticket,1,1)
)

combine <- mutate(combine, title = fct_lump(title_orig, n=4))

train <- combine %>% filter(!is.na(Survived))
test <- combine %>% filter(is.na(Survived))

p1 <- train %>%
  ggplot(aes(age_known, fill = Survived)) +
  geom_bar(position = "fill")
