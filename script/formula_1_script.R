library(tidyverse)
library(Hmisc) # Needed for correlation
library(performance) # Needed to check model assumptions

dataset1 <- read_csv("data/dataset1.csv") 

# First we will build a scatterplot of points against investment
set.seed(1234)
ggplot(dataset1, aes(x = investment, y = points)) + 
  geom_point() +
  labs(x = "Investment (in £100,000s)", y = "Points") +
  theme_minimal() +
  theme(text = element_text(size = 12)) +
  ylim(0,300)

# Let's add a regression line and a line of our outcome mean
set.seed(1234)
ggplot(dataset1, aes(x = investment, 
                     y = points)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Investment (in £100,000s)", 
       y = "Points") +
  theme_minimal() +
  theme(text = element_text(size = 12)) +
  ylim(0,300)

# Let’s calculate Pearson’s r
rcorr(dataset1$investment, dataset1$points)

# Let's do regression with just the one predictor
# We will first build a model where our outcome (points scored) is predicted 
#by the intercept of our line (i.e., the mean as a model of our data)
model0 <- lm(points ~ 1, data = dataset1)

# Now we will build a model where our outcome (points scored) is predicted by investment
model1 <- lm(points ~ investment, data = dataset1)

# Now we will compare the two models to each other
anova(model0, model1)

# Now let's get some parameter estimates
summary(model1)

# We can use the check_model() function from the performance package to
# check our model assumptions

check_model(model1)

cooks.distance.lm(model1)
