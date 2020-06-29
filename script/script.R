library(tidyverse)
library(MASS)
library(ggrepel)
library(Hmisc)

do_my_plot <- function(myr, mu){
  set.seed(1234)
  mysigma <- matrix(c(5, myr, myr, 50), nrow = 2, ncol = 2) 
  my_data <- as_tibble(mvrnorm(100, mu, mysigma))
  colnames(my_data) <- c("score", "study")
  
  my_data %>% 
    ggplot(aes(x = study, y = score)) +
    geom_point(size = 3, alpha = .5) +
    #geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Study Time", y = "Exam Performance") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
    theme(text = element_text(size = 12))
  

}

# mu = a vector with the means of each of the two variables
# For the below example to give us a Pearson's r of .5 
# we have covariance = .9 * sqrt(5) * sqrt(50) 
myr <- 14.23

do_my_plot(myr = myr, mu = c(80, 200))

my_df <- do_my_plot(myr, mu = c(80, 200))

my_df_with_id <- my_df %>% 
  rowid_to_column()

my_df_with_id %>% 
  ggplot(aes(x = rowid, y = score)) +
  geom_point(size = 3, alpha = .5) +
  labs(x = "Participant ID", y = "Exam Performance") +
  geom_label_repel(data = filter(my_df_with_id, rowid %in% c(1, 25, 50 ,100)), 
                   aes(label = rowid), nudge_x = -1, nudge_y = -1, colour = "red", ) +
  geom_hline(yintercept = mean(my_df_with_id$score)) +
  theme_minimal() +
  theme(
        axis.text.y = element_blank()) +
  theme(text = element_text(size = 12))

my_df_with_id %>% 
  ggplot(aes(x = rowid, y = study)) +
  geom_point(size = 3, alpha = .5) +
  labs(x = "Participant ID", y = "Study Time") +
  geom_label_repel(data = filter(my_df_with_id, rowid %in% c(1, 25, 50 ,100)), 
             aes(label = rowid), nudge_x = 3, nudge_y = -3, colour = "red", ) +
  geom_hline(yintercept = mean(my_df_with_id$study)) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank()) +
  theme(text = element_text(size = 12))

my_df_with_id$mean_study <- mean(my_df_with_id$study)
my_df_with_id$mean_score <- mean(my_df_with_id$score)
sd_study <- sd(my_df_with_id$study)
sd_score <- sd(my_df_with_id$score)

my_df_with_id$study_d <- my_df_with_id$study - my_df_with_id$mean_study
my_df_with_id$score_d <- my_df_with_id$score - my_df_with_id$mean_score

my_df_with_id$xbyy_d <- my_df_with_id$study_d * my_df_with_id$score_d

my_cov <- sum(my_df_with_id$xbyy_d)/(nrow(my_df_with_id) - 1)

my_r <- sum(my_df_with_id$xbyy_d)/((nrow(my_df_with_id) - 1) * (sd_study * sd_score))

rcorr(my_df_with_id$score, my_df_with_id$study)

# Regression plots
myr <- 14.23
mu <- c(80, 200)

set.seed(1234)
mysigma <- matrix(c(5, myr, myr, 50), nrow = 2, ncol = 2) 
my_data <- as_tibble(mvrnorm(8, mu, mysigma))
colnames(my_data) <- c("score", "study")

set.seed(1234)
my_data %>% 
  ggplot(aes(x = study, y = score)) +
  geom_point(size = 3, alpha = .5) +
  #geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Study Time (hours)", y = "Exam Performance") +
  ylim(74, 84) +
  theme_minimal() +
  #theme(axis.text.x = element_blank(),
  #      axis.text.y = element_blank()) +
  theme(text = element_text(size = 12))

set.seed(1234)
my_data %>% 
  ggplot(aes(x = study, y = score)) +
  geom_point(size = 3, alpha = .5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Study Time (hours)", y = "Exam Performance") +
  ylim(74, 84) +
  theme_minimal() +
  #theme(axis.text.x = element_blank(),
  #      axis.text.y = element_blank()) +
  theme(text = element_text(size = 12))

set.seed(1234)
my_data %>% 
  ggplot(aes(x = study, y = score)) +
  geom_point(size = 3, alpha = .5) +
  geom_hline(yintercept = mean(my_data$score), colour = "blue", size = 1) +
  labs(x = "Study Time (hours)", y = "Exam Performance") +
  ylim(74, 84) +
  theme_minimal() +
  #theme(axis.text.x = element_blank(),
  #      axis.text.y = element_blank()) +
  theme(text = element_text(size = 12))

set.seed(1234)
my_data %>% 
  ggplot(aes(x = study, y = score)) +
  geom_point(size = 3, alpha = .5) +
  geom_hline(yintercept = mean(my_data$score), colour = "blue", size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Study Time (hours)", y = "Exam Performance") +
  ylim(74, 84) +
  theme_minimal() +
  #theme(axis.text.x = element_blank(),
  #      axis.text.y = element_blank()) +
  theme(text = element_text(size = 12))
