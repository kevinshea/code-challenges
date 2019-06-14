library(tidyverse)


data.test <- read_csv("https://raw.githubusercontent.com/georgetown-analytics/nba/master/fixtures/nba_players.csv")


fit <- lm(PER ~ SALARY, nba.data.tidy)
my.rsquare <- summary(fit)$adj.r.squared
my.intercept <- signif(fit$coefficients[[1]], 3)
my.slope <- signif(fit$coefficients[[2]], 3)

my.formula <- paste0("y = ", my.slope, "x + ", my.intercept)

ggplotly(
  ggplot(nba.data.tidy, aes(x = PER, y = SALARY)) +
    geom_point(aes(text = PLAYER)) +
    geom_smooth(method = "lm") +
    theme_bw() +
    scale_x_continuous(labels = scales::comma) +
    ggtitle("NBA PER vs Salary")
)


nba.data.tidy$age.range <- cut(nba.data.tidy$AGE, 
                               breaks = c(-Inf, 22, 26, 29, 32, Inf), 
                               labels = c("Under 23", "23-25", "26-29", "30-32", "33+"))

ggplotly(
  ggplot(nba.data.tidy, aes(x = SALARY, y = PER)) +
    geom_point(aes(text = PLAYER, color = age.range)) +
    geom_smooth(method = "lm") +
    theme_bw() +
    scale_x_continuous(labels = scales::comma) +
    ggtitle("NBA PER vs Salary")
)


fit <- lm(PER ~ MPG + AST + USG + REBR , nba.data.tidy)
summary(fit)
