library(dplyr)
library(readxl)
library(ggplot2)

h<- read_excel("SEWS FFL MONTE CARLO.xlsx", sheet=3)

test3 <- h %>%
  filter(Owner == "Gallaway")
  
x1<-rnorm(1000, sd = sd(test$Score), mean = mean(test$Score))
y1<-dnorm(x,sd = sd(test$Score), mean = mean(test$Score))
plot(x,y)

summary(X)

h %>% ggplot(aes(x=Week, y=Score, color=Owner)) +
  geom_path()