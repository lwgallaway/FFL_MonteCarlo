library(dplyr)
library(readxl)
library(ggplot2)

training_data<- read_excel("SEWS FFL MONTE CARLO.xlsx", sheet=3)
schedule_data<- read_excel("SEWS FFL MONTE CARLO.xlsx", sheet=4)



#thoughts
#make a blank schedule and then fill it in with data from the training set until it hits blanks
# then start generating the data to fill out the season. then create function to basically score the season
# pts for, W-L.
# probably use forloop to generate set of data for each ID


# sort team data into a set based on ID number
gather_team_stats<-function(training_data){
  team_data=list()

  for(i in 1:10){
    x <- training_data %>%
      filter(ID == i)

    team_data[i]<-x[,4]
    
  }
  
  return(team_data)
}


#creating a season template so a new doesnt have to be created for each new run
set_week_template<-function(training_data){
  template_schedule<-training_data
  
  for(i in (max(training_data[,1])+1):13){
    for(j in 1:10){
      x<-c(i,0,j,0)
      template_schedule<-rbind(template_schedule,x)
    }
  }
  
  return(template_schedule)
}

# creating a season full of scores. requires a filling in season template and team_data list
make_season<-function(season_template, team_data){
  
for(i in 1:nrow(season_template)){
  if(season_template[i,4] > 10){
    next
  }else{
    season_template[i,4]<- rnorm(1, mean = mean(team_data[[as.numeric(season_template[i,3])]]), 
                                 sd= sd(team_data[[as.numeric(season_template[i,3])]]))
  }
}
  return(season_template)
}














# test3 <- h %>%
#   filter(Owner == "Gallaway")
#   
# x1<-rnorm(1000, sd = sd(test$Score), mean = mean(test$Score))
# y1<-dnorm(x,sd = sd(test$Score), mean = mean(test$Score))
# plot(x,y)
# 
# summary(X)
# 
# h %>% ggplot(aes(x=Week, y=Score, color=Owner)) +
#   geom_path()