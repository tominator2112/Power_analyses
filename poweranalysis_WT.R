
rm(list=ls())

#BASE MODELL [suitable for experiments 2 and 4. Sex is included but the sex effect will not differ from the food effect)
################################
reps <-16##number of mice per sex per treatment combination
runs <-1000
stdev <-5 #variation around the mean for the response variable. 
baseline <- 50 #average level of males in food1, uninfected.  i.e. one of the cells
age <- 50
stdevAge <- 5

##vectors containing labels to use in producing data frames
treatments <-c("control", "plus", "minus")
food <-c("food 1", "food 2")
sexlabels <-c("male", "female")
ageeffect <- c('age')

##matrixes with the deviations from the grand mean for the
##different treatment combinations for males and females
##first bracket is the food treatment 1 deviations for 
##each level of 

#We observed that food supplementation can cause a shift in, for example, body weight 
#of ~ 0.75 standard deviations from the mean
# fear response studies observe effect sizes in the range of ~35%, so that would be rises of 17.5 in what follows
#in swim tests, it is more like 1.5 to 2 in cohens d units.  d = (M1 - M2) / s_pooled
#Where M1 and M2 are the means of the two groups, and s_pooled is the pooled standard deviation. But
#d can be interpreted in SD units: 
#d = 0.5 indicates the mean difference is half a standard deviation
#d = 1.0 indicates the mean difference is equal to one standard deviation
#d = 2.0 indicates the mean difference is twice the standard deviation
# that would be from 2.5 to 10 in what follows.
#if I went with the lower number reported from behavioural studies, it would be 1.5 or 7.5
 
#here shows no effect of infection, but an effect of food.With 5, the % change due to the effects is 
effect.male <- rbind(c(0,7.5,7.5), c(0,5,5))
effect.female <-rbind(c(0,5.5,5.5), c(0,4,4))

# this set up can be imagined as two food, three treatments and two cages.  
effect.male <- rbind(c(0,10,0), c(0,10,0))#here shows  effect of treatment, but no effect of food.  
effect.female <-rbind(c(0,10,0), c(0,10,0))


effect.age <- c()


##combine male and female matrixes into an array
effect.array <-array(c(effect.male, effect.female), dim = c(2,3,2), dimnames = list(c("food1", "food2"), c("control", "plus", "minus"), c("males", "female")))
print(effect.array)

##empty vectors to store p values
treatP <- c()
foodP <-c()#power for sex will be identical to this
sexP<-c()
foodtreatP<-c()
treatsexP <-c()
foodsexP<-c()
treatfoodsexP<-c()
ageP <- c()


##output array here so user can see what values are

##big loop will start here
for(run in 1:runs) {
  
  ##run specific vectors that will be rebuilt each time
  epi <- c()
  food <- c()
  treat <- c()
  sex <- c()
  age <- c()
  
  ##loops through the various combinations
  for(d in 1:3) {
    for(f in 1:2) {
      for(s in 1:2) {
        for(r in 1:reps) {
          
          ##generates the value for the rep and adds to the list
          epi <- append(epi, baseline + effect.array[f,d,s] + rnorm(1,0,stdev))
          age <- append(age, baseline + effect.array[f,d,s] + rnorm(1,0,stdevAge))
          
          ##add the right lables to lists to make up the data frame.
          treat <- append(treat, treatments[[d]])
          food <-append(food, treatments[[f]])
          sex <- append(sex, sexlabels[[s]])
          
        }
      }
    }
  }
  
  epi.data <-data.frame(age, treat, food, sex, epi)
  
  epi.data
  mean(epi.data$epi)
  
  model1 <- lm(epi~age+treat*food*sex, data = epi.data)
  treat.result <- anova(model1)
  treat.result
  treatP <-append(treatP, treat.result[[2,5]])
  foodP <-append(foodP, treat.result[[3,5]])
  sexP <-append(sexP, treat.result[[4,5]])
  foodtreatP <-append(foodtreatP, treat.result[[5,5]])
  foodsexP <-append(foodsexP, treat.result[[6,5]])
  treatfoodsexP <-append(treatfoodsexP, treat.result[[7,5]])
  ageP <-append(ageP, treat.result)
  ##bigloop ends here
}

##to estimate power check how manu items in each P value list are 
##less than 0.05

treat_power <- length(which(treatP<0.05))#what proportion of the 1000 runs detected a significant effect
food_power <- length(which(foodP<0.05))
sex_power <- length(which(sexP<0.05))
treatbyfood_power <- length(which(foodP<0.05))
foodbysex_power <- length(which(foodsexP<0.05))
treatfoodsex_power <- length(which(treatfoodsexP<0.05))

#for the inital set up food power will be near 1, and the others 0.5, which is the fals discovery rate
print(paste("treatment power =", (treat_power/runs)))
print(paste("food power =", (food_power/runs)))
print(paste("sex power =", (sex_power/runs)))
print(paste("treatment by food power =", (treatbyfood_power/runs)))
print(paste("food by sex power =", (foodbysex_power/runs)))
print(paste("treatment by food by sex power =", (treatfoodsex_power/runs)))



rm(list=ls())

#MODEL FOR EXPERIMENT 1 OR 3: MALES, FEMALES AND TWO TREATMENTS. N=12(EXP1), N=16(EXP3)

reps <-16##number of mice per sex per treatment combination
runs <-1000
stdev <-5 #variation around the mean for the response variable. 
baseline <- 50 #average level of males in food1, uninfected.  i.e. one of the cells
age <- 50
stdevAge <- 5

##vectors containing labels to use in producing data frames
treatments <-c("plus", "minus")
#food <-c("food 1", "food 2")
sexlabels <-c("male", "female")
ageeffect <- c('age')

##matrixes with the deviations from the grand mean for the
##different treatment combinations for males and females
##first bracket is the food treatment 1 deviations for 
##each level of 

#here shows no effect of infection, but an effect of food.With 5, the % change due to the effects is

effect.male <-rbind(c(0,7.5))
effect.female <-rbind(c(2.5,7.5))

effect.age <- c()


##combine male and female matrixes into an array

effect.array <-array(c(effect.male, effect.female), dim = c(2,2), dimnames = list(c("males", "females"), c("plus", "minus")))
print(effect.array)

##empty vectors to store p values
treatP <- c()
foodP <-c()#power for sex will be identical to this
sexP<-c()
treatsexP <-c()

ageP <- c()


##output array here so user can see what values are

##big loop will start here
for(run in 1:runs) {
  
  ##run specific vectors that will be rebuilt each time
  epi <- c()
  treat <- c()
  sex <- c()
  age <- c()
  
  ##loops through the various combinations
  
    for(f in 1:2) {
      for(d in 1:2) {
        for(r in 1:reps) {
          
          ##generates the value for the rep and adds to the list
          epi <- append(epi, baseline + effect.array[f,d] + rnorm(1,0,stdev))
          age <- append(age, baseline + effect.array[f,d] + rnorm(1,0,stdevAge))
          
          ##add the right lables to lists to make up the data frame.
          treat <- append(treat, treatments[[f]])
          sex <- append(sex, sexlabels[[d]])
          
        }
      }
    }
  
  
  epi.data <-data.frame(age, treat,  sex, epi)
  
  epi.data
  mean(epi.data$epi)
  
  model1 <- lm(epi~age+treat*sex, data = epi.data)
  treat.result <- anova(model1)
  treat.result
  treatP <-append(treatP, treat.result[[2,5]])
  sexP <-append(sexP, treat.result[[3,5]])
  treatsexP <-append(treatsexP, treat.result[[4,5]])
  ageP <-append(ageP, treat.result)
  ##bigloop ends here
}
epi.data
##to estimate power check how manu items in each P value list are 
##less than 0.05

treat_power <- length(which(treatP<0.05))#what proportion of the 1000 runs detected a significant effect
sex_power <- length(which(sexP<0.05))
treatbysex_power <- length(which(treatsexP<0.05))


#for the inital set up food power will be near 1, and the others 0.5, which is the fals discovery rate
print(paste("treatment power =", (treat_power/runs)))
print(paste("sex power =", (sex_power/runs)))
print(paste("treat by sex power =", (treatbysex_power/runs)))

########EXPERIMENTS 2 AND 4 ARE SINGLE SEX BUT THREE LEVELS OF TREATMENT N=16 TO ACCOUNT FOR HOUSING EFFECTS. 

reps <-16##number of mice per sex per treatment combination
runs <-1000
stdev <-5 #variation around the mean for the response variable. 
baseline <- 50 #average level of males in food1, uninfected.  i.e. one of the cells
age <- 50
stdevAge <- 5

##vectors containing labels to use in producing data frames
treatments <-c("control", "plus", "minus")
foodlables <-c("food 1", "food 2")
#sexlabels <-c("male", "female")
ageeffect <- c('age')

##matrixes with the deviations from the grand mean for the
##different treatment combinations for males and females
##first bracket is the food treatment 1 deviations for 
##each level of 

#We observed that food supplementation can cause a shift in, for example, body weight 
#of ~ 0.75 standard deviations from the mean
# fear response studies observe effect sizes in the range of ~35%, so that would be rises of 17.5 in what follows
#in swim tests, it is more like 1.5 to 2 in cohens d units.  d = (M1 - M2) / s_pooled
#Where M1 and M2 are the means of the two groups, and s_pooled is the pooled standard deviation. But
#d can be interpreted in SD units: 
#d = 0.5 indicates the mean difference is half a standard deviation
#d = 1.0 indicates the mean difference is equal to one standard deviation
#d = 2.0 indicates the mean difference is twice the standard deviation
# that would be from 2.5 to 10 in what follows.
#if I went with the lower number reported from behavioural studies, it would be 1.5 or 7.5

#here shows no effect of infection, but an effect of food.With 5, the % change due to the effects is 
effect.lowF <- rbind(c(0,7.5,7.5))
effect.highF <-rbind(c(0,5.5,5.5))
effect.age <- c()


##combine male and female matrixes into an array
effect.array <-array(c(effect.lowF, effect.highF), dim = c(3,2), dimnames = list(c("control", "plus", "minus"), c("lowF", "highF")))
print(effect.array)

##empty vectors to store p values
treatP <- c()
foodP <-c()
foodtreatP<-c()
ageP <- c()


##output array here so user can see what values are

##big loop will start here
for(run in 1:runs) {
  
  ##run specific vectors that will be rebuilt each time
  epi <- c()
  food <- c()
  treat <- c()
  sex <- c()
  age <- c()
  
  ##loops through the various combinations
  for(d in 1:3) {
      for(s in 1:2) {
        for(r in 1:reps) {
          
          ##generates the value for the rep and adds to the list
          epi <- append(epi, baseline + effect.array[d,s] + rnorm(1,0,stdev))
          age <- append(age, baseline + effect.array[d,s] + rnorm(1,0,stdevAge))
          
          ##add the right lables to lists to make up the data frame.
          treat <- append(treat, treatments[[d]])
          food <-append(food, treatments[[f]])
          sex <- append(sex, sexlabels[[s]])
          
        }
      }
    }

  
  epi.data <-data.frame(age, treat, sex, epi)
  
  epi.data
  mean(epi.data$epi)
  
  model1 <- lm(epi~age+treat*food*sex, data = epi.data)
  treat.result <- anova(model1)
  treat.result
  treatP <-append(treatP, treat.result[[2,5]])
  foodP <-append(foodP, treat.result[[3,5]])
  sexP <-append(sexP, treat.result[[4,5]])
  foodtreatP <-append(foodtreatP, treat.result[[5,5]])
  foodsexP <-append(foodsexP, treat.result[[6,5]])
  treatfoodsexP <-append(treatfoodsexP, treat.result[[7,5]])
  ageP <-append(ageP, treat.result)
  ##bigloop ends here
}

##to estimate power check how manu items in each P value list are 
##less than 0.05

treat_power <- length(which(treatP<0.05))#what proportion of the 1000 runs detected a significant effect
food_power <- length(which(foodP<0.05))
sex_power <- length(which(sexP<0.05))
treatbyfood_power <- length(which(foodP<0.05))
foodbysex_power <- length(which(foodsexP<0.05))
treatfoodsex_power <- length(which(treatfoodsexP<0.05))

#for the inital set up food power will be near 1, and the others 0.5, which is the fals discovery rate
print(paste("treatment power =", (treat_power/runs)))
print(paste("food power =", (food_power/runs)))
print(paste("sex power =", (sex_power/runs)))
print(paste("treatment by food power =", (treatbyfood_power/runs)))
print(paste("food by sex power =", (foodbysex_power/runs)))
print(paste("treatment by food by sex power =", (treatfoodsex_power/runs)))




#ASSESSING CAGE EFFECtS (INSTEAD OF JUST USING SEX, WHICH IS DF=2 COMPARED TO 8)
rm(list=ls())

#For the repro and vaccination experiments, it would be better to imagine sex as cage effects
reps <-16##number of mice per sex per treatment combination
runs <-1000
stdev <-5 #variation around the mean for the response variable. 
baseline <- 50 #average level of males in food1, uninfected.  i.e. one of the cells
age <- 50
stdevAge <- 5

##vectors containing labels to use in producing data frames
treatments <-c("control", "plus", "minus")
food <-c("food 1", "food 2")
sexlabels <-c("cage1", "cage2", "cage3","cage4", "cage5","cage6", "cage7", "cage8")
ageeffect <- c('age')

##matrixes with the deviations from the grand mean for the
##different treatment combinations for males and females
##first bracket is the food treatment 1 deviations for 
##each level of 

#We observed that food supplementation can cause a shift in, for example, body weight 
#of ~ 0.75 standard deviations from the mean
# fear response studies observe effect sizes in the range of ~35%, so that would be rises of 17.5 in what follows
#in swim tests, it is more like 1.5 to 2 in cohens d units.  d = (M1 - M2) / s_pooled
#Where M1 and M2 are the means of the two groups, and s_pooled is the pooled standard deviation. But
#d can be interpreted in SD units: 
#d = 0.5 indicates the mean difference is half a standard deviation
#d = 1.0 indicates the mean difference is equal to one standard deviation
#d = 2.0 indicates the mean difference is twice the standard deviation
# that would be from 2.5 to 10 in what follows.
#if I went with the lower number reported from behavioural studies, it would be 1.5 or 7.5

#here shows no effect of infection, but an effect of food.With 5, the % change due to the effects is 
effect.cage1 <- rbind(c(0,7.5,7.5), c(0,5,5))
effect.cage2 <-rbind(c(0,5.5,5.5), c(0,4,4))
effect.cage3 <- rbind(c(0,7.5,7.5), c(0,5,5))
effect.cage4 <-rbind(c(0,5.5,5.5), c(0,4,4))
effect.cage5 <- rbind(c(0,7.5,7.5), c(0,5,5))
effect.cage6 <-rbind(c(0,5.5,5.5), c(0,4,4))
effect.cage7 <- rbind(c(0,7.5,7.5), c(0,5,5))
effect.cage8 <-rbind(c(0,5.5,5.5), c(0,4,4))



effect.age <- c()


##combine  matrixes into an array


effect.array <-array(c(effect.cage1, effect.cage2, effect.cage3, effect.cage4, effect.cage5, effect.cage6, effect.cage7, effect.cage8), dim = c(2,3,8), dimnames = list(c("food1", "food2"), c("control", "plus", "minus"), c("cage1", "cage2","cage3", "cage4",
                                                                                                           "cage5", "cage6", "cage7", "cage8")))
print(effect.array)

##empty vectors to store p values
treatP <- c()
foodP <-c()#power for sex will be identical to this
sexP<-c()
foodtreatP<-c()
treatsexP <-c()
foodsexP<-c()
treatfoodsexP<-c()
ageP <- c()


##output array here so user can see what values are

##big loop will start here
for(run in 1:runs) {
  
  ##run specific vectors that will be rebuilt each time
  epi <- c()
  food <- c()
  treat <- c()
  sex <- c()
  age <- c()
  
  ##loops through the various combinations
  for(d in 1:3) {
    for(f in 1:2) {
      for(s in 1:8) {
        for(r in 1:reps) {
          
          ##generates the value for the rep and adds to the list
          epi <- append(epi, baseline + effect.array[f,d,s] + rnorm(1,0,stdev))
          age <- append(age, baseline + effect.array[f,d,s] + rnorm(1,0,stdevAge))
          
          ##add the right lables to lists to make up the data frame.
          treat <- append(treat, treatments[[d]])
          food <-append(food, treatments[[f]])
          sex <- append(sex, sexlabels[[s]])
          
        }
      }
    }
  }
  
  epi.data <-data.frame(age, treat, food, sex, epi)
  

  mean(epi.data$epi)
  
  model1 <- lm(epi~age+treat*food*sex, data = epi.data)
  treat.result <- anova(model1)
  treat.result
  treatP <-append(treatP, treat.result[[2,5]])
  foodP <-append(foodP, treat.result[[3,5]])
  sexP <-append(sexP, treat.result[[4,5]])
  foodtreatP <-append(foodtreatP, treat.result[[5,5]])
  foodsexP <-append(foodsexP, treat.result[[6,5]])
  treatfoodsexP <-append(treatfoodsexP, treat.result[[7,5]])
  ageP <-append(ageP, treat.result)
  ##bigloop ends here
}
treat.result
##to estimate power check how manu items in each P value list are 
##less than 0.05

treat_power <- length(which(treatP<0.05))#what proportion of the 1000 runs detected a significant effect
food_power <- length(which(foodP<0.05))
sex_power <- length(which(sexP<0.05))
treatbyfood_power <- length(which(foodP<0.05))
foodbysex_power <- length(which(foodsexP<0.05))
treatfoodsex_power <- length(which(treatfoodsexP<0.05))

#for the inital set up food power will be near 1, and the others 0.5, which is the fals discovery rate
print(paste("treatment power =", (treat_power/runs)))
print(paste("food power =", (food_power/runs)))
print(paste("sex power =", (sex_power/runs)))
print(paste("treatment by food power =", (treatbyfood_power/runs)))
print(paste("food by sex power =", (foodbysex_power/runs)))
print(paste("treatment by food by sex power =", (treatfoodsex_power/runs)))