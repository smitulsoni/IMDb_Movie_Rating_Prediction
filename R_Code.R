#install.packages('readxl')
library('readxl')
films_test = read.csv("C:/Users/sonis/Desktop/Courses/Fall/MGSC661/IMDB Rating/40_films_dataset_fall_2021.csv")
films = read_excel("C:/Users/sonis/Desktop/Courses/Fall/MGSC661/IMDB Rating/films_fall_2021_training_data.xlsx")
View(films_test)
attach(films)
names(films)
View(films)

#### Exploratory Analysis ####

# Numerical variables
# Boxplots
par(mfrow = c(1,5))
boxplot(budget_in_millions, xlab = "budget_in_millions", col = "violet")
boxplot(month_of_release, xlab = "month_of_release", col = "purple")
boxplot(year_of_release, xlab = "year_of_release", col = "blue")
boxplot(duration_in_hours, xlab = "duration_in_hours", col = "green")
boxplot(total_number_languages, xlab = "total_number_languages", col = "yellow")
boxplot(total_number_of_actors, xlab = "total_number_of_actors", col = "orange")
boxplot(total_number_of_directors, xlab = "total_number_of_directors", col = "red")
boxplot(total_number_of_producers, xlab = "total_number_of_producers", col = "gray")
boxplot(total_number_of_production_companies, xlab = "total_number_of_production_companies", col = "purple")
boxplot(total_number_of_production_countries, xlab = "total_number_of_production_countries", col = "maroon")

# Histograms
par(mfrow = c(1,5))
hist(budget_in_millions,xlab = "budget_in_millions", col = "violet", main = NULL)
hist(month_of_release, xlab = "month_of_release", col = "purple", main = NULL)
hist(year_of_release, xlab = "year_of_release", col = "blue", main = NULL)
hist(duration_in_hours, xlab = "duration_in_hours", col = "green", main = NULL)
hist(total_number_languages, xlab = "total_number_languages", col = "yellow", main = NULL)
hist(total_number_of_actors, xlab = "total_number_of_actors", col = "orange", main = NULL)
hist(total_number_of_directors, xlab = "total_number_of_directors", col = "red", main = NULL)
hist(total_number_of_producers, xlab = "total_number_of_producers", col = "gray", main = NULL)
hist(total_number_of_production_companies, xlab = "total_number_of_production_companies", col = "purple", main = NULL)
hist(total_number_of_production_countries, xlab = "total_number_of_production_countries", col = "maroon", main = NULL)


## Individual Regression

#install.packages("lmtest")
require(lmtest)  
#install.packages("plm")
require(plm)    

library(car)
library(boot)
par(mfrow = c(3,4))

#budget_in_millions
plot(budget_in_millions, imdb_score, col="#E1AD01")
lm.fit=lm(imdb_score~budget_in_millions)
abline(lm.fit)
summary(lm.fit)
ncvTest(lm.fit)

#month_of_release
plot(month_of_release, imdb_score, col="#E1AD01")
lm.fit=lm(imdb_score~month_of_release)
abline(lm.fit)
summary(lm.fit)
ncvTest(lm.fit)

#year_of_release
plot(year_of_release, imdb_score, col="#E1AD01")
lm.fit=lm(imdb_score~year_of_release)
abline(lm.fit)
summary(lm.fit)
ncvTest(lm.fit)

#duration_in_hours
plot(duration_in_hours, imdb_score, col="#E1AD01")
lm.fit=lm(imdb_score~duration_in_hours)
abline(lm.fit)
summary(lm.fit)
ncvTest(lm.fit)

#total_number_languages
plot(total_number_languages, imdb_score, col="#E1AD01")
lm.fit=lm(imdb_score~total_number_languages)
abline(lm.fit)
summary(lm.fit)
ncvTest(lm.fit)

#main_actor1_is_female
plot(main_actor1_is_female, imdb_score, col="#E1AD01")
lm.fit=lm(imdb_score~main_actor1_is_female)
abline(lm.fit)
summary(lm.fit)
ncvTest(lm.fit)

#main_actor2_is_female
plot(main_actor2_is_female, imdb_score, col="#E1AD01")
lm.fit=lm(imdb_score~main_actor2_is_female)
abline(lm.fit)
summary(lm.fit)

#main_actor3_is_female
plot(main_actor3_is_female, imdb_score, col="#E1AD01")
lm.fit=lm(imdb_score~main_actor3_is_female)
abline(lm.fit)
summary(lm.fit)

#total_number_of_actors
plot(total_number_of_actors, imdb_score, col="#E1AD01")
lm.fit=lm(imdb_score~total_number_of_actors)
abline(lm.fit)
summary(lm.fit)

#total_number_of_directors
plot(total_number_of_directors, imdb_score, col="#E1AD01")
lm.fit=lm(imdb_score~total_number_of_directors)
abline(lm.fit)
summary(lm.fit)

#total_number_of_producers
plot(total_number_of_producers, imdb_score, col="#E1AD01")
lm.fit=lm(imdb_score~total_number_of_producers)
abline(lm.fit)
summary(lm.fit)

#total_number_of_production_companies
plot(total_number_of_production_companies, imdb_score, col="#E1AD01")
lm.fit=lm(imdb_score~total_number_of_production_companies)
abline(lm.fit)
summary(lm.fit)

#total_number_of_production_countries
plot(total_number_of_production_countries, imdb_score, col="#E1AD01")
lm.fit=lm(imdb_score~total_number_of_production_countries)
abline(lm.fit)
summary(lm.fit)




# Tukey test for plotting residuals
library(car)
mreg= lm(imdb_score ~ budget_in_millions + month_of_release + 
           year_of_release + duration_in_hours + total_number_languages +
           main_actor1_is_female + main_actor2_is_female + 
           main_actor3_is_female + total_number_of_actors +
           main_director_is_female + total_number_of_directors +
           total_number_of_producers + total_number_of_production_companies +
           total_number_of_production_countries, data=films)
residualPlots(mreg)




# Correlation Matrix for identifying collinearity
install.packages("metan")
library(metan)

films1 = films[c("budget_in_millions", "month_of_release","year_of_release","duration_in_hours","total_number_languages"
                 ,"total_number_of_actors","total_number_of_directors","total_number_of_producers",
                 "total_number_of_production_companies","total_number_of_production_countries")]

corrl<-corr_coef(films1)
plot(corrl)


###################################################################################################################################

#### MODEL SELECTION ####

# Removing heteroskedasticity and selecting relevant predictors

library(car)
library(boot)
library(plm)
library(lmtest)

films<-films[,
             c('imdb_score',
               'genre_action',
               'genre_animation',
               'genre_comedy',
               'genre_documentary',
               'genre_family',
               'genre_filmnoir',
               'genre_horror',
               'genre_musical',
               'genre_scifi',
               'genre_sport',
               'genre_war',
               'main_actor1_name',
               'main_actor2_name',
               'main_actor3_name',
               'total_number_of_actors',
               'main_director_is_female',
               'main_producer_name',
               'editor_name',
               'total_number_of_production_companies',
               'total_number_of_production_countries',
               'budget_in_millions',
               'year_of_release',
               'main_lang',
               'genre_adventure',
               'genre_biography',
               'genre_crime',
               'genre_drama',
               'genre_fantasy',
               'genre_history',
               'genre_music',
               'genre_mystery',
               'genre_romance',
               'genre_thriller',
               'genre_western',
               'main_actor1_is_female',
               'main_actor2_is_female',
               'main_actor3_is_female',
               'main_director_name',
               'total_number_of_directors',
               'total_number_of_producers',
               'main_production_company',
               'main_production_country',
               'month_of_release',
               'duration_in_hours',
               'total_number_languages'
             )]

attach(films)

names<-list()
with_hsk<-list()
without_hsk<-list()
significance<-list()

for(i in 1:ncol(films))
{
  mreg_hsk=lm(films$imdb_score~films[[i]],data=films)
  names[[i]]<-names(films)[i]
  print(names[[i]])
  with_hsk[[i]]<-ncvTest(mreg_hsk)$p
  print(with_hsk[[i]])
  without_hsk[[i]]<-coeftest(mreg_hsk, vcov=vcovHC(mreg_hsk, type="HC1"))[[8]]
  print(without_hsk[[i]])
  significance[[i]]<-without_hsk[[i]]<0.05
}


# only significant predictors are chosen from the table below for creating a model
hsk<-do.call(rbind, Map(data.frame, A=names, B=with_hsk, C=without_hsk,D=significance))
colnames(hsk) <- c("Name", "With heteroskedasticity","Without heteroskedasticity","Significance")



# Identifying interaction variables (to check pairs that interact and can influence the model)

par(mfrow=c(1,1))
# duration_in_hours vs genre_drama
int1=lm(imdb_score~duration_in_hours+genre_drama+duration_in_hours*genre_drama)

b0=coef(int1)[1]
b1=coef(int1)[2]
b2=coef(int1)[3]
b3=coef(int1)[4]

plot(x=duration_in_hours, y=imdb_score, col=ifelse("red", "blue"), main="genre_drama vs duration_in_hours interaction plot")
abline(b0+b2,b1+b3, col= "red")
abline(b0, b1, col= "blue")


# genre_romance vs genre_drama
int1=lm(imdb_score~genre_romance+genre_drama+genre_romance*genre_drama)

b0=coef(int1)[1]
b1=coef(int1)[2]
b2=coef(int1)[3]
b3=coef(int1)[4]

plot(x=genre_romance, y=imdb_score, col=ifelse("red", "blue"), main="genre_drama vs genre_romance interaction plot")
abline(b0+b2,b1+b3, col= "red")
abline(b0, b1, col= "blue")


# genre_thriller vs genre_drama
int1=lm(imdb_score~genre_thriller+genre_drama+genre_thriller*genre_drama)

b0=coef(int1)[1]
b1=coef(int1,)[2]
b2=coef(int1)[3]
b3=coef(int1)[4]

plot(x=genre_thriller, y=imdb_score, col=ifelse("red", "blue"), main="genre_drama vs genre_thriller interaction plot")
abline(b0+b2,b1+b3, col= "red")
abline(b0, b1, col= "blue")


# genre_horror vs genre_drama
int1=lm(imdb_score~genre_horror+genre_drama+genre_horror*genre_drama)

b0=coef(int1)[1]
b1=coef(int1)[2]
b2=coef(int1)[3]
b3=coef(int1)[4]

plot(x=genre_horror, y=imdb_score, col=ifelse("red", "blue"), main="genre_drama vs genre_horror interaction plot")
abline(b0+b2,b1+b3, col= "red")
abline(b0, b1, col= "blue")


# genre_animation vs genre_fantasy
int1=lm(imdb_score~genre_animation+genre_fantasy+genre_animation*genre_fantasy)

b0=coef(int1)[1]
b1=coef(int1)[2]
b2=coef(int1)[3]
b3=coef(int1)[4]

plot(x=genre_animation, y=imdb_score, col=ifelse("red", "blue"), main="genre_animation vs genre_fantasy interaction plot")
abline(b0+b2,b1+b3, col= "red")
abline(b0, b1, col= "blue")


# genre_fantasy vs genre_adventure
int1=lm(imdb_score~genre_fantasy+genre_adventure+genre_fantasy*genre_adventure)

b0=coef(int1)[1]
b1=coef(int1)[2]
b2=coef(int1)[3]
b3=coef(int1)[4]

plot(x=genre_fantasy, y=imdb_score, col=ifelse("red", "blue"), main="genre_fantasy vs genre_adventure interaction plot")
abline(b0+b2,b1+b3, col= "red")
abline(b0, b1, col= "blue")


# year_of_release vs genre_drama
int1=lm(imdb_score~year_of_release+genre_drama+year_of_release*genre_drama)

b0=coef(int1)[1]
b1=coef(int1)[2]
b2=coef(int1)[3]
b3=coef(int1)[4]

plot(x=year_of_release, y=imdb_score, col=ifelse("red", "blue"), main="genre_drama vs year_of_release interaction plot")
abline(b0+b2,b1+b3, col= "red")
abline(b0, b1, col= "blue")


# year_of_release vs genre_comedy
int1=lm(imdb_score~year_of_release+genre_comedy+year_of_release*genre_comedy)

b0=coef(int1)[1]
b1=coef(int1)[2]
b2=coef(int1)[3]
b3=coef(int1)[4]

plot(x=year_of_release, y=imdb_score, col=ifelse("red", "blue"), main="genre_comedy vs year_of_release interaction plot")
abline(b0+b2,b1+b3, col= "red")
abline(b0, b1, col= "blue")


# year_of_release vs genre_comedy
int1=lm(imdb_score~year_of_release+genre_action+year_of_release*genre_action)

b0=coef(int1)[1]
b1=coef(int1)[2]
b2=coef(int1)[3]
b3=coef(int1)[4]

plot(x=year_of_release, y=imdb_score, col=ifelse("red", "blue"), main="genre_action vs year_of_release interaction plot")
abline(b0+b2,b1+b3, col= "red")
abline(b0, b1, col= "blue")



# Selecting polynomial degrees for non-linear predictors

# Method 1 : Finding degree for each predictor through calculation of minimum MSE

min_mse_which1=rep(NA,4)
min_mse1=NA

for (a in 1:5){
  for (b in 1:5){
    for (c in 1:5){
      for (d in 1:5){
        fit=glm(imdb_score~poly(budget_in_millions, a) + poly(year_of_release, b) + poly(duration_in_hours, c)
                + poly(total_number_languages, 1) + poly(total_number_of_actors, d) + poly(total_number_of_directors, 1) + 
                  poly(month_of_release, 3) + genre_action
                + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
                  genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
                  genre_scifi + genre_war + genre_western + 
                  main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
                +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
                  (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
                  (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action),
                data=films
        )
        
        if  ((cv.glm(films,fit,K=10)$delta[1] < min_mse1 ) || is.na(min_mse1)) {
          min_mse1 = cv.glm(films,fit,K=10)$delta[1]
          min_mse_which1 = c(a,b,c,d)
        }
      }
    }
  }
}

min_mse_which1
min_mse1


# Method 2 : Finding degree for each predictor through ANOVA test on each predictor

#budget_in_millions
reg1=lm(imdb_score~budget_in_millions)
reg2=lm(imdb_score~poly(budget_in_millions,2))
reg3=lm(imdb_score~poly(budget_in_millions,3))
reg4=lm(imdb_score~poly(budget_in_millions,4))
reg5=lm(imdb_score~poly(budget_in_millions,5))
anova(reg1, reg2, reg3, reg4, reg5)

lm.fit=lm(imdb_score~poly(budget_in_millions,4))
summary(lm.fit)

#year_of_release
reg1=lm(imdb_score~year_of_release)
reg2=lm(imdb_score~poly(year_of_release,2))
reg3=lm(imdb_score~poly(year_of_release,3))
reg4=lm(imdb_score~poly(year_of_release,4))
reg5=lm(imdb_score~poly(year_of_release,5))
anova(reg1, reg2, reg3, reg4, reg5)

lm.fit=lm(imdb_score~poly(year_of_release,3))
summary(lm.fit)

#duration_in_hours
reg1=lm(imdb_score~duration_in_hours)
reg2=lm(imdb_score~poly(duration_in_hours,2))
reg3=lm(imdb_score~poly(duration_in_hours,3))
reg4=lm(imdb_score~poly(duration_in_hours,4))
reg5=lm(imdb_score~poly(duration_in_hours,5))
anova(reg1, reg2, reg3, reg4, reg5)

lm.fit=lm(imdb_score~poly(duration_in_hours,4))
summary(lm.fit)

#total_number_languages
reg1=lm(imdb_score~total_number_languages)
reg2=lm(imdb_score~poly(total_number_languages,2))
reg3=lm(imdb_score~poly(total_number_languages,3))
reg4=lm(imdb_score~poly(total_number_languages,4))
reg5=lm(imdb_score~poly(total_number_languages,5))
anova(reg1, reg2, reg3, reg4, reg5)

lm.fit=lm(imdb_score~total_number_languages)
summary(lm.fit)

#total_number_of_actors
reg1=lm(imdb_score~total_number_of_actors)
reg2=lm(imdb_score~poly(total_number_of_actors,2))
reg3=lm(imdb_score~poly(total_number_of_actors,3))
reg4=lm(imdb_score~poly(total_number_of_actors,4))
reg5=lm(imdb_score~poly(total_number_of_actors,5))
anova(reg1, reg2, reg3, reg4, reg5)

lm.fit=lm(imdb_score~poly(total_number_of_actors,2))
summary(lm.fit)

#total_number_of_directors
reg1=lm(imdb_score~total_number_of_directors)
reg2=lm(imdb_score~poly(total_number_of_directors,2))
reg3=lm(imdb_score~poly(total_number_of_directors,3))
reg4=lm(imdb_score~poly(total_number_of_directors,4))
reg5=lm(imdb_score~poly(total_number_of_directors,5))
anova(reg1, reg2, reg3, reg4, reg5)

lm.fit=lm(imdb_score~total_number_of_directors)
summary(lm.fit)

#month_of_release
reg1=lm(imdb_score~month_of_release)
reg2=lm(imdb_score~poly(month_of_release,2))
reg3=lm(imdb_score~poly(month_of_release,3))
reg4=lm(imdb_score~poly(month_of_release,4))
reg5=lm(imdb_score~poly(month_of_release,5))
anova(reg1, reg2, reg3, reg4, reg5)

lm.fit=lm(imdb_score~poly(month_of_release,3))
summary(lm.fit)


# Calculating MSE for numerical + genre + interaction variables
fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)


# Removing outliers to enhance model
# The code was run step by step in 22 iterations, reduced here in 1 step by removing all. (Please see appendix below for all iterations)
films=films[-c(633,895,2045,2718,2310,2610,526), ]
films=films[-c(574, 754), ]
films=films[-c(446, 1162), ]
films=films[-c(439, 1375), ]
films=films[-c(264, 550), ]
films=films[-c(2204, 2801), ]
films=films[-c(431, 1904), ]
films=films[-c(1889, 1969), ]
films=films[-c(774, 1856), ]
films=films[-c(1143, 2044), ]
films=films[-c(944, 1460), ]
films=films[-c(898, 1968), ]
films=films[-c(1498, 2654), ]
films=films[-c(1445, 1542), ]
films=films[-c(114, 751), ]
films=films[-c(870, 1206), ]
films=films[-c(1422, 2020), ]
films=films[-c(8, 674), ]
films=films[-c(1960, 2059), ]
films=films[-c(343, 1888), ]
films=films[-c(912, 1205), ]
films=films[-c(258, 1921), ]
nrow(films)
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1)
           +  poly(month_of_release, 3)
           + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)


# Adding categorical variables that influence model prediction

#films$Frequency <- 1

#data(films)
#smp_size <- floor(0.986 * nrow(films))
#set.seed(0)
#train_ind <- sample(seq_len(nrow(films)), size = smp_size)

films_train <- films
#films_test <- films[-train_ind,]

nrow(films_train)
nrow(films_test)
attach(films_train)


#main_production_country
agg_main_production_country = aggregate(Frequency ~ main_production_country, films_train, sum)
z = agg_main_production_country[order(-agg_main_production_country$Frequency),]
z1 <- z$main_production_country[z$Frequency<8]
z2 <- z$main_production_country[z$Frequency>=8]
films_train$main_production_country_category = ifelse(films_train$main_production_country %in% z1,"Others", films_train$main_production_country)
films_test$main_production_country_category = ifelse(films_test$main_production_country %in% z2, films_test$main_production_country, "Others")

#main_lang
agg_main_lang = aggregate(Frequency ~ main_lang, films_train, sum)
z = agg_main_lang[order(-agg_main_lang$Frequency),]
z1 <- z$main_lang[z$Frequency<8]
z2 <- z$main_lang[z$Frequency>=8]
films_train$main_lang_category = ifelse(films_train$main_lang %in% z1,"Others", films_train$main_lang)
films_test$main_lang_category = ifelse(films_test$main_lang %in% z2, films_test$main_lang, "Others")

#main_production_company
agg_main_production_company = aggregate(Frequency ~ main_production_company, films_train, sum)
z = agg_main_production_company[order(-agg_main_production_company$Frequency),]
z1 <- z$main_production_company[z$Frequency<10]
z2 <- z$main_production_company[z$Frequency>=10]
films_train$main_production_company_category = ifelse(films_train$main_production_company %in% z1,"Others", films_train$main_production_company)
films_test$main_production_company_category = ifelse(films_test$main_production_company %in% z2, films_test$main_production_company, "Others")

attach(films_train)
temp_len = nrow(films_test)*0.3
others_count_country = ifelse(is.na(table(films_test$main_production_country_category)["Others"])==TRUE,0,table(films_test$main_production_country_category)["Others"])
others_count_lang = ifelse(is.na(table(films_test$main_lang_category)["Others"])==TRUE,0,table(films_test$main_lang_category)["Others"])
others_count_company = ifelse(is.na(table(films_test$main_production_company_category)["Others"])==TRUE,0,table(films_test$main_production_company_category)["Others"])

fit_without_categorical = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
                              + total_number_languages + poly(total_number_of_actors, 2) + total_number_of_directors + 
                                poly(month_of_release, 3) + genre_action
                              + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
                                genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
                                genre_scifi + genre_war + genre_western + 
                                main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
                              +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
                                (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
                                (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action))


fit_main_production_country = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
                                  + total_number_languages + poly(total_number_of_actors, 2) + total_number_of_directors + 
                                    poly(month_of_release, 3) + genre_action
                                  + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
                                    genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
                                    genre_scifi + genre_war + genre_western + 
                                    main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
                                  +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
                                    (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
                                    (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
                                  + main_production_country_category)

fit_main_lang = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
                    + total_number_languages + poly(total_number_of_actors, 2) + total_number_of_directors + 
                      poly(month_of_release, 3) + genre_action
                    + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
                      genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
                      genre_scifi + genre_war + genre_western + 
                      main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
                    +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
                      (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
                      (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
                    + main_lang_category)

fit_main_production_company = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
                                  + total_number_languages + poly(total_number_of_actors, 2) + total_number_of_directors + 
                                    poly(month_of_release, 3) + genre_action
                                  + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
                                    genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
                                    genre_scifi + genre_war + genre_western + 
                                    main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
                                  +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
                                    (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
                                    (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
                                  + main_production_company_category)

fit_main_country_lang = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
                            + total_number_languages + poly(total_number_of_actors, 2) + total_number_of_directors + 
                              poly(month_of_release, 3) + genre_action
                            + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
                              genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
                              genre_scifi + genre_war + genre_western + 
                              main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
                            +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
                              (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
                              (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
                            + main_production_country_category + main_lang_category)

fit_main_company_lang = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
                            + total_number_languages + poly(total_number_of_actors, 2) + total_number_of_directors + 
                              poly(month_of_release, 3) + genre_action
                            + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
                              genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
                              genre_scifi + genre_war + genre_western + 
                              main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
                            +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
                              (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
                              (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
                            + main_production_company_category + main_lang_category)

fit_main_company_country = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
                               + total_number_languages + poly(total_number_of_actors, 2) + total_number_of_directors + 
                                 poly(month_of_release, 3) + genre_action
                               + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
                                 genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
                                 genre_scifi + genre_war + genre_western + 
                                 main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
                               +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
                                 (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
                                 (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
                               + main_production_company_category + main_production_country_category)

fit_all_categorical = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
                          + total_number_languages + poly(total_number_of_actors, 2) + total_number_of_directors + 
                            poly(month_of_release, 3) + genre_action
                          + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
                            genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
                            genre_scifi + genre_war + genre_western + 
                            main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
                          +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
                            (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
                            (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
                          + main_production_company_category + main_production_country_category + main_lang_category)


if ((others_count_country<=temp_len) && (others_count_company<=temp_len) && (others_count_lang<=temp_len)){
  films_test$pred=predict(fit_all_categorical, films_test)
  print("Running fit_all_categorical")
} else if ((others_count_country>temp_len) && (others_count_company<=temp_len) && (others_count_lang<=temp_len)){
  films_test$pred=predict(fit_main_company_lang, films_test)
  print("Running fit_main_company_lang")
} else if ((others_count_country<=temp_len) && (others_count_company>temp_len) && (others_count_lang<=temp_len)){
  films_test$pred=predict(fit_main_country_lang, films_test)
  print("Running fit_main_country_lang")
} else if ((others_count_country<=temp_len) && (others_count_company<=temp_len) && (others_count_lang>temp_len)){
  films_test$pred=predict(fit_main_company_country, films_test)
  print("Running fit_main_company_country")
} else if ((others_count_country>temp_len) && (others_count_company>temp_len) && (others_count_lang<=temp_len)){
  films_test$pred=predict(fit_main_lang, films_test)
  print("Running fit_main_lang")
} else if ((others_count_country<=temp_len) && (others_count_company>temp_len) && (others_count_lang>temp_len)){
  films_test$pred=predict(fit_main_production_country, films_test)
  print("Running fit_main_production_country")
} else if ((others_count_country>temp_len) && (others_count_company<=temp_len) && (others_count_lang>temp_len)){
  films_test$pred=predict(fit_main_production_company, films_test)
  print("Running fit_main_production_company")
} else {
  films_test$pred=predict(fit_without_categorical, films_test)
  print("Running fit_without_categorical")
}

films_test$res=(films_test$imdb_score-films_test$pred)
films_test$res_sq=(films_test$res)^2
set.seed(0)
MSE=mean(films_test$res_sq)
MSE







#########################################################################

### Appendix

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)

library(car)
outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(633,895,2045,2718,2310,2610,526), ]  #633,895,2045,2718,2310,2610,526
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)

outlierTest(mreg1)
qqPlot(mreg1)

films=films[-c(574, 754), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)

outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(446, 1162), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)

outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(439, 1375), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)

outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(264, 550), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)

outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(2204, 2801), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)

outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(431, 1904), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)


outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(1889, 1969), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)

nrow(films)


outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(774, 1856), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)


outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(1143, 2044), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)


outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(944, 1460), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)

nrow(films)
outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(898, 1968), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)


outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(1498, 2654), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)


outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(1445, 1542), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)


outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(114, 751), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)


outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(870, 1206), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)

nrow(films)

outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(1422, 2020), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)


outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(8, 674), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)


outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(1960, 2059), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)

outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(343, 1888), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)

outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(912, 1205), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)

outlierTest(mreg1)
qqPlot(mreg1)
films=films[-c(258, 1921), ]
attach(films)

fit1 = glm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1)
           +  poly(month_of_release, 3)
           + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
set.seed(0)
mse = cv.glm(films, fit1, K = 10)$delta[1]
print(mse)

mreg1 = lm(imdb_score~poly(budget_in_millions, 4) + poly(year_of_release, 3) + poly(duration_in_hours, 4)
           + poly(total_number_languages, 1) + poly(total_number_of_actors, 2) + poly(total_number_of_directors, 1) + 
             poly(month_of_release, 3) + genre_action
           + genre_biography + genre_comedy + genre_crime + genre_documentary + genre_drama + 
             genre_family + genre_fantasy + genre_filmnoir + genre_history + genre_horror + genre_musical + 
             genre_scifi + genre_war + genre_western + 
             main_actor1_is_female + main_actor2_is_female + main_actor3_is_female
           +(duration_in_hours*genre_drama) + (genre_drama*genre_romance) + (genre_drama*genre_thriller) +
             (genre_drama*genre_horror) + (genre_animation*genre_fantasy) + (genre_fantasy*genre_adventure) +
             (year_of_release*genre_drama) + (year_of_release*genre_comedy) + (year_of_release*genre_action)
)
summary(mreg1)

outlierTest(mreg1)
qqPlot(mreg1)
nrow(films)    #2904 rows

