##########################################################
# 1) Install & download
##########################################################
#first, some housekeeping

#set digits
options(digits = 3)

#clear existing objects
rm(acc_results)

#download packages if needed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purr", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(ggforce)) install.packages("ggforce", repos = "http://cran.us.r-project.org")

#load libraries
library(tidyverse)
library(caret)
library(data.table)
library(scales)
library(lubridate)
library(purrr)
library(ggrepel)
library(randomForest)
library(Rborist)
library(rpart.plot)
library(ggforce)


#download raw data files
url_us17 <- "https://raw.githubusercontent.com/JeffSackmann/tennis_slam_pointbypoint/master/2017-usopen-points.csv"
url_wm21 <- "https://raw.githubusercontent.com/JeffSackmann/tennis_slam_pointbypoint/master/2021-wimbledon-points.csv"

download.file(url_us17,"us17_points")
download.file(url_wm21,"wm21_points")

#create data frames
us17points <- read_csv("us17_points")
wm21points <- read_csv("wm21_points")
tennispoints <- rbind(us17points,wm21points)

#initial structure check
dim(tennispoints)
str(tennispoints)

##########################################################
# 2) Clean up points table
##########################################################
  #create tournament column using text from match_id
  tennispoints <- tennispoints %>%
    mutate(Tournament=ifelse(str_sub(match_id,6,6)=="u","US17","WM21"))

  #create gender column using match_id (key = first digit of #)
  tennispoints <- tennispoints %>%
    mutate(Gender=ifelse(str_sub(match_id,-4,-4)==1,"M","W"))
  
  #create tournament_gender column using paste
  tennispoints <- tennispoints %>%
    mutate(TGComb=paste(Tournament, Gender, sep="_"))

  #create combined Y/N cols for add.l point features (ace, net pt, etc.)
  tennispoints <- tennispoints %>%
    mutate(UfeYN=P1UnfErr+P2UnfErr, 
          NetYN=P1NetPoint+P2NetPoint,
          WnrYN=P1Winner+P2Winner,
          AceYN=P1Ace+P2Ace,
          DfYN=P1DoubleFault+P2DoubleFault,
          BrkYN=P1BreakPointWon+P2BreakPointWon,
          CombDist=P1DistanceRun+P2DistanceRun)
  
  #remove blank point rows 
  #all of which conveniently have X, Y, or D in PointNumber field
  tennispoints <- tennispoints[!grepl("X", tennispoints$PointNumber),]
  tennispoints <- tennispoints[!grepl("Y", tennispoints$PointNumber),]
  tennispoints <- tennispoints[!grepl("D", tennispoints$PointNumber),]
  dim(tennispoints)
  
  #metric adjustment for WM distance data
  dist_adjust <- 3.1
  tennispoints <- tennispoints %>%
    mutate(Comb2Dist=ifelse(Tournament=="WM21",CombDist/dist_adjust,CombDist))
  
  #check avg combined dist per shot
  tennispoints %>%
    group_by(Tournament, Gender) %>%
    summarize(AvCombDistPerShot=sum(Comb2Dist)/sum(RallyCount)) %>%
    pivot_wider(names_from = Gender, values_from = AvCombDistPerShot)
  
  #check avg dist per shot each
  tennispoints %>%
    group_by(Tournament, Gender) %>%
    summarize(AvDistPerShotEa=sum(Comb2Dist)/(2*sum(RallyCount))) %>%
    pivot_wider(names_from = Gender, values_from = AvDistPerShotEa)
  
  #clean combined distance column
  #2% of rows have unexpected 0 in distance columns
  
  #calculate avg. comb. dist/shot otherwise
  AvCombDistPerShot <- tennispoints %>%
    filter(Comb2Dist!=0) %>%
    summarize(AvCombDistPerShot=sum(Comb2Dist)/(sum(RallyCount)))
  AvCombDistPerShot
  
  #convert to a scalar
  AvCombDistPerShotValue <- AvCombDistPerShot[[1]]
  
  #swap in average value where Comb2Dist is 0
  tennispoints$Comb2Dist[tennispoints$Comb2Dist==0] <- 
    AvCombDistPerShotValue*tennispoints$RallyCount[tennispoints$Comb2Dist==0]
  
  #check that average w/o filter now matches previous avg w/ filter
  AdjAvCombDistPerShot <- tennispoints %>%
    summarize(AdjAvCombDistPerShot=sum(Comb2Dist)/(sum(RallyCount)))
  AdjAvCombDistPerShot
  
  #add Point, Game, and Set ID numbers
  #add pointID row
  tennispoints$PointID<-1:nrow(tennispoints)
  
  #add GameID to each row - uses lag and cumsum functions
  #detect if a game was won after the pt in question
  tennispoints <- tennispoints %>%
    mutate(GameWon=ifelse(GameWinner==0,0,1))
  
  #cumulative sum of games completed, lagged to align properly
  tp_game_cs <- 1+lag(cumsum(tennispoints$GameWon))
  length(tp_game_cs)
  tp_game_cs[1:5]
  
  #gameid column added
  tennispoints <- tennispoints %>%
    mutate(GameID=ifelse(is.na(tp_game_cs),1,tp_game_cs))
  
  #check for reasonableness
  max(tennispoints$GameID)
  
  #add SetID to each row - similarly uses lag and cumsum functions
  tennispoints <- tennispoints %>%
    mutate(SetWon=ifelse(SetWinner==0,0,1))
  
  tp_set_cs <- 1+lag(cumsum(tennispoints$SetWon))
  length(tp_set_cs)
  tp_set_cs[1:5]
  
  tennispoints <- tennispoints %>%
    mutate(SetID=ifelse(is.na(tp_set_cs),1,tp_set_cs))
  
  max(tennispoints$SetID)
  
##########################################################
# 3) Create tables by game and set
##########################################################
#create core data table by game
  tpbygame <- tennispoints %>%
    group_by(GameID) %>%
    summarize(
      Tournament=first(Tournament),
      Gender=first(Gender),
      TGComb=paste(Tournament, Gender, sep="_"),
      Set=first(SetID),
      Points=n(),
      Margin=ifelse(Points<4,4,ifelse(Points==4,4,ifelse(Points==5,3,2))),
      AvgRC=sum(RallyCount)/Points,
      AvgCDist=sum(Comb2Dist)/Points,
      AcePct=sum(AceYN)/Points,
      WnrPct=sum(WnrYN)/Points,
      DfPct=sum(DfYN)/Points,
      UfePct=sum(UfeYN)/Points,
      NetPct=sum(NetYN)/Points,
      BrkYN=sum(BrkYN))

#create data table by set
  tpbyset <- tennispoints %>%
    group_by(SetID) %>%
    summarize(
      Tournament=first(Tournament),
      Gender=first(Gender),
      TGComb=paste(Tournament, Gender, sep="_"),
      Set=first(SetID),
      Points=n(),
      Margin=ifelse(Points<4,4,ifelse(Points==4,4,ifelse(Points==5,3,2))),
      AvgRC=sum(RallyCount)/Points,
      AvgCDist=sum(Comb2Dist)/Points,
      AcePct=sum(AceYN)/Points,
      WnrPct=sum(WnrYN)/Points,
      DfPct=sum(DfYN)/Points,
      UfePct=sum(UfeYN)/Points,
      NetPct=sum(NetYN)/Points,
      BrkYN=sum(BrkYN))

##########################################################
# 4) Partition data
##########################################################
#set seed
set.seed(7, sample.kind="Rounding")

#for "by game" tables
#create final validation set...the rest -> "trim" set for now
test_index <- createDataPartition(y = tpbygame$TGComb, 
                                  times = 1, p = 0.1, 
                                  list = FALSE)
tpbg_validation <- tpbygame[test_index,]
tpbg_trim <- tpbygame[-test_index,]


#create train and test sets from the trim set
set.seed(5, sample.kind="Rounding")
test_index <- createDataPartition(y = tpbg_trim$TGComb, 
                                  times = 1, p = 0.1, 
                                  list = FALSE)
tpbg_test <- tpbg_trim[test_index,]
tpbg_train <- tpbg_trim[-test_index,]


#verify that all rows are accounted for
nrow(tpbg_validation)+
  nrow(tpbg_test)+
  nrow(tpbg_train)

nrow(tpbg_validation)+
  nrow(tpbg_trim)

nrow(tpbygame)


#now, do the same, except for "By Set" tables
#create final validation set...the rest -> "trim" set for now
test_index <- createDataPartition(y = tpbyset$TGComb, 
                                  times = 1, p = 0.1, 
                                  list = FALSE)
tpbs_validation <- tpbyset[test_index,]
tpbs_trim <- tpbyset[-test_index,]


#create train and test sets from the trim set
set.seed(5, sample.kind="Rounding")
test_index <- createDataPartition(y = tpbs_trim$TGComb, 
                                  times = 1, p = 0.1, 
                                  list = FALSE)
tpbs_test <- tpbs_trim[test_index,]
tpbs_train <- tpbs_trim[-test_index,]


#verify that all rows are accounted for
nrow(tpbs_validation)+
  nrow(tpbs_test)+
  nrow(tpbs_train)

nrow(tpbs_validation)+
  nrow(tpbs_trim)

nrow(tpbyset)

##########################################################
# 5) Analysis & visualizations
##########################################################
#tables
  #simple table by tournament
  TournaTable <- tennispoints %>% 
  group_by(Tournament) %>%
  summarize(AvgRC=sum(RallyCount)/n(),
            AvgCDist=sum(Comb2Dist)/n(),
            AcePct=sum(AceYN)/n(),
            WnrPct=sum(WnrYN)/n(),
            DfPct=sum(DfYN)/n(),
            UfePct=sum(UfeYN)/n(),
            NetPct=sum(NetYN)/n())
  TournaTable
  
  #simple table by gender
  GenderTable <- tennispoints %>% 
  group_by(Gender) %>%
  summarize(AvgRC=sum(RallyCount)/n(),
            AvgCDist=sum(Comb2Dist)/n(),
            AcePct=sum(AceYN)/n(),
            WnrPct=sum(WnrYN)/n(),
            DfPct=sum(DfYN)/n(),
            UfePct=sum(UfeYN)/n(),
            NetPct=sum(NetYN)/n())
  GenderTable
  
  #simple table by TGComb
  TGCombTable <- tennispoints %>% 
    group_by(TGComb) %>%
    summarize(AvgRC=sum(RallyCount)/n(),
              AvgCDist=sum(Comb2Dist)/n(),
              AcePct=sum(AceYN)/n(),
              WnrPct=sum(WnrYN)/n(),
              DfPct=sum(DfYN)/n(),
              UfePct=sum(UfeYN)/n(),
              NetPct=sum(NetYN)/n())
  TGCombTable
  
#bar charts of these
  #tournament integer-based
  TTTall1 <- TournaTable %>% gather(key = Variable, value = Value, AvgRC:AvgCDist)
  TTTall1 %>% ggplot(aes(Variable, Value, fill = Tournament)) + 
    geom_col(position = "dodge")
  
  #tournament pct-based
  TTTall2 <- TournaTable %>% gather(key = Variable, value = Value, AcePct:NetPct)
  TTTall2 %>% ggplot(aes(Variable, Value, fill = Tournament)) + 
    geom_col(position = "dodge")
  
  #gender integer-based
  GTTall1 <- GenderTable %>% gather(key = Variable, value = Value, AvgRC:AvgCDist)
  GTTall1 %>% ggplot(aes(Variable, Value, fill = Gender)) + 
    geom_col(position = "dodge")
  
  #gender pct-based
  GTTall2 <- GenderTable %>% gather(key = Variable, value = Value, AcePct:NetPct)
  GTTall2 %>% ggplot(aes(Variable, Value, fill = Gender)) + 
    geom_col(position = "dodge")
  
  #comb integer-based
  TGTall1 <- TGCombTable %>% gather(key = Variable, value = Value, AvgRC:AvgCDist)
  TGTall1 %>% ggplot(aes(Variable, Value, fill = TGComb)) + 
    geom_col(position = "dodge")
  
  #comb pct-based
  TGTall2 <- TGCombTable %>% gather(key = Variable, value = Value, AcePct:NetPct)
  TGTall2 %>% ggplot(aes(Variable, Value, fill = TGComb)) + 
    geom_col(position = "dodge")
  

#graphs
  #point distribution plot by game
  tpbg_train %>% ggplot(aes(AvgRC, AvgCDist, color = Tournament)) + 
    geom_point(alpha = .1)
  
  #same graph, by set
  tpbs_train %>% ggplot(aes(AvgRC, AvgCDist, color = Tournament)) +
    geom_ellipse(color="#F8766D", aes(x0 = 4.5, y0 = 23, a = .6, b = 9, angle = -pi/16)) +
    geom_ellipse(color="#00BFC4", aes(x0 = 2.1, y0 = 15.5, a = .35, b = 4.5, angle = -pi/48)) +
    geom_ellipse(color="#00BFC4", aes(x0 = 4, y0 = 16, a = .6, b = 4.5, angle = -pi/10)) +
    geom_point(alpha = .5)
  
  #same graph, by set
  tpbs_train %>% ggplot(aes(AvgRC, AvgCDist, color = TGComb)) + 
    geom_point(alpha = .5)
  
  #same graph, shows that net points are indeed the cause!
  tpbs_train %>% ggplot(aes(AvgRC, NetPct, color = Tournament)) +
    geom_ellipse(aes(x0 = 2.1, y0 = .2, a = .4, b = .175, angle = pi/8)) +
    geom_point(alpha = .5)
  
  #really good gender by set graph 1
  tpbs_train %>% ggplot(aes(AcePct, NetPct, color = Gender)) +
    geom_ellipse(color="#F8766D", aes(x0 = .125, y0 = .275, a = .1, b = .2, angle = pi/6)) +
    geom_ellipse(color="#00BFC4", aes(x0 = .04, y0 = .11, a = .06, b = .12, angle = pi/12)) +
    geom_point(alpha = .5)
  
  #really good gender by set graph 2
  tpbs_train %>% ggplot(aes(AcePct, DfPct, color = Gender)) + 
    geom_point(alpha = .5)
  
  #other tournament by game graphs
  tpbg_train %>% ggplot(aes(AvgRC, NetPct, color = Tournament)) + geom_point()
  tpbg_train %>% ggplot(aes(AvgRC, WnrPct, color = Tournament)) + geom_point()
  tpbg_train %>% ggplot(aes(AvgRC, DfPct, color = Tournament)) + geom_point()
  
  #tournament by set graphs
  tpbs_train %>% ggplot(aes(AvgRC, NetPct, color = Tournament)) + geom_point()
  tpbs_train %>% ggplot(aes(AvgRC, WnrPct, color = Tournament)) + geom_point()
  tpbs_train %>% ggplot(aes(AvgRC, DfPct, color = Tournament)) + geom_point()
  
  #gender by game graphs
  tpbg_train %>% ggplot(aes(AcePct, NetPct, color = Gender)) + geom_point()
  tpbg_train %>% ggplot(aes(AcePct, UfePct, color = Gender)) + geom_point()
  tpbg_train %>% ggplot(aes(AcePct, DfPct, color = Gender)) + geom_point()
  
  #gender by set graphs
  tpbs_train %>% ggplot(aes(AcePct, NetPct, color = Gender)) + geom_point()
  tpbs_train %>% ggplot(aes(AcePct, UfePct, color = Gender)) + geom_point()
  tpbs_train %>% ggplot(aes(AcePct, DfPct, color = Gender)) + geom_point()
  
  #code to create graphs to show odds of a tournament by value of a predictor
  tpbg_train %>% 
    mutate(x = NetPct) %>%
    group_by(x) %>%
    summarize(prop = mean(Tournament == "WM21")) %>%
    ggplot(aes(x, prop)) +
    geom_point()
  
  #code to create graphs to show odds of a gender by value of a predictor
  tpbg_train %>% 
    mutate(x = round(AvgRC)) %>%
    group_by(x) %>%
    summarize(prop = mean(Tournament == "WM21")) %>%
    ggplot(aes(x, prop)) +
    geom_point()

##########################################################
# 6) Models: Guessing
##########################################################
#random guessing
  #guess tournament by game
  y_hat_guess <- factor(sample(c("US17","WM21"),nrow(tpbg_test),replace=TRUE))
  #check results
  tgg <- round(as.numeric(confusionMatrix(y_hat_guess,factor(tpbg_test$Tournament))$overall["Accuracy"]),2)
  tgg
  
  #guess gender by game
  y_hat_guess <- factor(sample(c("M","W"),nrow(tpbg_test),replace=TRUE))
  #check results
  ggg <- round(as.numeric(confusionMatrix(y_hat_guess, factor(tpbg_test$Gender))$overall["Accuracy"]),2)
  ggg
  
  #guess tgcomb by game
  y_hat_guess <- factor(sample(c("US17_M","US17_W","WM21_M","WM21_W"),
                               nrow(tpbg_test),replace=TRUE))
  #check results
  tggg <- round(as.numeric(confusionMatrix(y_hat_guess, factor(tpbg_test$TGComb))$overall["Accuracy"]),2)
  tggg
  
  #guess tournament by set
  y_hat_guess <- factor(sample(c("US17","WM21"),nrow(tpbs_test),replace=TRUE))
  #check results
  tsg <- round(as.numeric(confusionMatrix(y_hat_guess, factor(tpbs_test$Tournament))$overall["Accuracy"]),2)
  tsg
  
  #guess gender by set
  y_hat_guess <- factor(sample(c("M","W"),nrow(tpbs_test),replace=TRUE))
  #check results
  gsg <-round(as.numeric(confusionMatrix(y_hat_guess, factor(tpbs_test$Gender))$overall["Accuracy"]),2)
  gsg
  
  #guess tgcomb by set
  y_hat_guess <- factor(sample(c("US17_M","US17_W","WM21_M","WM21_W"),
                               nrow(tpbs_test),replace=TRUE))
  #check results
  tgsg <- round(as.numeric(confusionMatrix(y_hat_guess, factor(tpbs_test$TGComb))$overall["Accuracy"]),2)
  tgsg

#create a data frame to keep track of all results, add first result to it
  acc_results <- data.frame(Method = character(),
                            TmtByG = character(),
                            GenByG = character(),
                            TgByG = character(),
                            TmtByS = character(),
                            GenByS = character(),
                            TgByS = character())
  acc_guess <- c("Guess",tgg, ggg, tggg, tsg, gsg, tgsg)
  acc_results[nrow(acc_results) + 1,] = acc_guess
  acc_results %>% knitr::kable()

##########################################################
# 7) Models: Logistic regression
##########################################################
#using sapply to try all predictors at once
  #sapply w/ tournament by game
  predictors <- c("AcePct","AvgRC","WnrPct","NetPct","UfePct",
                  "Margin","AvgCDist","DfPct","BrkYN")
  tpbg_train <- tpbg_train %>% mutate(t = as.numeric(Tournament == "US17"))
  tpbg_test$t <- factor(tpbg_test$Tournament)
  accs <- sapply(predictors, function(p){
    form <- reformulate(p, response = "t")
    fit_glm <- glm(form, 
                   data=tpbg_train, 
                   family = "binomial")
    p_hat_glm <- predict(fit_glm, tpbg_test, type="response")
    y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, "US17", "WM21"))
    
    return(confusionMatrix(y_hat_glm, tpbg_test$t)$overall["Accuracy"])
  })
  accs <- stack(accs)[2:1]
  colnames(accs) <- c("Variable","Accuracy")
  rnl <- levels(accs$Variable)
  levels(accs$Variable) <- substr(rnl,1,nchar(rnl)-9)
  accs %>%
    ggplot(aes(Variable, Accuracy)) +
    geom_bar(stat="identity", fill="#009E73")+
    geom_text(aes(label=round(Accuracy,3)), position=position_dodge(width=0.9), vjust=-0.25)+
    theme_minimal()
  
  #sapply w/ gender by game
  predictors <- c("AcePct","AvgRC","WnrPct","NetPct","UfePct",
                  "Margin","AvgCDist","DfPct","BrkYN")
  tpbg_train <- tpbg_train %>% mutate(g = as.numeric(Gender == "M"))
  tpbg_test$g <- factor(tpbg_test$Gender)
  accs <- sapply(predictors, function(p){
    form <- reformulate(p, response = "g")
    fit_glm <- glm(form, 
                   data=tpbg_train, 
                   family = "binomial")
    p_hat_glm <- predict(fit_glm, tpbg_test, type="response")
    y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, "M", "W"))
    
    return(confusionMatrix(y_hat_glm, tpbg_test$g)$overall["Accuracy"])
  })
  accs <- stack(accs)[2:1]
  colnames(accs) <- c("Variable","Accuracy")
  rnl <- levels(accs$Variable)
  levels(accs$Variable) <- substr(rnl,1,nchar(rnl)-9)
  accs %>%
    ggplot(aes(Variable, Accuracy)) +
    geom_bar(stat="identity", fill="#009E73")+
    geom_text(aes(label=round(Accuracy,3)), position=position_dodge(width=0.9), vjust=-0.25)+
    theme_minimal()
  
  #sapply w/ tournament by set
  predictors <- c("AcePct","AvgRC","WnrPct","NetPct","UfePct",
                  "Margin","AvgCDist","DfPct","BrkYN")
  tpbs_train <- tpbs_train %>% mutate(t = as.numeric(Tournament == "US17"))
  tpbs_test$t <- factor(tpbs_test$Tournament)
  accs <- sapply(predictors, function(p){
    form <- reformulate(p, response = "t")
    fit_glm <- glm(form, 
                   data=tpbs_train, 
                   family = "binomial")
    p_hat_glm <- predict(fit_glm, tpbs_test, type="response")
    y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, "US17", "WM21"))
    
    return(confusionMatrix(y_hat_glm, tpbs_test$t)$overall["Accuracy"])
  })
  accs <- stack(accs)[2:1]
  colnames(accs) <- c("Variable","Accuracy")
  rnl <- levels(accs$Variable)
  levels(accs$Variable) <- substr(rnl,1,nchar(rnl)-9)
  accs %>%
    ggplot(aes(Variable, Accuracy)) +
    geom_bar(stat="identity", fill="#009E73")+
    geom_text(aes(label=round(Accuracy,3)), position=position_dodge(width=0.9), vjust=-0.25)+
    theme_minimal()
  
  #check out top two lone predictors
  accs %>% arrange(desc(Accuracy)) %>%.[1:2,]
  
  #sapply w/ gender by set
  predictors <- c("AcePct","AvgRC","WnrPct","NetPct","UfePct",
                  "Margin","AvgCDist","DfPct","BrkYN")
  tpbs_train <- tpbs_train %>% mutate(g = as.numeric(Gender == "M"))
  tpbs_test$g <- factor(tpbs_test$Gender)
  accs <- sapply(predictors, function(p){
    form <- reformulate(p, response = "g")
    fit_glm <- glm(form, 
                   data=tpbs_train, 
                   family = "binomial")
    p_hat_glm <- predict(fit_glm, tpbs_test, type="response")
    y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, "M", "W"))
    
    return(confusionMatrix(y_hat_glm, tpbs_test$g)$overall["Accuracy"])
  })
  accs <- stack(accs)[2:1]
  colnames(accs) <- c("Variable","Accuracy")
  rnl <- levels(accs$Variable)
  levels(accs$Variable) <- substr(rnl,1,nchar(rnl)-9)
  accs %>%
    ggplot(aes(Variable, Accuracy)) +
    geom_bar(stat="identity", fill="#009E73")+
    geom_text(aes(label=round(Accuracy,3)), position=position_dodge(width=0.9), vjust=-0.25)+
    theme_minimal()
  
#logistic regressions
  #logistic regression of tournament by game on top two variables
  #assign 1 if US17
  tpbg_train <- tpbg_train %>% mutate(t = as.numeric(Tournament == "US17"))
  tpbg_test$t <- factor(tpbg_test$Tournament)
  #fit logistic model on two predictors to start
  fit_glm <- glm(t ~ AvgCDist + AvgRC, 
                 data=tpbg_train, 
                 family = "binomial")
  #generate predictions
  p_hat_glm <- predict(fit_glm, tpbg_test, type="response")
  #assign binary outcome to predictions based on %
  y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, "US17", "WM21"))
  #check results
  tglr <-
  as.numeric(round(confusionMatrix(y_hat_glm, tpbg_test$t)$overall["Accuracy"],2))
  tglr
  
  #logistic regression of gender by game on top two variables
  #assign 1 if M
  tpbg_train <- tpbg_train %>% mutate(g = as.numeric(Gender == "M"))
  tpbg_test$g <- factor(tpbg_test$Gender)
  #fit logistic model on two predictors to start
  fit_glm <- glm(g ~ AcePct + NetPct, 
                 data=tpbg_train, 
                 family = "binomial")
  #generate predictions
  p_hat_glm <- predict(fit_glm, tpbg_test, type="response")
  #assign binary outcome to predictions based on %
  y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, "M", "W"))
  #check results
  gglr <-
  as.numeric(round(confusionMatrix(y_hat_glm, tpbg_test$g)$overall["Accuracy"],2))
  gglr
  
  #logistic regression of tournament by set on top two variables
  #assign 1 if US17
  tpbs_train <- tpbs_train %>% mutate(t = as.numeric(Tournament == "US17"))
  tpbs_test$t <- factor(tpbs_test$Tournament)
  #fit logistic model on two predictors to start
  fit_glm <- glm(t ~ AvgCDist + AvgRC, 
                 data=tpbs_train, 
                 family = "binomial")
  #generate predictions
  p_hat_glm <- predict(fit_glm, tpbs_test, type="response")
  #assign binary outcome to predictions based on %
  y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, "US17", "WM21"))
  #check results
  tslr <-
  as.numeric(round(confusionMatrix(y_hat_glm, tpbs_test$t)$overall["Accuracy"],2))
  tslr
  
  #logistic regression of gender by set on top two variables
  #assign 1 if M
  tpbs_train <- tpbs_train %>% mutate(g = as.numeric(Gender == "M"))
  tpbs_test$g <- factor(tpbs_test$Gender)
  #fit logistic model on two predictors to start
  fit_glm <- glm(g ~ AcePct + NetPct, 
                 data=tpbs_train, 
                 family = "binomial")
  #generate predictions
  p_hat_glm <- predict(fit_glm, tpbs_test, type="response")
  #assign binary outcome to predictions based on %
  y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, "M", "W"))
  #check results
  gslr <-
  as.numeric(round(confusionMatrix(y_hat_glm, tpbs_test$g)$overall["Accuracy"],2))
  gslr
  
#add results to data frame
  acc_lr <- c("LogReg",tglr, gglr, "-", tslr, gslr, "-")
  acc_results[nrow(acc_results) + 1,] = acc_lr
  acc_results %>% knitr::kable()
##########################################################
# 8) Models: Knn 
##########################################################
#knn models
# tournament by game
  # cols 8 and 9 are two best predictors, AvgRC and AvgCDist
  x <- tpbg_train[,8:9]
  y <- factor(tpbg_train$Tournament)
  knn_fit <- knn3(x, y)
  y_hat_knn <- predict(knn_fit, tpbg_test[,8:9], type = "class")
  confusionMatrix(data = y_hat_knn, reference = (tpbg_test$t))$overall["Accuracy"]
  #pick best k
  ks <- seq(5, 100, 5)
  x <- tpbg_train[,8:9]
  y <- factor(tpbg_train$Tournament)
  accuracy <- map_df(ks, function(k){
    knn_fit <- knn3(x, y, k = k)
    y_hat <- predict(knn_fit, tpbg_test[,8:9], type = "class")
    cm_test <- confusionMatrix(y_hat_knn, reference = (tpbg_test$t))
    test_error <- cm_test$overall["Accuracy"]
    tibble(test = test_error)
  })
  tgkn <- round(max(accuracy$test),2)
  tgkn
  
str(tpbg_train)  
  
# gender by game, avoiding 2 pct-based predictors in this case to avoid too many ties
  # trying AvgRC and AcePct
  x <- tpbg_train[,c(8,10)]
  y <- factor(tpbg_train$Gender)
  knn_fit <- knn3(x, y)
  y_hat_knn <- predict(knn_fit, tpbg_test[,c(8,10)], type = "class")
  confusionMatrix(data = y_hat_knn, reference = tpbg_test$g)$overall["Accuracy"]
  #pick best k
  ks <- seq(5, 100, 5)
  x <- tpbg_train[,c(8,10)]
  y <- factor(tpbg_train$Gender)
  accuracy <- map_df(ks, function(k){
    knn_fit <- knn3(x, y, k = k)
    y_hat <- predict(knn_fit, tpbg_test[,c(8,10)], type = "class")
    cm_test <- confusionMatrix(y_hat_knn, reference = (tpbg_test$g))
    test_error <- cm_test$overall["Accuracy"]
    tibble(test = test_error)
  })
  ggkn <- round(max(accuracy$test),2)
  ggkn
  
# tg_comb by game
  # use 4 best predictors: AvgRC, AvgCDist, AcePct, NetPct
  x <- tpbg_train[,c(8,9,10,14)]
  y <- factor(tpbg_train$TGComb)
  knn_fit <- knn3(x, y)
  y_hat_knn <- predict(knn_fit, tpbg_test[,c(8,9,10,14)], type = "class")
  confusionMatrix(data = y_hat_knn, reference = factor(tpbg_test$TGComb))$overall["Accuracy"]
  #pick best k
  ks <- seq(5, 100, 5)
  x <- tpbg_train[,c(8,9,10,14)]
  y <- factor(tpbg_train$TGComb)
  accuracy <- map_df(ks, function(k){
    knn_fit <- knn3(x, y, k = k)
    y_hat <- predict(knn_fit, tpbg_test[,c(8,9,10,14)], type = "class")
    cm_test <- confusionMatrix(y_hat_knn, reference = factor(tpbg_test$TGComb))
    test_error <- cm_test$overall["Accuracy"]
    tibble(test = test_error)
  })
  tggkn <- round(max(accuracy$test),2)
  tggkn
  
# tournament by set
  # cols 8 and 9 are still two best predictors: AvgRC and AvgCDist
  x <- tpbs_train[,8:9]
  y <- factor(tpbs_train$Tournament)
  knn_fit <- knn3(x, y)
  y_hat_knn <- predict(knn_fit, tpbs_test[,8:9], type = "class")
  confusionMatrix(data = y_hat_knn, reference = (tpbs_test$t))$overall["Accuracy"]
  #pick best k
  ks <- seq(5, 100, 5)
  x <- tpbs_train[,8:9]
  y <- factor(tpbs_train$Tournament)
  accuracy <- map_df(ks, function(k){
    knn_fit <- knn3(x, y, k = k)
    y_hat_knn <- predict(knn_fit, tpbs_test[,8:9], type = "class")
    cm_test <- confusionMatrix(y_hat_knn, reference = (tpbs_test$t))
    test_error <- cm_test$overall["Accuracy"]
    tibble(test = test_error)
  })
  tskn <- round(max(accuracy$test),2)
  tskn

  #same graph, by set
  #tpbs_test %>% ggplot(aes(AvgRC, AvgCDist, color = y_hat_knn)) + 
    #geom_point(alpha = .8)
  
    
# gender by set
  # trying AcePct and NetPct, now that we are going by set
  x <- tpbs_train[,c(10,14)]
  y <- factor(tpbs_train$Gender)
  knn_fit <- knn3(x, y)
  y_hat_knn <- predict(knn_fit, tpbs_test[,c(10,14)], type = "class")
  confusionMatrix(data = y_hat_knn, reference = tpbs_test$g)$overall["Accuracy"]
  #pick best k
  ks <- seq(5, 100, 5)
  x <- tpbs_train[,c(10,14)]
  y <- factor(tpbs_train$Gender)
  accuracy <- map_df(ks, function(k){
    knn_fit <- knn3(x, y, k = k)
    y_hat <- predict(knn_fit, tpbs_test[,c(10,14)], type = "class")
    cm_test <- confusionMatrix(y_hat_knn, reference = (tpbs_test$g))
    test_error <- cm_test$overall["Accuracy"]
    tibble(test = test_error)
  })
  gskn <- round(max(accuracy$test),2)
  gskn
  
# tg_comb by set
  # Use AvgRC, AvgCDist, AcePct, NetPct
  x <- tpbs_train[,c(8,9,10,14)]
  y <- factor(tpbs_train$TGComb)
  knn_fit <- knn3(x, y)
  y_hat_knn <- predict(knn_fit, tpbs_test[,c(8,9,10,14)], type = "class")
  confusionMatrix(data = y_hat_knn, reference = factor(tpbs_test$TGComb))$overall["Accuracy"]
  #pick best k
  ks <- seq(5, 100, 5)
  x <- tpbs_train[,c(8,9,10,14)]
  y <- factor(tpbs_train$TGComb)
  accuracy <- map_df(ks, function(k){
    knn_fit <- knn3(x, y, k = k)
    y_hat <- predict(knn_fit, tpbs_test[,c(8,9,10,14)], type = "class")
    cm_test <- confusionMatrix(y_hat_knn, reference = factor(tpbs_test$TGComb))
    test_error <- cm_test$overall["Accuracy"]
    tibble(test = test_error)
  })
  tgskn <- round(max(accuracy$test),2)
  tgskn
  
# add to results data frame
  acc_kn <- c("Knn",tgkn, ggkn, tggkn, tskn, gskn, tgskn)
  acc_results[nrow(acc_results) +1,] = acc_kn
  acc_results %>% knitr::kable()
  
##########################################################
# 9) Models: Classification trees 
##########################################################
# classification trees
  #tournament by set
  y <- tpbs_train$Tournament
  train_rpart <- train(Tournament ~ AvgRC + AvgCDist,
                       method = "rpart",
                       tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                       data=tpbs_train)
  plot(train_rpart)
  cpbest <- train_rpart$bestTune
  #once tuned, time for rpart with graphs
  fit_rpart <- rpart(Tournament ~ AvgRC + AvgCDist,
                     data=tpbs_train,
                     cp=cpbest,
                     method = "class")
  rpart.plot(fit_rpart)
  y_hat <- predict(fit_rpart, tpbs_test, type="class")
  tsct <- confusionMatrix(factor(y_hat), factor(tpbs_test$Tournament))$overall["Accuracy"]
  tsct <- round(as.numeric(tsct),2)
  tsct
  
  #gender by set
  y <- tpbs_train$Gender
  train_rpart <- train(Gender ~ AcePct + NetPct,
                       method = "rpart",
                       tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                       data=tpbs_train)
  plot(train_rpart)
  cpbest <- train_rpart$bestTune
  #once tuned, time for rpart with graphs
  fit_rpart <- rpart(Gender ~ AcePct + NetPct,
                     data=tpbs_train,
                     cp=cpbest,
                     method = "class")
  rpart.plot(fit_rpart)
  y_hat <- predict(fit_rpart, tpbs_test, type="class")
  gsct <- confusionMatrix(factor(y_hat), factor(tpbs_test$Gender))$overall["Accuracy"]
  gsct <- round(as.numeric(gsct),2)
  gsct
  
  #TGComb by set
  y <- tpbs_train$TGComb
  train_rpart <- train(TGComb ~ AvgRC + AvgCDist + AcePct + NetPct,
                       method = "rpart",
                       tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                       data=tpbs_train)
  plot(train_rpart)
  cpbest <- train_rpart$bestTune
  #once tuned, time for rpart with graphs
  fit_rpart <- rpart(TGComb ~ AvgRC + AvgCDist + AcePct + NetPct,
                     data=tpbs_train,
                     cp=cpbest,
                     method = "class")
  rpart.plot(fit_rpart)
  y_hat <- predict(fit_rpart, tpbs_test, type="class")
  tgsct <- confusionMatrix(factor(y_hat), factor(tpbs_test$TGComb))$overall["Accuracy"]
  tgsct <- round(as.numeric(tgsct),2)
  tgsct
  
  # add to results data frame
  acc_ct <- c("CTree","-", "-", "-", tsct, gsct, tgsct)
  acc_results[nrow(acc_results) +1,] = acc_ct
  acc_results %>% knitr::kable()
  
##########################################################
# 10) Models: Random forests
##########################################################
#random forests
  #tuned version of tournament by set
  train_rft <- train(Tournament ~ AvgCDist + AvgRC, 
                     method = "Rborist",
                     tuneGrid = data.frame(predFixed = 2, minNode = c(3, 25)),
                     data=tpbs_train)
  y_hat2 <- predict(train_rft, tpbs_test)
  tsrf <- confusionMatrix(y_hat2, factor(tpbs_test$Tournament))$overall["Accuracy"]
  tsrf <- round(as.numeric(tsrf),2)
  tsrf
  
  #tuned rf of gender by set
  train_rft <- train(Gender ~ AcePct + NetPct,
                     method = "Rborist",
                     tuneGrid = data.frame(predFixed = 2, minNode = c(3, 25)),
                     data=tpbs_train)
  y_hat2 <- predict(train_rft, tpbs_test)
  gsrf <- confusionMatrix(y_hat2, factor(tpbs_test$Gender))$overall["Accuracy"]
  gsrf <- round(as.numeric(gsrf),2)
  gsrf
  
  #tuned version of TGComb by set
  train_rft <- train(TGComb ~ AvgRC + AvgCDist + AcePct + NetPct, 
                     method = "Rborist",
                     tuneGrid = data.frame(predFixed = 2, minNode = c(3, 25)),
                     data=tpbs_train)
  y_hat2 <- predict(train_rft, tpbs_test)
  tgsrf <- confusionMatrix(y_hat2, factor(tpbs_test$TGComb))$overall["Accuracy"]
  tgsrf <- round(as.numeric(tgsrf),2)
  tgsrf
  
  # add to results data frame
  acc_rf <- c("RForest","-", "-", "-", tsrf, gsrf, tgsrf)
  acc_results[nrow(acc_results) +1,] = acc_rf
  acc_results %>% knitr::kable()
  
  
##########################################################
# 11) Models: Applying best to validation data
##########################################################
#Knn vs. validation set
  #add factored result column
  tpbs_validation$t <- factor(tpbs_validation$Tournament)
  tpbs_validation$g <- factor(tpbs_validation$Gender)
  
  #knn tournament by set vs. validation
  # cols 8 and 9 are still two best predictors: AvgRC and AvgCDist
  x <- tpbs_trim[,8:9]
  y <- factor(tpbs_trim$Tournament)
  knn_fit <- knn3(x, y)
  y_hat_knn <- predict(knn_fit, tpbs_validation[,8:9], type = "class")
  confusionMatrix(data = y_hat_knn, reference = (tpbs_validation$t))$overall["Accuracy"]
  #pick best k
  ks <- seq(5, 100, 5)
  x <- tpbs_trim[,8:9]
  y <- factor(tpbs_trim$Tournament)
  accuracy <- map_df(ks, function(k){
    knn_fit <- knn3(x, y, k = k)
    y_hat_knn <- predict(knn_fit, tpbs_validation[,8:9], type = "class")
    cm_validation <- confusionMatrix(y_hat_knn, reference = (tpbs_validation$t))
    validation_error <- cm_validation$overall["Accuracy"]
    tibble(validation = validation_error)
  })
  tsknv <- round(max(accuracy$validation),2)
  tsknv
  
  #key graph
  tpbs_validation %>% ggplot(aes(AvgRC, AvgCDist, color = y_hat_knn)) + 
    geom_point(alpha = .8)
  
  # knn gender by set vs. validation
  # trying AcePct and NetPct, now that we are going by set
  x <- tpbs_trim[,c(10,14)]
  y <- factor(tpbs_trim$Gender)
  knn_fit <- knn3(x, y)
  y_hat_knn <- predict(knn_fit, tpbs_validation[,c(10,14)], type = "class")
  confusionMatrix(data = y_hat_knn, reference = tpbs_validation$g)$overall["Accuracy"]
  #pick best k
  ks <- seq(5, 100, 5)
  x <- tpbs_trim[,c(10,14)]
  y <- factor(tpbs_trim$Gender)
  accuracy <- map_df(ks, function(k){
    knn_fit <- knn3(x, y, k = k)
    y_hat <- predict(knn_fit, tpbs_validation[,c(10,14)], type = "class")
    cm_validation <- confusionMatrix(y_hat_knn, reference = (tpbs_validation$g))
    validation_error <- cm_validation$overall["Accuracy"]
    tibble(validation = validation_error)
  })
  gsknv <- round(max(accuracy$validation),2)
  gsknv
  
  # knn tg_comb by set vs. validation
  # AvgRC and AvgCDist
  x <- tpbs_trim[,c(8,9,10,14)]
  y <- factor(tpbs_trim$TGComb)
  knn_fit <- knn3(x, y)
  y_hat_knn <- predict(knn_fit, tpbs_validation[,c(8,9,10,14)], type = "class")
  confusionMatrix(data = y_hat_knn, reference = factor(tpbs_validation$TGComb))$overall["Accuracy"]
  #pick best k
  ks <- seq(5, 100, 5)
  x <- tpbs_trim[,c(8,9,10,14)]
  y <- factor(tpbs_trim$TGComb)
  accuracy <- map_df(ks, function(k){
    knn_fit <- knn3(x, y, k = k)
    y_hat <- predict(knn_fit, tpbs_validation[,c(8,9,10,14)], type = "class")
    cm_validation <- confusionMatrix(y_hat_knn, reference = factor(tpbs_validation$TGComb))
    validation_error <- cm_validation$overall["Accuracy"]
    tibble(validation = validation_error)
  })
  tgsknv <- round(max(accuracy$validation),2)
  tgsknv
  
#Classification tree vs. validation set
  #tournament by set
  y <- tpbs_trim$Tournament
  train_rpart <- train(Tournament ~ AvgRC + AvgCDist,
                       method = "rpart",
                       tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                       data=tpbs_trim)
  plot(train_rpart)
  cpbest <- train_rpart$bestTune
  #once tuned, time for rpart with graphs
  fit_rpart <- rpart(Tournament ~ AvgRC + AvgCDist,
                     data=tpbs_trim,
                     cp=cpbest,
                     method = "class")
  rpart.plot(fit_rpart)
  y_hat <- predict(fit_rpart, tpbs_validation, type="class")
  tsctv <- confusionMatrix(factor(y_hat), factor(tpbs_validation$Tournament))$overall["Accuracy"]
  tsctv <- round(as.numeric(tsct),2)
  tsctv
  
  #gender by set
  y <- tpbs_trim$Gender
  train_rpart <- train(Gender ~ AcePct + NetPct,
                       method = "rpart",
                       tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                       data=tpbs_trim)
  plot(train_rpart)
  cpbest <- train_rpart$bestTune
  #once tuned, time for rpart with graphs
  fit_rpart <- rpart(Gender ~ AcePct + NetPct,
                     data=tpbs_trim,
                     cp=cpbest,
                     method = "class")
  rpart.plot(fit_rpart)
  y_hat <- predict(fit_rpart, tpbs_validation, type="class")
  gsctv <- confusionMatrix(factor(y_hat), factor(tpbs_validation$Gender))$overall["Accuracy"]
  gsctv <- round(as.numeric(gsct),2)
  gsctv
  
  #TGComb by set
  y <- tpbs_trim$TGComb
  train_rpart <- train(TGComb ~ AvgRC + AvgCDist + AcePct + NetPct,
                       method = "rpart",
                       tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                       data=tpbs_trim)
  plot(train_rpart)
  cpbest <- train_rpart$bestTune
  #once tuned, time for rpart with graphs
  fit_rpart <- rpart(TGComb ~ AvgRC + AvgCDist + AcePct + NetPct,
                     data=tpbs_trim,
                     cp=cpbest,
                     method = "class")
  rpart.plot(fit_rpart)
  y_hat <- predict(fit_rpart, tpbs_validation, type="class")
  tgsctv <- confusionMatrix(factor(y_hat), factor(tpbs_validation$TGComb))$overall["Accuracy"]
  tgsctv <- round(as.numeric(tgsct),2)
  tgsctv

  
#Random forest vs. validation set
  #random forests
  #rf tournament by set vs. validation
  train_rft <- train(Tournament ~ AvgCDist + AvgRC, 
                     method = "Rborist",
                     tuneGrid = data.frame(predFixed = 2, minNode = c(3, 25)),
                     data=tpbs_trim)
  y_hat2 <- predict(train_rft, tpbs_validation)
  tsrfv <- confusionMatrix(y_hat2, factor(tpbs_validation$Tournament))$overall["Accuracy"]
  tsrfv <- round(as.numeric(tsrf),2)
  tsrfv
  
  #rf gender by set vs. validation
  train_rft <- train(Gender ~ AcePct + NetPct,
                     method = "Rborist",
                     tuneGrid = data.frame(predFixed = 2, minNode = c(3, 25)),
                     data=tpbs_trim)
  y_hat2 <- predict(train_rft, tpbs_validation)
  gsrfv <- confusionMatrix(y_hat2, factor(tpbs_validation$Gender))$overall["Accuracy"]
  gsrfv <- round(as.numeric(gsrf),2)
  gsrfv
  
  #rf tgcomb by set vs. validation
  train_rft <- train(TGComb ~ AvgRC + AvgCDist + AcePct + NetPct, 
                     method = "Rborist",
                     tuneGrid = data.frame(predFixed = 2, minNode = c(3, 25)),
                     data=tpbs_trim)
  y_hat2 <- predict(train_rft, tpbs_validation)
  tgsrfv <- confusionMatrix(y_hat2, factor(tpbs_validation$TGComb))$overall["Accuracy"]
  tgsrfv <- round(as.numeric(tgsrf),2)
  tgsrfv
  
  
#logistic regression of gender by set on top two variables
  #assign 1 if M
  tpbs_trim <- tpbs_trim %>% mutate(g = as.numeric(Gender == "M"))
  tpbs_validation$g <- factor(tpbs_validation$Gender)
  #fit logistic model on two predictors to start
  fit_glm <- glm(g ~ AcePct + NetPct, 
                 data=tpbs_trim, 
                 family = "binomial")
  #generate predictions
  p_hat_glm <- predict(fit_glm, tpbs_validation, type="response")
  #assign binary outcome to predictions based on %
  y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, "M", "W"))
  #check results
  gslrv <-
    as.numeric(round(confusionMatrix(y_hat_glm, tpbs_validation$g)$overall["Accuracy"],2))
  gslrv
  
#final data frame of results
  acc_validation <- data.frame(Method = character(),
                            TmtByS = character(),
                            GenByS = character(),
                            TgByS = character())
  
  acc_v_lr <- c("LogReg","-", gslrv, "-")
  acc_v_kn <- c("Knn",tsknv, gsknv, tgsknv)
  acc_v_ct <- c("CTree",tsctv, gsctv, tgsctv)
  acc_v_rf <- c("RForest",tsrfv, gsrfv, tgsrfv)
  
  acc_validation[1,] = acc_v_lr
  acc_validation[2,] = acc_v_kn
  acc_validation[3,] = acc_v_ct
  acc_validation[4,] = acc_v_rf
  
  acc_validation %>% knitr::kable()