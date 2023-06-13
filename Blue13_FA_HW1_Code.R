library(gmodels)
library(vcd)
library(smbinning)
library(dplyr)
library(shades)
library(magrittr)
library(stringr)
library(forcats)
library(plotly)
##Set Seed
set.seed(42)

##Read in CSV
accepted_customers <- read.csv("accepted_customers.csv")
rejected_customers <- read.csv("rejected_customers.csv")

##Set categorical variables as factors
categorical <- c("TEL","NMBLOAN","FINLOAN","EC_CARD","BUREAU","LOCATION","REGN","DIV","PRODUCT","RESID","NAT","PROF","CAR","CARDS")
accepted_customers %<>%
  mutate_each_(funs(factor(.)),categorical)

##Flip 0->1 and 1->0 (1 becomes good; 0 becomes bad)
accepted_customers$WOE_GB <- +(!accepted_customers$GB)  #WOE_GB will be used for smbinning, but NOT for model building

# combine categories in CARDS
# fix quasi-completion problem for "CARDS_WOE":'American Express', 'VISA Others'
table(accepted_customers$CARDS,accepted_customers$WOE_GB)
accepted_customers <- accepted_customers %>%
  mutate(CARDS = fct_collapse(CARDS, 
                              "Other credit car" = c("American Express","VISA mybank","VISA Others")))

##Training/Validation Split
ac_train_id <- sample(seq_len(nrow(accepted_customers)), size = floor(0.70*nrow(accepted_customers)))
ac_train <- accepted_customers[ac_train_id, ]
ac_valid <- accepted_customers[-ac_train_id, ]

##Information Value for Each Variable
iv_summary <- smbinning.sumiv(df = ac_train, y = "WOE_GB")
smbinning.sumiv.plot(iv_summary)
iv_summary #Only Continuous Variables >= 0.1 IV

##Binning of Continuous Variables - IV >= 0.1
num_names <- names(ac_train)[sapply(ac_train, is.numeric)] #Gathering the names of numeric variables in data

result_all_sig <- list() #Creating empty list to store all results
#iv_summary_sub <- subset(iv_summary, Char %in% num_names) #Subset only IV summaries for numeric columns

##Loop through numeric variables and check whether the WOE should be calculated
for(i in 1:length(num_names)){
  check_res <- smbinning(df = ac_train, y = "WOE_GB", x = num_names[i])
  if(check_res[i] == "Uniques values < 5") {
    next
  }
  else if(check_res[i] == "No significant splits") {
    next
  }
  else if(check_res$iv < 0.1) {
    next
  }
  else {
    result_all_sig[[num_names[i]]] <- check_res
  }
}

# Binning of Factor Variables #
factors <- names(ac_train)[sapply(ac_train, is.factor)]
result_factor <- list()
for (i in 1:length(factors)){
  results <- smbinning.factor(df = ac_train, y = "WOE_GB", x =factors[i])
  if (results$iv < 0.1) {
    next
  } else {
    result_factor[[factors[i]]]<- results
  }
}
result_all <- c(result_all_sig,result_factor)

##Generating Variables of Bins and WOE Values (Training Set)
for(i in 1:length(result_all_sig)) {
  ac_train <- smbinning.gen(df = ac_train, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}
for(i in 1:length(result_factor)){
  ac_train <- smbinning.factor.gen(df=ac_train, ivout = result_factor[[i]], chrname=paste(result_factor[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all)) {
  for (i in 1:nrow(ac_train)) {
    bin_name <- paste(result_all[[j]]$x, "_bin", sep = "")
    bin <- substr(ac_train[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) { #If there is a missing bin
      bin <- dim(result_all[[j]]$ivtable)[1] - 1
      ac_train[[woe_name]][i] <- result_all[[j]]$ivtable[bin, "WoE"]
    } else {
      ac_train[[woe_name]][i] <- result_all[[j]]$ivtable[bin, "WoE"]   #Calculates WOE for each observation
    }
  }
}

##### Build Initial Logistic Regression #####
initial_score <- glm(data = ac_train, GB ~ AGE_WOE + 
                       TMJOB1_WOE + 
                       PERS_H_WOE + +INCOME_WOE+CARDS_WOE+EC_CARD_WOE,
                     weights = ac_train$X_freq_, family = "binomial")
summary(initial_score)

##Evaluate the Initial Model - Training Data
ac_train$pred <- initial_score$fitted.values

smbinning.metrics(dataset = ac_train, prediction = "pred", actualclass = "GB", report = 1)
smbinning.metrics(dataset = ac_train, prediction = "pred", actualclass = "GB", report = 0, plot = "ks")
smbinning.metrics(dataset = ac_train, prediction = "pred", actualclass = "GB", report = 0, plot = "auc")

##Generating Variables of Bins and WOE Values (Validation Set)
for(i in 1:length(result_all_sig)) {
  ac_valid <- smbinning.gen(df = ac_valid, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}
for(i in 1:length(result_factor)){
  ac_valid <- smbinning.factor.gen(df=ac_valid, ivout = result_factor[[i]], chrname=paste(result_factor[[i]]$x, "_bin", sep = ""))
}
for (j in 1:length(result_all)) {
  for (i in 1:nrow(ac_valid)) {
    bin_name <- paste(result_all[[j]]$x, "_bin", sep = "")
    bin <- substr(ac_valid[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) { #If there is a missing bin
      bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
      ac_valid[[woe_name]][i] <- result_all[[j]]$ivtable[bin, "WoE"]
    } else {
      ac_valid[[woe_name]][i] <- result_all[[j]]$ivtable[bin, "WoE"]   #Calculates WOE for each observation
    }
  }
}

ac_valid$pred <- predict(initial_score, newdata=ac_valid, type='response')

smbinning.metrics(dataset = ac_valid, prediction = "pred", actualclass = "GB", report = 1)
smbinning.metrics(dataset = ac_valid, prediction = "pred", actualclass = "GB", report = 0, plot = "ks")
smbinning.metrics(dataset = ac_valid, prediction = "pred", actualclass = "GB", report = 0, plot = "auc")

##Add Scores to Initial Model
pdo <- 50
score <- 500
odds <- 20
fact <- pdo/log(2)
os <- score - fact*log(odds)
var_names <- names(initial_score$coefficients[-1])

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- ac_train[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  ac_train[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(ac_train)-nvar + 1)
colend <- ncol(ac_train)
ac_train$Score <- rowSums(ac_train[, colini:colend])

hist(ac_train$Score, breaks = 50, xlim = c(400,700), main = "Distribution of Train Scores", xlab = "Score")

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- ac_valid[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  ac_valid[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(ac_valid)-nvar + 1)
colend <- ncol(ac_valid)
ac_valid$Score <- rowSums(ac_valid[, colini:colend])

hist(ac_valid$Score, breaks = 50, xlim = c(400,700), main = "Distribution of Validation Scores", xlab = "Score")

accepts_scored <- rbind(ac_train, ac_valid)
hist(accepts_scored$Score, breaks = 50, xlim = c(400,700), main = "Distribution of Scores", xlab = "Score")

##### Reject Inference #####

##Clean & Prepare Reject Data
rejected_customers <- rejected_customers %>%
  mutate(CARDS = fct_collapse(CARDS, 
                              "Other credit car" = c("VISA Citibank","VISA Others")))

for(i in names(result_all_sig)) {
  result_all_sig[[i]]$bands[1] <- min(c(accepted_customers[[i]], rejected_customers[[i]]), na.rm = TRUE)
  result_all_sig[[i]]$bands[length(result_all_sig[[i]]$bands)] <- max(c(accepted_customers[[i]], rejected_customers[[i]]), na.rm = TRUE)
}
for(i in 1:length(rejected_customers[["INCOME"]])){
  rejected_customers[["INCOME"]][is.na(rejected_customers[["INCOME"]])] <- floor(mean(rejected_customers[["INCOME"]], na.rm = TRUE))
}
rejects_scored <- rejected_customers
for(i in 1:length(result_all_sig)) {
  rejects_scored <- smbinning.gen(df = rejects_scored, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}
for(i in 1:length(result_factor)){
  rejects_scored <- smbinning.factor.gen(df=rejects_scored, ivout = result_factor[[i]], chrname=paste(result_factor[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all)) {
  for (i in 1:nrow(rejects_scored)) {
    bin_name <- paste(result_all[[j]]$x, "_bin", sep = "")
    bin <- substr(rejects_scored[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_all[[j]]$ivtable)[1] - 1
      rejects_scored[[woe_name]][i] <- result_all[[j]]$ivtable[bin, "WoE"]
    } else {
      rejects_scored[[woe_name]][i] <- result_all[[j]]$ivtable[bin, "WoE"]
    }
  }
}

pdo <- 50
score <- 500
odds <- 20
fact <- pdo/log(2)
os <- score - fact*log(odds)
var_names <- names(initial_score$coefficients[-1])

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- rejects_scored[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  rejects_scored[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(rejects_scored)-nvar + 1)
colend <- ncol(rejects_scored)
rejects_scored$Score <- rowSums(rejects_scored[, colini:colend])

##Hard Cut-off
rejects_scored$pred <- predict(initial_score, newdata=rejects_scored, type='response')
rejected_customers$GB <- as.numeric(rejects_scored$pred > 0.0342)
rejected_customers$X_freq_ <- ifelse(rejected_customers$GB == 1, 1, 30)
rejected_customers$WOE_GB <- abs(rejected_customers$GB - 1)
comb_hard <- rbind(accepted_customers, rejected_customers)

##Parceling
parc <- seq(400, 700, 30)

accepts_scored$Score_parc <- cut(accepts_scored$Score, breaks = parc)
rejects_scored$Score_parc <- cut(rejects_scored$Score, breaks = parc)

#table(accepts_scored$Score_parc, accepts_scored$bad)
parc_perc <- table(accepts_scored$Score_parc, accepts_scored$GB)[,2]/rowSums(table(accepts_scored$Score_parc, accepts_scored$GB))
rejected_customers$GB <- 0
rej_bump <- 1

for(i in 1:(length(parc)-1)) {
  for(j in 1:length(rejects_scored$Score)) {
    if((rejects_scored$Score[j] > parc[i]) & 
       (rejects_scored$Score[j] <= parc[i+1]) & 
       (runif(n = 1, min = 0, max = 1) < (rej_bump*parc_perc[i]))) {
      rejected_customers$GB[j] <- 1
    }
  }
}
rejected_customers$X_freq_ <- ifelse(rejected_customers$GB == 1, 1, 30)
rejected_customers$WOE_GB <- abs(rejected_customers$GB - 1)

comb_parc <- rbind(accepted_customers, rejected_customers)

##Fuzzy Augmentation
rejects_scored$pred <- predict(initial_score, newdata = rejects_scored, type = 'response')
rejects_g <- rejected_customers
rejects_b <- rejected_customers

rejects_g$GB <- 0
rejects_g$X_freq_ <- (1 - rejects_scored$pred)*1
rejects_g$WOE_GB <- 1

rejects_b$GB <- 1
rejects_b$X_freq_ <- (rejects_scored$pred)*30
rejects_b$WOE_GB <- 0

comb_fuzz <- rbind(accepted_customers, rejects_g, rejects_b)

#####Build Final Scorecard Model#####

comb <- comb_parc # Select which data set you want to use from above techniques

set.seed(42)
train_id <- sample(seq_len(nrow(comb)), size = floor(0.70*nrow(comb)))

train_comb <- comb[train_id, ]
test_comb <- comb[-train_id, ]

iv_summary_com <- smbinning.sumiv(df = train_comb, y = "WOE_GB")

smbinning.sumiv.plot(iv_summary_com)
iv_summary_com

num_com <- names(train_comb)[sapply(train_comb, is.numeric)] # Gathering the names of numeric variables in data #
result_num <- list() # Creating empty list to store all results #
iv_summary_com_sub <- subset(iv_summary_com, Char %in% num_com)

for(i in 1:length(iv_summary_com_sub)){
  check_res <- iv_summary_com_sub$Char[i] #Store variable name
  if(iv_summary_com_sub$Process[i] == "Uniques values < 5") {
    next
  }
  else if(iv_summary_com_sub$Process[i] == "No significant splits") {
    next
  }
  else if(iv_summary_com_sub$IV[i] < 0.1) {
    next
  }
  else {
    result_num[[check_res]] <- smbinning(df = train_comb, y = "WOE_GB", x = check_res)
  }
}
# Binning of Factor Variables #
char_com <- names(train_comb)[sapply(train_comb, is.factor)]
iv_summary_fac <- subset(iv_summary_com, Char %in% char_com)
result_fac <- list()
for(i in 1:length(iv_summary_fac)){
  check <- iv_summary_fac$Char[i] #Store variable name
  if(iv_summary_fac$Process[i] == "NA Values contain comma") {
    next
  }
  else if(iv_summary_fac$IV[i] < 0.1) {
    next
  }
  else {
    result_fac[[check]] <- smbinning.factor(df = train_comb, y = "WOE_GB", x = check)
  }
}

for(i in 1:length(result_num)) {
  train_comb <- smbinning.gen(df = train_comb, ivout = result_num[[i]], chrname = paste(result_num[[i]]$x, "_bin", sep = ""))
}
for(i in 1:length(result_fac)){
  train_comb <- smbinning.factor.gen(df=train_comb, ivout = result_fac[[i]], chrname=paste(result_fac[[i]]$x, "_bin", sep = ""))
}
result_com <- c(result_num,result_fac)
for (j in 1:length(result_com)) {
  for (i in 1:nrow(train_comb)) {
    bin_name <- paste(result_com[[j]]$x, "_bin", sep = "")
    bin <- substr(train_comb[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_com[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_com[[j]]$ivtable)[1] - 1
      train_comb[[woe_name]][i] <- result_com[[j]]$ivtable[bin, "WoE"]
    } else {
      train_comb[[woe_name]][i] <- result_com[[j]]$ivtable[bin, "WoE"]
    }
  }
}

final_score <- glm(data = train_comb, GB ~ AGE_WOE + 
                     TMJOB1_WOE + 
                     PERS_H_WOE+CARDS_WOE,
                   weights = train_comb$X_freq_, family = "binomial")
summary(final_score)

train_comb$pred <- final_score$fitted.values

smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "GB", report = 1)
smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "GB", report = 0, plot = "ks")
smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "GB", report = 0, plot = "auc")

for(i in 1:length(result_num)) {
  test_comb <- smbinning.gen(df = test_comb, ivout = result_num[[i]], chrname = paste(result_num[[i]]$x, "_bin", sep = ""))
}
for(i in 1:length(result_fac)){
  test_comb <- smbinning.factor.gen(df=test_comb, ivout = result_fac[[i]], chrname=paste(result_fac[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_com)) {
  for (i in 1:nrow(test_comb)) {
    bin_name <- paste(result_com[[j]]$x, "_bin", sep = "")
    bin <- substr(test_comb[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_com[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_com[[j]]$ivtable)[1] - 1
      test_comb[[woe_name]][i] <- result_com[[j]]$ivtable[bin, "WoE"]
    } else {
      test_comb[[woe_name]][i] <- result_com[[j]]$ivtable[bin, "WoE"]
    }
  }
}

test_comb$pred <- predict(final_score, newdata=test_comb, type='response')

smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "GB", report = 1)
smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "GB", report = 0, plot = "ks")
smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "GB", report = 0, plot = "auc")

pdo <- 50
score <- 500
odds <- 20
fact <- pdo/log(2)
os <- score - fact*log(odds)
var_names <- names(final_score$coefficients[-1])

for(i in var_names) {
  beta <- final_score$coefficients[i]
  beta0 <- final_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- train_comb[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  train_comb[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(train_comb)-nvar + 1)
colend <- ncol(train_comb)
train_comb$Score <- rowSums(train_comb[, colini:colend])

hist(train_comb$Score, breaks = 50,xlim = c(400,700), main = "Distribution of Scores", xlab = "Score")

for(i in var_names) {
  beta <- final_score$coefficients[i]
  beta0 <- final_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- test_comb[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  test_comb[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(test_comb)-nvar + 1)
colend <- ncol(test_comb)
test_comb$Score <- rowSums(test_comb[, colini:colend])

hist(test_comb$Score, breaks = 50, xlim = c(300,800), main = "Distribution of Test Scores", xlab = "Score")

accepts_scored_comb <- rbind(train_comb, test_comb)
hist(accepts_scored_comb$Score, breaks = 50, xlim = c(400,700), main = "Distribution of Scores", xlab = "Score")


##Calculations of Default, Acceptance Rate, and Profit by Cut-off Score
def <- NULL
acc <- NULL
prof <- NULL
score <- NULL
cost <- 52000
profit <- 2000

for(i in min(floor(train_comb$Score)):max(floor(train_comb$Score))){
  score[i - min(floor(train_comb$Score)) + 1] <- i
  def[i - min(floor(train_comb$Score)) + 1] <- 100*sum(train_comb$GB[which(train_comb$Score >= i)])/(length(train_comb$GB[which(train_comb$Score >= i & train_comb$GB == 1)]) + 30*length(train_comb$GB[which(train_comb$Score >= i & train_comb$GB == 0)]))
  acc[i - min(floor(train_comb$Score)) + 1] <- 100*(length(train_comb$GB[which(train_comb$Score >= i & train_comb$GB == 1)]) + 30*length(train_comb$GB[which(train_comb$Score >= i & train_comb$GB == 0)]))/(length(train_comb$GB[which(train_comb$GB == 1)]) + 30*length(train_comb$GB[which(train_comb$GB == 0)]))
  prof[i - min(floor(train_comb$Score)) + 1] <- length(train_comb$GB[which(train_comb$Score >= i & train_comb$GB == 1)])*(-cost) + 30*length(train_comb$GB[which(train_comb$Score >= i & train_comb$GB == 0)])*profit
}

plot_data <- data.frame(def, acc, prof, score)

##Plot of Acceptance Rate by Default Rate
ay1 <- list(
  title = "Default Rate (%)",
  range = c(0, 10)
)
ay2 <- list(
  tickfont = list(),
  range = c(0, 100),
  overlaying = "y",
  side = "right",
  title = "Acceptance Rate (%)"
)
fig <- plot_ly()
fig <- fig %>% add_lines(x = ~score, y = ~def, name = "Default Rate (%)")
fig <- fig %>% add_lines(x = ~score, y = ~acc, name = "Acceptance Rate (%)", yaxis = "y2")
fig <- fig %>% layout(
  title = "Default Rate by Acceptance Across Score", yaxis = ay1, yaxis2 = ay2,
  xaxis = list(title="Scorecard Value"),
  legend = list(x = 1.2, y = 0.8)
)

fig
##Plot of Acceptance Rate by Profit
ay1 <- list(
  title = "Profit ($)",
  showline = FALSE,
  showgrid = FALSE
)
ay2 <- list(
  tickfont = list(),
  range = c(0, 100),
  overlaying = "y",
  side = "right",
  title = "Acceptance Rate (%)"
)
fig <- plot_ly()
fig <- fig %>% add_lines(x = ~score, y = ~prof, name = "Profit ($)")
fig <- fig %>% add_lines(x = ~score, y = ~acc, name = "Acceptance Rate (%)", yaxis = "y2")
fig <- fig %>% layout(
  title = "Profit by Acceptance Across Score", yaxis = ay1, yaxis2 = ay2,
  xaxis = list(title="Scorecard Value"),
  legend = list(x = 1.2, y = 0.8)
)

fig

cutpoints <- quantile(accepts_scored_comb$Score, probs = seq(0,1,0.1))
accepts_scored_comb$Score.QBin <- cut(accepts_scored_comb$Score, breaks=cutpoints, include.lowest=TRUE)
Default.QBin.pop <- round(table(accepts_scored_comb$Score.QBin, accepts_scored_comb$GB)[,2]/(table(accepts_scored_comb$Score.QBin, accepts_scored_comb$GB)[,2] + table(accepts_scored_comb$Score.QBin, accepts_scored_comb$GB)[,1]*30)*100,2)

print(Default.QBin.pop)

barplot(Default.QBin.pop, 
        main = "Default Decile Plot", 
        xlab = "Deciles of Scorecard",
        ylab = "Default Rate (%)", ylim = c(0,12),
        col = saturation(heat.colors, scalefac(0.8))(10))
abline(h = 1.58, lwd = 2, lty = "dashed")
text(10, 5, "Current = 1.89%")
