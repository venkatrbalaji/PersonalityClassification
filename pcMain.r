install.packages("Matrix")
install.packages("irlba")
install.packages("topicmodels")
install.packages("corrplot")
install.packages("ggplot2", "reshape2")
install.packages("reshape2")
install.packages('ROCR')

# Input Dataset into user,likes and userlikes table
users <- read.csv(file = "C:\Users\HP\Downloads\SWM\users.csv")
likes <- read.csv(file = "C:\Users\HP\Downloads\SWM\likes.csv")
users_likes <- read.csv(file = "C:\Users\HP\Downloads\SWM\users-likes.csv")

# Match indices of users into userlikes[userrow] and indices of likes into userlikes[likerow]
users_likes$user_row<-match(users_likes$userid,users$userid)
users_likes$like_row<-match(users_likes$likeid,likes$likeid)

# Load Matrix library
require(Matrix)

# Construct the sparse matrix also known as a footprint matrix
ufp <- sparseMatrix(i = users_likes$user_row, j = users_likes$like_row, x = 1)

# Save user IDs as row names in the sparse matrix
rownames(ufp) <- users$userid

# Save Like names as column names in the sparse matrix
colnames(ufp) <- likes$name

# Data Processing removing users that gave less than 50 likes and pages that have been liked by less than 150 users
repeat {                                       
  i <- sum(dim(ufp))                             
  ufp <- ufp[rowSums(ufp) >= 50, colSums(ufp) >= 150] 
  if (sum(dim(ufp)) == i) break                  
}

#match the processed sparse matrix to user matrix by removing noise users
users <- users[match(rownames(ufp), users$userid), ]

# Preset the random number generator in R for the comparability of the results
set.seed(seed = 76)

# Load irlba and extract 5 SVD dimensions from the sparse footprint matrix
library(irlba)
Msvd <- irlba(ufp, nv = 5)

# User SVD left score(u) and like SVD right scores(v) are here:
u <- Msvd$u
v <- Msvd$v

# Scatter plot of singular values of d corresponding to the 5 dimensions
plot(Msvd$d)

# Fetch the v_rot like matrix through varimax rotation:
v_rot <- unclass(varimax(Msvd$v)$loadings)

# Cross product v_rot with ufp sparse footprint matrix to get u_rot:
u_rot <- as.matrix(ufp %*% v_rot)

library(topicmodels)

# # Perform LDA analysis for 5 topics
# Mlda <- LDA(ufp, control = list(alpha = 10, delta = .1, seed=76), k = 5, method = "Gibbs")
# 
# # Find User Cluster Membership probabilities for 5 dimensions(19782*5)
# gamma <- Mlda@gamma
# 
# # Find Like Cluster Membership probabilities for 5 dimensions(5*8732) and exponentiate it
# beta <- exp(Mlda@beta)
# 
# # Run and fetch the exponential likelihood of log for 2,3,4 and 5 topics: 
# lg <- list()
# for (i in 2:5) {
#   Mlda <- LDA(ufp, k = i, control = list(alpha = 10, delta = .1, seed = 68), method = "Gibbs")
#   lg[[i]] <- logLik(Mlda) 
# }

#Plot the Log values obtained versus the 5 topics
plot(2:5, unlist(lg))   

# SVD Correlate user traits and their SVD scores removing user id column m*n and m*k = n*k
cor(u_rot, users[,-1], use = "pairwise")

# LDA Correlate user traits and their LDA scores removing user id column m*n and m*k = n*k
# cor(gamma, users[,-1], use = "pairwise")


library(corrplot)
library(ggplot2)
library(reshape2)

# Fetch the Top and Bottom Liked Pages based on SVD Scores
top <- list()
bottom <-list()
for (i in 1:5) {
  f <- order(v_rot[ ,i])
  temp <- tail(f, n = 10)
  top[[i]]<-colnames(ufp)[temp]  
  temp <- head(f, n = 10)
  bottom[[i]]<-colnames(ufp)[temp]  
}

# Correlate the SVD u_rot matrix and users matrix removing user ids
x<-round(cor(u_rot, users[,-1], use="p"),2)

# Melt the correlated matrix into a molten dataframe
y<-melt(x)
colnames(y)<-c("SVD", "Trait", "r")

# Generate Heatmap of SVD scores and the dimensions
qplot(x=SVD, y=Trait, data=y, fill=r, geom="tile") +
  scale_fill_gradient2(limits=range(x), breaks=c(min(x), 0, max(x)))+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"),
        panel.background = element_rect(fill='white', colour='white'))+
  labs(x=expression('SVD'[rot]), y=NULL)

# # Fetch the Top Liked Pages based on LDA Scores
# top <- list()
# for (i in 1:5) {
#   f <- order(beta[i,])
#   temp <- tail(f, n = 10)
#   top[[i]]<-colnames(ufp)[temp]  
# }
# 
# x<-round(cor(gamma, users[,-1], use="p"),2)
# y<-melt(x)
# colnames(y)<-c("LDA", "Trait", "r")
# 
# # Generate Heatmap of LDA scores and the dimensions
# qplot(x=LDA, y=Trait, data=y, fill=r, geom="tile") +
#   scale_fill_gradient2(limits=range(x), breaks=c(min(x), 0, max(x)))+
#   theme(axis.text=element_text(size=12), 
#         axis.title=element_text(size=14,face="bold"),
#         panel.background = element_rect(fill='white', colour='white'))+
#   labs(x=expression('LDA'), y=NULL)

#Model Training and Testing
# Split users into 10 groups where 1 group is used to Test and the other 9 are used to Train
folds <- sample(1:10, size = nrow(users), replace = T)
test <- folds == 1

library(irlba)

# Extract 50 SVD dimensions from the TRAINING data
Msvd <- irlba(ufp[!test, ], nv = 50)

# Fetch the v_rot like matrix through varimax rotation:
v_rot <- unclass(varimax(Msvd$v)$loadings)

# Cross product v_rot with ufp sparse footprint matrix to get u_rot:
u_rot <- as.data.frame(as.matrix(ufp %*% v_rot))

# Linear regression model for openness 
fit_o <- glm(users$ope~., data = u_rot, subset = !test)

# Linear regression model for conscientiousness
fit_c <- glm(users$con~., data = u_rot, subset = !test)

# Build linear regression model for extrovertness
fit_e <- glm(users$ext~., data = u_rot, subset = !test)

# Build linear regression model for agreeableness
fit_a <- glm(users$agr~., data = u_rot, subset = !test)

# Build linear regression model for neuroticism
fit_n <- glm(users$neu~., data = u_rot, subset = !test)

# Build Logistic Regression Model for Gender
fit_g <- glm(users$gender~.,data = u_rot, subset = !test, family = "binomial")

# Build Linear Regression Model for Age
fit_ag <- glm(users$age~.,data = u_rot, subset = !test)

# Build Logistic Regression Model for Political
fit_p <- glm(users$pol~.,data = u_rot, subset = !test, family = "binomial")

# Compute the predictions for the TEST subset
pred_o <- predict(fit_o, u_rot[test, ])
pred_c <- predict(fit_c, u_rot[test, ])
pred_e <- predict(fit_e, u_rot[test, ])
pred_a <- predict(fit_a, u_rot[test, ])
pred_n <- predict(fit_n, u_rot[test, ])
pred_g <- predict(fit_g, u_rot[test, ], type = "response")
pred_ag <- predict(fit_ag, u_rot[test, ])
pred_p <- predict(fit_p, u_rot[test, ], type = "response")

# Correlation of predicted and actual values for Linear Regression Implemented OCEAN Parameteres and Age
r <- cor(users$ope[test], pred_o)
r <- cor(users$con[test], pred_c)
r <- cor(users$ext[test], pred_e)
r <- cor(users$agr[test], pred_a)
r <- cor(users$neu[test], pred_n)
r <- cor(users$age[test], pred_ag)

# Area under the curve for Logistic Regression Implemented Gender and Political
library(ROCR)
temp_gen <- prediction(pred_g, users$gender[test])
auc_gen <- performance(temp_gen,"auc")@y.values

temp_political <- prediction(pred_p, users$political[test])
auc_political <- performance(temp_political,"auc")@y.values

# Start predictions for multiple values of dimensions
set.seed(seed=76)
n_folds<-10                # set number of folds
kvals<-c(10,30,60,90,120,150)      # set k
vars<-colnames(users)[-1]  # choose variables to predict

folds <- sample(1:n_folds, size = nrow(users), replace = T)

results<-list()
accuracies<-c()

for (k in kvals){
  print(k)
  for (fold in 1:n_folds){ 
    print(paste("Cross-validated predictions, fold:", fold))
    test <- folds == fold
    
    Msvd <- irlba(ufp[!test, ], nv = k)
    v_rot <- unclass(varimax(Msvd$v[, 1:k])$loadings)
    predictors <- as.data.frame(as.matrix(ufp %*% v_rot))
    
    
    for (var in vars){
      print(var)
      results[[var]]<-rep(NA, n = nrow(users))
      
      if (length(unique(na.omit(users[,var]))) ==2) {    
        fit <- glm(users[,var]~., data = predictors, subset = !test, family = "binomial")
        results[[var]][test] <- predict(fit, predictors[test, ], type = "response")
      } else {
        fit<-glm(users[,var]~., data = predictors, subset = !test)
        results[[var]][test] <- predict(fit, predictors[test, ])
      }
      print(paste(" Variable", var, "done."))
    }
  }
  
  compute_accuracy <- function(ground_truth, predicted){
    if (length(unique(na.omit(ground_truth))) ==2) {
      f<-which(!is.na(ground_truth))
      temp <- prediction(predicted[f], ground_truth[f])
      return(performance(temp,"auc")@y.values)
    } else {return(cor(ground_truth, predicted,use = "pairwise"))}
  }
  
  for (var in vars) accuracies <- c(accuracies,compute_accuracy(users[,var][test], results[[var]][test]))
  
}
print(accuracies)

traits <- c('Gender','Age','Political-Factor','Openness','Conscientiousness','Extroversion','Agreeableness','Neuroticism')

k <- c(10,10,10,10,10,10,10,10,30,30,30,30,30,30,30,30,60,60,60,60,60,60,60,60,90,90,90,90,90,90,90,90,120,120,120,120,120,120,120,120,150,150,150,150,150,150,150,150)
names <- c("SVD","SVD","SVD","SVD","SVD","SVD","SVD","SVD","LDA","LDA","LDA","LDA","LDA","LDA","LDA","LDA")

data_val<-data.frame(People_Personality_Traits=traits, accuracies=as.numeric(accuracies),k=k)
# Plot Personality traits against the accuracies for K SVD dimensions utilized
ggplot(data_val,aes(x = People_Personality_Traits, y = accuracies, group = k, color = k)) + 
  geom_line()+
  theme_light() +
  ggtitle("Predition-Accuracy with Change in Dimensions")+
  ylab(label="Reported Accuracies") + 
  xlab("User_Personality_Traits")+       
  geom_point()


# Choose optimal number of dimensions K
ks<-c(2:10,15,20,30,50,75,100)

rs <- list()

# Run the code below for each k in ks
for (k in ks){
  v_rot <- unclass(varimax(Msvd$v[, 1:k])$loadings)
  u_rot <- as.data.frame(as.matrix(ufp %*% v_rot))
  fit_o <- glm(users$ope~., data = u_rot, subset = !test)
  pred_o <- predict(fit_o, u_rot[test, ])
  rs[[as.character(k)]] <- cor(users$ope[test], pred_o)
}
data<-data.frame(k=ks, r=as.numeric(rs))

# Plot the optimality graph of K number of dimension values
ggplot(data=data, aes(x=k, y=r, group=1)) + 
  theme_light() +
  stat_smooth(colour="red", linetype="dashed", size=1,se=F) + 
  geom_point(colour="red", size=2, shape=21, fill="white") +
  scale_y_continuous(breaks = seq(0, .5, by = 0.05))


# SVD vs LDA

library(irlba)
library(ROCR)
print(traits)

compute_accuracy <- function(ground_truth, predicted){
  if (length(unique(na.omit(ground_truth))) ==2) {
    f<-which(!is.na(ground_truth))
    temp <- prediction(predicted[f], ground_truth[f])
    return(performance(temp,"auc")@y.values)
  } else {return(cor(ground_truth, predicted,use = "pairwise"))}
}

results_lda<-list()
results_svd<-list()
accuracies_svd_vs_lda<-c()
comparison_k<-5
algos<-c(1,2)
n_folds<-2
folds <- sample(1:n_folds, size = nrow(users), replace = T)

for (algo in algos){
  if (algo == 1){
    print("SVD")
    for (fold in 1:n_folds){ 
      print(paste("Cross-validated predictions, fold:", fold))
      test <- folds == fold
      
      Msvd <- irlba(ufp[!test, ], nv = comparison_k)
      v_rot <- unclass(varimax(Msvd$v[, 1:comparison_k])$loadings)
      predictors_svd <- as.data.frame(as.matrix(ufp %*% v_rot))
      
      for (var in vars){
        print(var)
        results_svd[[var]]<-rep(NA, n = nrow(users))
        
        if (length(unique(na.omit(users[,var]))) ==2) {    
          fit <- glm(users[,var]~., data = predictors_svd, subset = !test, family = "binomial")
          results_svd[[var]][test] <- predict(fit, predictors_svd[test, ], type = "response")
        } else {
          fit<-glm(users[,var]~., data = predictors_svd, subset = !test)
          results_svd[[var]][test] <- predict(fit, predictors_svd[test, ])
        }
        print(paste(" Variable", var, "done."))
      }
    }
    
    for (var in vars) accuracies_svd_vs_lda <- c(accuracies_svd_vs_lda,compute_accuracy(users[,var][test], results_svd[[var]][test]))
  }
  if (algo == 2){
    print("LDA")
    for (fold in 1:n_folds){ 
      print(paste("Cross-validated predictions, fold:", fold))
      test <- folds == fold
      
      # Msvd <- irlba(ufp[!test, ], nv = 5)
      # v_rot <- unclass(varimax(Msvd$v[, 1:5])$loadings)
      # predictors <- as.data.frame(as.matrix(ufp %*% v_rot))
      
      Mlda <- LDA(ufp, control = list(alpha = 10, delta = .1, seed=76, verbose=5), k = comparison_k, method = "Gibbs")
      
      # Find User Cluster Membership probabilities for 5 dimensions(19782*5)
      predictors_lda <- as.data.frame(Mlda@gamma)
      
      # Find Like Cluster Membership probabilities for 5 dimensions(5*8732) and exponentiate it
      #predictors <- as.data.frame(exp(Mlda@beta))
      #nrow(ufp[!test, ])
      
      for (var in vars){
        print(var)
        results_lda[[var]]<-rep(NA, n = nrow(users))
        
        if (length(unique(na.omit(users[,var]))) ==2) {    
          fit <- glm(users[,var]~., data = predictors_lda, subset = !test, family = "binomial")
          results_lda[[var]][test] <- predict(fit, predictors_lda[test, ], type = "response")
        } else {
          fit<-glm(users[,var]~., data = predictors_lda, subset = !test)
          results_lda[[var]][test] <- predict(fit, predictors_lda[test, ])
        }
        print(paste(" Variable", var, "done."))
      }
    }
    
    for (var in vars) accuracies_svd_vs_lda <- c(accuracies_svd_vs_lda,compute_accuracy(users[,var][test], results_lda[[var]][test])) 
  }
  
}


#accuracies_1 <- c(0.93,0.60,0.85,0.44,0.21,0.28,0.22,0.30,0.87,0.67,0.80,0.41,0.20,0.25,0.17,0.25)
data_val<-data.frame(People_Personality_Traits=traits, accuracies=as.numeric(accuracies_svd_vs_lda),k=names)
# Performance of SVD against the LDA
library(ggplot2)
ggplot(data_val,aes(x = People_Personality_Traits, y = as.numeric(accuracies_svd_vs_lda), group = names, color = names)) + 
  geom_line()+
  theme_light() +
  ggtitle("SVD vs LDA")+
  ylab(label="Reported Accuracies") + 
  xlab("User_Personality_Traits")+       
  geom_point()
ggsave("test1.tiff", width = 30, height = 20 , units = "cm")

