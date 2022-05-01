install.packages("Matrix")
install.packages("irlba")
install.packages("topicmodels")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("reshape2")
install.packages('ROCR')
install.packages('dplyr',dependencies=TRUE)
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("wordcloud2")
install.packages("tm",dependencies=TRUE)
install.packages('Rcpp') 
install.packages("plotrix")

# Input Dataset into user,likes and userlikes table
users <- read.csv(file = "../data/users.csv")
likes <- read.csv(file = "../data/likes.csv")
users_likes <- read.csv(file = "../data/users-likes.csv")

# A copy of users data.frame to include only those with political influence
#pol_users <- users  # uncomment for processing full data
pol_users <- users[users$political==1,]  # uncomment for processing politically influential user data
pol_users <- na.omit(pol_users)

pol_users_likes <- users_likes
pol_users_likes$user_row<-match(pol_users_likes$userid,pol_users$userid)
pol_users_likes <- na.omit(pol_users_likes)


pol_users_likes$like_row<-match(pol_users_likes$likeid,likes$likeid)
pol_users_likes <- na.omit(pol_users_likes)

xj <- merge(pol_users_likes, likes, by='likeid')
xc<-xj[,c('like_row', 'likeid', 'name')]
pol_likes <- xc[order(xc$like_row),]
pol_likes <- unique(pol_likes[, c('likeid', 'name')])

pol_users_likes$like_row<-match(pol_users_likes$likeid,pol_likes$likeid)
pol_users_likes <- na.omit(pol_users_likes)


# Load Matrix library
require(Matrix)


# Construct the sparse matrix also known as a footprint matrix on political users
pol_ufp <- sparseMatrix(i = pol_users_likes$user_row, j = pol_users_likes$like_row, x = 1)


### Political users

#temp_likes = merge()
dim(pol_ufp)

dim(pol_likes)
dim(likes)

dim(users_likes)
dim(pol_users_likes)
# Save user IDs as row names in the sparse matrix
rownames(pol_ufp) <- pol_users$userid

# Save Like names as column names in the sparse matrix
colnames(pol_ufp) <- pol_likes$name

# Data Processing removing users that gave less than 50 likes and pages that have been liked by less than 150 users
repeat {                                       
  i <- sum(dim(pol_ufp))                             
  pol_ufp <- pol_ufp[rowSums(pol_ufp) >= 10, colSums(pol_ufp) >= 50] 
  if (sum(dim(pol_ufp)) == i) break                  
}

#match the processed sparse matrix to user matrix by removing noise users
pol_users <- pol_users[match(rownames(pol_ufp), pol_users$userid), ]
pol_likes <- pol_likes[match(colnames(pol_ufp), pol_likes$name), ]


library(topicmodels)

# Perform LDA analysis for 5 topics
Mlda <- LDA(pol_ufp, control = list(alpha = 10, delta = .1, seed=76, verbose=10), k = 2, method = "Gibbs")

# Find User Cluster Membership probabilities for 5 dimensions(19782*5)
gamma <- Mlda@gamma

# Find Like Cluster Membership probabilities for 5 dimensions(5*8732) and exponentiate it
beta <- exp(Mlda@beta)

# # Run and fetch the exponential likelihood of log for 2,3,4 and 5 topics: 
# lg <- list()
# for (i in 2:5) {
#   Mlda <- LDA(ufp, k = i, control = list(alpha = 10, delta = .1, seed = 68), method = "Gibbs")
#   lg[[i]] <- logLik(Mlda) 
# }
# 
# #Plot the Log values obtained versus the 5 topics
# plot(2:5, unlist(lg))   
# 
# # SVD Correlate user traits and their SVD scores removing user id column m*n and m*k = n*k
# cor(u_rot, pol_users[,-1], use = "pairwise")
# 
# # LDA Correlate user traits and their LDA scores removing user id column m*n and m*k = n*k
# cor(gamma, pol_users[,-1], use = "pairwise")


library(corrplot)
library(ggplot2)
library(reshape2)

# # Fetch the Top and Bottom Liked Pages based on SVD Scores
# top <- list()
# bottom <-list()
# for (i in 1:5) {
#   f <- order(v_rot[ ,i])
#   temp <- tail(f, n = 10)
#   top[[i]]<-colnames(pol_ufp)[temp]  
#   temp <- head(f, n = 10)
#   bottom[[i]]<-colnames(pol_ufp)[temp]  
# }
# 
# # Correlate the SVD u_rot matrix and users matrix removing user ids
# x<-round(cor(u_rot, pol_users[,-1], use="p"),2)
# 
# # Melt the correlated matrix into a molten dataframe
# y<-melt(x)
# colnames(y)<-c("SVD", "Trait", "r")
# 
# # Generate Heatmap of SVD scores and the dimensions
# qplot(x=SVD, y=Trait, data=y, fill=r, geom="tile") +
#   scale_fill_gradient2(limits=range(x), breaks=c(min(x), 0, max(x)))+
#   theme(axis.text=element_text(size=12), 
#         axis.title=element_text(size=14,face="bold"),
#         panel.background = element_rect(fill='white', colour='white'))+
#   labs(x=expression('SVD'[rot]), y=NULL)

# Fetch the Top Liked Pages based on LDA Scores
top <- list()
for (i in 1:2) {
  f <- order(beta[i,])
  temp <- tail(f, n = 10)
  top[[i]]<-colnames(pol_ufp)[temp]  
}

x<-round(cor(gamma, pol_users[,-1], use="p"),2)
y<-melt(x)
colnames(y)<-c("LDA", "Trait", "r")


# Generate Heatmap of LDA scores and the dimensions
qplot(x=LDA, y=Trait, data=y, fill=r, geom="tile") +
  scale_fill_gradient2(limits=range(x), breaks=c(min(x), 0, max(x)))+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"),
        panel.background = element_rect(fill='white', colour='white'))+
  labs(x=expression('LDA'), y=NULL)


# 
# pol_users[, c('age', 'gender')]
# 
# pol_users$avg_score = rowMeans(pol_users[,c("ope", "con", "ext", "agr", "neu")])
# 
# var1 <- 'age'
# var2 <- 'avg_score'
# 
# plot(pol_users[,var1], pol_users[,var2])
# 
# plot(gamma[,2], gamma[,1])
# 
# plot(u_rot[,1], u_rot[,2])

gam_df <- data.frame(gamma)

gam_df$group1 <- gam_df$X1>gam_df$X2
gam_df$group2 <- gam_df$X1<gam_df$X2

rownames(gam_df) <- pol_users$userid

gam_df <- gam_df[, c("group1", "group2")]
# User group strengths
user_groups <- as.data.frame(colSums(gam_df))
colnames(user_groups) <- c("No:of Users")

bet_df <- data.frame(beta)
colnames(bet_df) <- pol_likes$name

bet_df_trans <- data.frame(t(bet_df))
colnames(bet_df_trans) <- c("x1", "x2")
g1_dom_pages <- bet_df_trans[order(-bet_df_trans$x1),]
g2_dom_pages <- bet_df_trans[order(-bet_df_trans$x2),]



library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(dplyr)
library(tm)
library(Rcpp)


# GROUP 1
text <- rownames(g1_dom_pages[1:nrow(g1_dom_pages/2),])

# Create a corpus  
docs <- Corpus(VectorSource(text))
# docs <- docs %>%
#   tm_map(removeNumbers) %>%
#   tm_map(removePunctuation) %>%
#   tm_map(stripWhitespace)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(1234)

#wordcloud(words=df$word, freq=df$freq, min.freq=1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

wordcloud2(data=df, size = 0.5, shape = 'pentagon')

# GROUP 2

text2 <- rownames(g2_dom_pages[1:nrow(g2_dom_pages)/2,])
#text2 <- rownames(g1_dom_pages[nrow(g1_dom_pages/2):,])

# Create a corpus  
docs2 <- Corpus(VectorSource(text2))
docs2 <- tm_map(docs2, removeNumbers)
docs2 <- tm_map(docs2, removePunctuation)
docs2 <- tm_map(docs2, stripWhitespace)
docs2 <- tm_map(docs2, content_transformer(tolower))
docs2 <- tm_map(docs2, removeWords, stopwords("english"))

dtm2 <- TermDocumentMatrix(docs2) 
matrix2 <- as.matrix(dtm2) 
words2 <- sort(rowSums(matrix2),decreasing=TRUE) 
df2 <- data.frame(word = names(words2),freq=words2)

set.seed(1234)

#wordcloud(words=df2$word, freq=df2$freq, min.freq=1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

#shape = "random-dark"
wordcloud2(data=df2, size = 0.5, shape = 'pentagon')

