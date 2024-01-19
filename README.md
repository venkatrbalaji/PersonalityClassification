# PersonalityClassification - Semantic Web Mining Academic Project
Every individual of the modern generation often uses the social media and the likes of the users play a prime role in defining the user personality. 
The data on the social networking platforms help in analyzing individual users personality and find meaningful insights.
Our motive is to use supervised and unsupervised ML techniques to predict a user’s personality based on age, gender, political views and pages liked.
The five components used to identify an individual’s personality are the OCEAN attributes.

## DATASET:
Source: https://www.michalkosinski.com/data-mining-tutorial
The dataset consists of 3 datasets(users.csv,likes.csv and user-likes.csv) of Facebook user profiles and their social media likes

## Requirements:
 Installation of RStudio to execute the scripts.
 Download datasets and update the path of the dataset folder in the scripts
 
## Scripts:
### pcMain.r:
This script performs the following operations,
* Build a user-likes footprint sparse matrix
* Use SVD (Singular Value Decomposition) and LDA (Laten Dirichlet Allocation) machine learning algorithms to perform Data mining operations on the input dataset
### political_wordcloud.r:
This script performs the following operations,
* Build a user-likes footprint sparse matrix
* Cluster users into 2 groups using LDA and build wordclouds using the liked page names to reperesent the differences in their sentiments, activities and view points.
