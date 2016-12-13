getwd()

#get the train and test data
train <- read.csv('/Users/karansharma/Downloads/Data/USAIncome/train.csv', header = TRUE, sep = ',')
test <- read.csv('/Users/karansharma/Downloads/Data/USAIncome/test.csv', header = TRUE, sep = ',')

#Looking at the data

#first 5 rows
train[1:5,]

#structure of the data
str(train)
str(test)

dim(train)
dim(test)

View(train)

#looking at the target variable
unique(train$income_level)
unique(test$income_level)

#encode the target variable to binary variables
#install.packages('data.table')
library(data.table)
train = as.data.table(train)
train[,income_level := ifelse(income_level == "-50000", 0, 1)]
test[, income_level := ifelse(income_level == "-50000", 0, 1)]

# we figured out data is imbalanced
prop.table(table(train$income_level))
test = as.data.table(test)
prop.table(table(test$income_level))

#The columns data types are different from what it is given in the data description 
#here 
factcols <- c(2:5,7,8:16,20:29,31:38,40,41)
numcols <- setdiff(1:40,factcols)

#make the factcols in train and test as factors and numcols as numerics. Do the same for test.
# .SD is subset of data table that are mentioned in .SDcols 
train[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

test[,(factcols) := lapply(.SD, as.factor), .SDcols = factcols]
test[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

#Visulization using ggplot
#install.packages('ggplot2')
library(ggplot2)
help(ggplot)

#density plot function
func_Dens_Hist <- function(a){
  ggplot(data=train, aes(x=a, y=..density..)) + 
    geom_histogram(color = "red", fill = "blue", alpha=0.5,bins = 100) + 
    geom_density()
}

#plot age
func_Dens_Hist(train$age)

#plot weeks worked in year
func_Dens_Hist(train$weeks_worked_in_year)

#plot capital loss
func_Dens_Hist(train$capital_losses)

#scatter plot
ggplot(data=train, aes(x=age,y=wage_per_hour,colour= (income_level))) +
  geom_point() + 
  scale_y_continuous("wage per labour", breaks = seq(0,10000, 1000))

#bar plot of catagorical value
#We use dodge to seperate binary catorical value in different bars
func_bar_plot <- function(a){
  ggplot(data = train, aes(x=a, fill = income_level)) + 
    geom_bar(position = "dodge", color = "black") + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1.0, size = 8)) +
    scale_fill_brewer(palette = 5)
}

#bar plot of class of the worker
func_bar_plot(train$class_of_worker)

#bar plot of the sex
func_bar_plot(train$sex)

func_bar_plot(train$member_of_labor_union)

func_bar_plot(train$education)

#-----------------Data Cleansing-------------------------------------------------

#checking the missing values in the numeric cols 
#test
is.na(test[,numcols])
#train
is.na(train[,numcols])

#find correlation between variables 
library(caret)
corr_train <- findCorrelation(x=train, cutoff = 0.7)

##?? why is.ne is not working for factorial values 

#checking the same for catagorical columns 
is.na(test[,factcols])
#train
is.na(train[,factcols])



