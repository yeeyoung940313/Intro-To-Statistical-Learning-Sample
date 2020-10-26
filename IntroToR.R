############################################################
### Intro
############################################################

### Basic Commands
# show a list of all the objects
ls()

# delete some you dont want
x <- c(1, 2, 3)
rm(x)

# to delete all that you want 
rm(list=ls())

### Matrix
?matrix
x=matrix(data=c(1,2,3,4), nrow=2, ncol=2)
x # note that it is by column

#alternatively
x=matrix(c(1,2,3,4), nrow=2, ncol=2, byrow='True')
x


### Random Value Generator

x=rnorm(50)
y=x+rnorm(50, mean=50,sd=0.1)
cor(x,y)

### Indexing
A=matrix(1:16, nrow=4, ncol=4)

A[,1:2]
A[-c(1,3),]

dim(A)


