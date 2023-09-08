a <- 1
b <- 2
#y is sum of a and b
y <- a + b
y

a <- 5
b <- 8
z <- a*b
z

a <- 5
b <- 8
z <- a *b
z

a <- 1
b <- 2
y <- sum(a,b)
y

Sys.Date()

log(100)

args(log)

?sum

example("sum")

c(1,2,3)

my_vector <- c("a","b")
my_vector
# creating a vector 
c(my_vector,"c")
my_vector

# my_vector er ikkje endra

sqrt(10)

pi <- 3.14159265359

round(pi,2)
?round

word = "HeLLo"

?tolower

tolower(word)

toupper(word)

word2 <- HeLLo
tolower(word2)
toupper(word2)

args(rnorm)

y <- rnorm(100,mean = 100, sd = 15)

mean(y)
sd(y)
head(y)
tail(y)

set.seed(123)  #ikkje bruk dette i data-analyse. er ikkje randomisert lengre
head(rnorm(n = 100, mean = 100, sd = 15))

matrix2 <- cbind(A = c(2,3,5),
                 B = c("I","am","here"),
                 C = c(2,5,5),
                 D = c(T,F,T))
matrix2


list123 <- list(A = c(2,3,5))

?c

x <- c(3,6,2,10)

mean(x)
sd(x)

mean(3,6,2,10) # bruker berre første verdien, mean skal ta ein vektor som argument
sd(3,6,2,10)

x <- c(3,6,2,NA)
mean(x,na.rm =TRUE)

?mean

x[1]
