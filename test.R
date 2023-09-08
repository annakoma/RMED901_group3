matrix1 <- cbind(A= c(2,3,5),
                 B= c(7.5,4,9),
                 C= c(2,5,5),
                 D= 1:3)
matrix1
matrix1[2,]

df1 <- cbind.data.frame(A = c(2, 3, 5),
                        B = c(3.5, 4, 9),
                        C = c(2, 5, 5),
                        D = 1:3)
df1[["B"]]

list1 <- list(A = c(2, 3, 5),
               B = c("I", "am", "here"),
               C = c(2, 5, 5),
               D = c(T, F, T))

list1$A



?factor
args(factor)


for (i in 1:10){
  j <- i + 1
  print(j)
}

