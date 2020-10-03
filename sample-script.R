library(lfe)

x1 <- runif(1000,0,5)
x2 <- factor( sample.int(3,size=1000,replace=TRUE) )
x3 <- runif(1000,3,7)
x4 <- factor( sample.int(5, size = 1000, replace = TRUE) )
bc34 <- runif(1000, 0, 1)
bc34_na_fl <- (runif(1000, 0, 1) > 0.5)
y <- 5*x1 + -2*(x2==2) + 0.5*x3 + rnorm(1000)
myResult1 <- felm(y ~ x1 + x2 + x3 + bc34_na_fl + bc34)

x2[x2==2] <- 1
myResult2 <- felm(y ~ x1 + x2 + as.factor(x4) + x1*x3)

myResult3 <- lm(y ~ x1 + x2 + x2*x3)

myResult4 <- felm(y ~ x1 | x4)

temp <- data.frame(y, x1, x2, x3, x4)

col1 <- make_column(myResult1)
col2 <- make_column(myResult2)
col3 <- make_column(myResult3)
col4 <- make_column(myResult4)

tabler(col1) %>% print.latex()
tblr_obj <- tabler(col1,col2, col3, col4)
print.latex(temp, rename=c('x2'='income'))
