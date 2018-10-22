f <- factorial(round(2.0015) + 1)

s <- log10(1000) + log10(100)

foo <- round(3.1415) + 1
factorial(foo)

rm(f)
rm(s)
rm(foo)

vec <- c(1,2,3,10,100)
vec + 4
vec * 4
vec * vec
rm(vec)

mat <- matrix(c(1,2,3,4,5,6), nrow = 2)
t(mat)
rm(mat)

# Create three 2×2 matrices each with 2 rows and 2 columns.
array(c(1,2,3,4,5,6), dim = c(2,2,3))

# data types
class(0.0001)
class("hello")
class(TRUE)
class(T)

nchar("hello")

paste("hello", "world")

# factor
fac <- factor(c("a", "b", "c"))
fac
class(fac)
rm(fac)

vec <- c(1, "R", TRUE)
class(vec)

class(c(5, "two"))
class(c(TRUE, "a"))
class(c(1, "TRUE"))

foo <- as.numeric("1")
foo <- as.character(TRUE)
rm(foo)

# lists and data frames
lst <- list(1, "R", TRUE)
class(lst)
rm(lst)

list(c(1,2), TRUE, c("a", "b", "c"))

df <- data.frame(c(1, 2, 3), c("R", "S", "T"), c(TRUE, FALSE, TRUE))
class(df)
rm(df)

# names
nvec <- c(one = 1, two = 2, three = 3)
nvec
names(nvec)
names(nvec) <- c("uno", "dos", "tres")
nvec
rm(nvec)

ndf <- data.frame(numbers = c(1, 2, 3), 
                  letters = c("R", "S", "T"), 
                  logic = c(TRUE, FALSE, TRUE))
ndf
names(ndf)
rm(ndf)

beatles <- data.frame(name = c("John", "Paul", "George", "Ringo"),
                      birth = c(1940, 1941, 1943, 1940),
                      instrument = c("guitar", "bass", "guitar", "drums"))
beatles
beatles[c(2, 4), c(2, 3)]
beatles[c(2, 4), 3]
beatles[c(2:4), 2:3]
beatles[-c(2:4), -(2:3)]
beatles[1, ]
beatles[ , 2]
beatles[ , "birth"]
beatles[ , c("name", "birth")]
beatles[c(FALSE, TRUE, TRUE, FALSE), ]
beatles$birth

rm(beatles)

# integers (negative)
vec <- c(6, 1, 3, 6, 10, 5)
vec[-c(5, 6)]
vec[1:4]
vec[-(1:4)]
vec[c(3, 4, 5)]
# blank spaces return everything
vec[ ]
names(vec) <- c("a", "b", "c", "d", "e", "f")
vec[c("a", "b", "d")]
vec[c("a", "b", "f")]
vec[c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)]
rm(vec)

# logical comparisons
x <- c(1, 2, 3, 4, 5)
x > 3
x >= 3
x < 3
x <= 3
x == 3
x != 3
rm(x)

# %in%
1 %in% c(1, 2, 3, 4)
1 %in% c(2, 3, 4)
c(3, 4, 5, 6) %in% c(2, 3, 4)

# boolean operators
x <- 3
x > 2 & x < 9
x > 2 | x < 9
xor(x > 2, x > 9)
!(x < 2)
any(x < 2, x == 3, x > 4)
any(c(FALSE, FALSE, TRUE))
all(c(TRUE, TRUE, TRUE))

# logical subsetting
attach(airquality)
x <- airquality$Wind
x > 15
# return the values greater than 15
x[x > 15]
rm(x)

# missing values
5 + NA
mean(c(5, NA))
NA > 3
NA == 3
NA == NA

a <- c(1, NA)
a == NA
# NA NA
is.na(a)
# FALSE TRUE

# sum, length, sqrt
attach(airquality)
x <- airquality$Ozone
length(x)
# remove missing values
total <- sum(x, na.rm = TRUE)
sqrt(total)
rm(x)
rm(total)

# ozono - example
df <- attach(airquality)
ozono <- df$Ozone
length(ozono)

ozono_s <- ozono[!is.na(ozono)]
length(ozono_s)

hist(ozono_s)
median(ozono_s)

ozono_ns <- ozono[is.na(ozono)]
length(ozono_ns)

rm(df)
rm(ozono)
rm(ozono_s)
rm(ozono_ns)

