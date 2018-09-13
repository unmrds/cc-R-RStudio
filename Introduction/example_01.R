# Do basic math - turn your computer into the most expensive pocket calculator you have ever used
2+3
23*(2^26 + 1.36)/sqrt(23e123)

# Create a vector (collection) of values to work with
# you can assign values using either the "<-" symbol or a "=". 
# The preferred method is using the "<-" for clarity
x <- c(15,18,6,56,15,28,35)
x

# Do some math with the vector
y <- x^2
y

# Calculate some statistics
mean(x)
mean(y)

var(x)
var(y)

summary(x)
summary(y)

fivenum(x)
fivenum(y)

# Generate some plots
plot(x,y)          # basic scatterplot

plot(x,y)          # same scatterplot with an added regression line for the paired x-y values 
abline(lm(y~x), col="red")

# Do some linear modeling
my_lm <- lm(y ~ x) # simple linear regression model of the form "y = B0 + (B1 * x)"
print(my_lm)       # print out the coefficients for the model
summary(my_lm)     # compute and print the more detailed statistics for the model
plot(my_lm)

