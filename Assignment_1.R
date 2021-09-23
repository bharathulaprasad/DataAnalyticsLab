# Highest
{
  x <- as.integer(readline(prompt = "Enter first number :"))
  y <- as.integer(readline(prompt = "Enter second number :"))
  z <- as.integer(readline(prompt = "Enter third number :"))
  
  if (x > y && x > z) {
    print(paste("Greatest is :", x))
  } else if (y > z) {
    print(paste("Greatest is :", y))
  } else{
    print(paste("Greatest is :", z))
  }
  
}

{
  # Program make a simple calculator that can add, subtract, multiply and divide using functions
  add <- function(x, y) {
    return(x + y)
  }
  subtract <- function(x, y) {
    return(x - y)
  }
  multiply <- function(x, y) {
    return(x * y)
  }
  divide <- function(x, y) {
    return(x / y)
  }
  # take input from the user
  print("Select operation.")
  print("1.Add")
  print("2.Subtract")
  print("3.Multiply")
  print("4.Divide")
  choice = as.integer(readline(prompt = "Enter choice[1/2/3/4]: "))
  num1 = as.integer(readline(prompt = "Enter first number: "))
  num2 = as.integer(readline(prompt = "Enter second number: "))
  operator <- switch(choice, "+", "-", "*", "/")
  result <-
    switch(
      choice,
      add(num1, num2),
      subtract(num1, num2),
      multiply(num1, num2),
      divide(num1, num2)
    )
  print(paste(num1, operator, num2, "=", result))
}

#Highest of three numbers
{
  x <- as.integer(readline(prompt = "Enter first number :"))
  y <- as.integer(readline(prompt = "Enter second number :"))
  z <- as.integer(readline(prompt = "Enter third number :"))
  
  if (x > y && x > z) {
    print(paste("Greatest is :", x))
  } else if (y > z) {
    print(paste("Greatest is :", y))
  } else{
    print(paste("Greatest is :", z))
  }
  
}

# Multiplication table 1x10
# take input from the user

{
  num = as.integer(readline(prompt = "Enter a number: "))
  # use for loop to iterate 10 times
  for (i in 1:10) {
    print(paste(num, 'x', i, '=', num * i))
  }
}

#Sum of natural numbers
# take input from the user
{
  num = as.integer(readline(prompt = "Enter a number: "))
  if (num < 0) {
    print("Enter a positive number")
  } else {
    sum = 0
    # use while loop to iterate until zero
    while (num > 0) {
      sum = sum + num
      num = num - 1
    }
    print(paste("The sum is", sum))
  }
}

# sum of natural numbers using formula
#
# take input from the user
{
  num = as.integer(readline(prompt = "Enter a number: "))
  if (num < 0) {
    print("Enter a positive number")
  } else {
    sum = (num * (num + 1)) / 2
    
    print(paste("The sum is", sum))
  }
}
