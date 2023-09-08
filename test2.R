adding_to_5_more <- function(x, y){
  if(x <= 5){
    return(x)
  }
  return(x + y)
}

adding_to_5_more(1)


check_input <- function(
    argument1, # no default: this argument is required!
    argument2 = 2,
    argument3 = 'a'
){
  if(argument1 > 5){
    warning("Argument1 should be <= 5, assuming 5.")
    argument1 <- 5
  }
  if(!is.numeric(argument2)){
    stop("Argument2 should be numeric!")
  }
  if(is.character(argument3)){
    message("Well done!")
  }
  output <- list(argument1, argument2, argument3)
  return(output)
}

check_input(1)
check_input(6)
check_input(6, 'a')
check_input(6, argument3 = 'b')


a <- 1
my_fun <- function(a){
  d <- 2
  a <- d + a
  return(a)
}

a <- my_fun(a)
a
