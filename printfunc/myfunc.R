myfunc <- function(x, y) {
  # Here's a function with a comment and custom spacing in the source code
  switch(x, 
    one = 1,
    two = if (y) {
            3
          } else {
            2
          }
  )
}
