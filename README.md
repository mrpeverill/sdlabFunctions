# sdlabFunctions
This is the Stress and Development Function Library! To load the package:

1. library(devtools)
2. install_github("halpclub/sdlabFunctions")
3. library(sdlabFunctions)

A few guidelines for building new packages:

# Document your functions
Use roxygen2 to build documentation in to your functions. Here is how to do that: https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html (or you can look at the examples)

# Use one R file per function, or one per author.

# Remember Human Error
If you're using a function for the first time - remember to check that it does what it says it does.