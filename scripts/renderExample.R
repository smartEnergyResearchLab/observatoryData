# Example using render to save a report to /docs

library(here)
library(rmarkdown)

# Params ----
rmdFile <- "renderExample" # Ellen: change this to your Rmd file name (no suffix)

# for Rmd ----
title <- "render Example" # Ellen: change this
subtitle <- "test" # Ellen: change this
authors <- "Ben Anderson" # Ellen: change this

# Functions ----
makeReport <- function(f){
  # f = the rmd file to render
  # default output = whatever the Rmd specifies
  rmarkdown::render(input = paste0(here::here("scripts", f),".Rmd"), # we love here:here() - it helps us find the .Rmd to use
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors),
                    output_file = paste0(here::here("docs", f)) # where the output goes
  )
}

# Set up ----
startTime <- proc.time()

# build and save ----
makeReport(rmdFile)

t <- proc.time() - startTime # how long did it take?
elapsed <- t[[3]]

print("Done")
print(paste0("Completed in ", round(elapsed/60,3), " minutes using ",
             R.version.string, " running on ", R.version$platform))
