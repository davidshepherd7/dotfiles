
## Set a default CRAN mirror, so it doesn't pester me every time.
options(repos=structure(c(CRAN="http://cran.ma.imperial.ac.uk")))


## Load history from the home directory on startup
.First <- function(){
   if(interactive()) try(utils::loadhistory("~/.Rhistory"))
}

## Save history in the home directory on exit
.Last <- function()
{
    print("bye")
    if(interactive()) try(savehistory("~/.Rhistory"))
}
