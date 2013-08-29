Latest.End.Date <- function()
{
  
    if (exists(as.character(substitute(calendar)))) 
    {calendar <- calendar}
    
    else 
    {calendar        <- read.csv("//ny-filer1/common_pbr/risk/dan cashion/r/Rcalendar.csv"
                                 ,sep=",",header=T,, colClasses="character")
    } 
    
    latestDate      <- as.character(Sys.Date())
    prevBizDate.row <- which(calendar$calendar_date %in% latestDate)
    end.date        <- as.Date(calendar[prevBizDate.row,4],format="%Y-%m-%d")
    return(end.date)
}
