countries <- character(5000)
for(i in seq_along(countries))
{
  for(j in 1:10)
  {
    found <- (countries[i] != "")
    if(!found)
    {
      # Create url string.
      url <-
        paste0(
          "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=",
          j,
          ";team=",
          i,
          ";template=results;type=batting;wrappertype=print"
          )
      raw <- try(xml2::read_html(url), silent=TRUE)
      if(class(raw) != "try-error")
        countries[i] <- substr(rvest::html_table(raw)[[1]][2,1], 14, 500)
    }
  }
  print(paste(i,countries[i]))
}



#1 England
#2 Australia
#3 South Africa
#4 West Indies
#5 New Zealand
#6 India
#7 Pakistan
#8 Sri Lanka
#9 Zimbabwe
#25 Bangladesh
#40 Afghanistan

#140 ICC World XI
