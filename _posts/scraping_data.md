---  
layout: post
title: Scraping pga data
---
## Introduction

In order to begin analyzing some golf data, we are going to need... some golf data. Today I am going to explain how I obtain scorecard data from the PGA tour. 

## What is available?

The pga tour keeps its recent tournament data at [pgatour.com/data/r](pgatour.com/data/r). There are links for each tournament, and within each tournament there are sublinks for each year the tournament is held. Within a given year there are json and xml files containing tournament summaries, tee times, player scorecards, etc...

However, not every tournament folder contains every sub foulder. The following function loops over every tournament folder (and year) and creates an indicator variable for each piece of information that you may be interested in. Note that I pause the loop after every iteration for 2 seconds. It is good practice to not overburden a website's server.


```r
fetch_pga_data_info <- function() {
  require(XML)
  #data frame to store the information
  results <- data.frame()
  
  url_seed <- "http://www.pgatour.com/data/r/"
  
  get_links <- function() {
    url_seed <- "http://www.pgatour.com/data/r/"
    url_doc <- htmlParse(url_seed)
    url_links <- xpathSApply(url_doc, "//a/@href")
    links <- grep("\\d{3}", tourn_links, value = T)
    return(links)
  }
  
  url_links <  get_links()
  
  #loop over each tournament link
  for (i in 1:length(url_links)){
    url_doc <- htmlParse(paste(url_seed, url_links[i], sep = ""))
    tourn_links <- xpathSApply(url_doc, "//a/@href")
    tourn_years_links <- grep("\\d{4}", tourn_links, value = T)
    
    if (length(tourn_years_links) != 0){
    df <- matrix(nrow = length(tourn_years_links), ncol = 8)
    df[, 1] <- substr(url_links[i], 1, 3)
  
    for (j in 1:length(tourn_years_links)){
      df[j, 2] <- substr(tourn_years_links[j], 1, 4)
      
      doc_year <- htmlParse(paste(url_seed, url_links[i], tourn_years_links[j], sep = ""))
      
      # the links for a given year
      single_yr_links <- xpathSApply(doc_year, "//a/@href")
      
      df[j, 3] <- ifelse(sum(grepl("setup.json", single_yr_links)) == 1, 1, 0)
      df[j, 4] <- ifelse(sum(grepl("tournsum.json", single_yr_links)) == 1, 1, 0)
      df[j, 5] <- ifelse(sum(grepl("teetimes.json", single_yr_links)) == 1, 1, 0)
      df[j, 6] <- ifelse(sum(grepl("scorecards/", single_yr_links)) == 1, 1, 0)
      df[j, 7] <- ifelse(sum(grepl("course.json", single_yr_links)) == 1, 1, 0)
      df[j, 8] <- ifelse(sum(grepl("field.json", single_yr_links)) == 1, 1, 0)
      print(j)
      Sys.sleep(2)
      df[j,7] <- ifelse(sum(grepl("course.json", single_yr_links)) == 1, 1, 0)
    }
    results<-rbind(results,df)
    }
    
    
    print(url_links[i])
  }
  names(results)<-c("id","year", "tourn_setup", "tourn_sum", "tee_times", "scorecards", "course_info","field")
  results$year<-as.numeric(as.character(results$year))
  results$tourn_setup<-as.numeric(as.character(results$tourn_setup))
  results$tourn_sum<-as.numeric(as.character(results$tourn_sum))
  results$tee_times<-as.numeric(as.character(results$tee_times))
  results$scorecards<-as.numeric(as.character(results$scorecards))
  results$course_info<-as.numeric(as.character(results$course_info))
  return(results)
}
```

Running the above function will give you all of the tournament id's and years, and what is available for each year. Note: It takes quite a while to run. 

## The field data set

The following will extract the field (participating players) for a given tournament id and year. Valid tourn-id/year combos can be extracted from the fetch-pga-data_info function above (filtering for field == 1).


```r
fetch_field <- function(tourn_id, yr){
  require(jsonlite)
  
  url_seed <- "http://www.pgatour.com/data/r/"  

  url_field <- paste0(url_seed, tourn_id, "/", yr, "/", "field.json")
  dat <- fromJSON(url_field)
  field <- dat$Tournament$Players
  field$tournamen_name <- dat$Tournament$TournamentName
  field$tourn_id <- tourn_id
  field$year <- yr
  return(field) 
}
```


## The play by play data

The following function will extract play by play scorecard data from the pga tour website for a given tournament id and year. It loops over all of the players in the field (extracted using the function above) and over each round. 

The data is somewhat inconsistent from tournament to tournament. Not all tournaments have play by play data, or they only contain certain variables. Sometimes the variables are listed in the json data, but are empty, sometimes they aren't even listed. Some tournaments also have multiple courses.

The function returns a list. The first entry in the list is the full play by play data should it be available. The second item in the list is a simplified hole by hole report for each player.


```r
fetch_pbp <- function(tourn_id, yr){
  require(jsonlite)
  
 url_seed<-"http://www.pgatour.com/data/r/"  

  # create an empty data to store the results
  df <- data.frame()
  round_info_all <- data.frame()

  # retrieve the field for the tournament
  
  # loop over each player
  field <- fetch_field(tourn_id, yr)
    for (i in 1:nrow(field)){
  
      url_scorecard <- paste0(url_seed, tourn_id, "/", yr, "/", "scorecards/", 
                              field$TournamentPlayerId[i], ".json")
  
      # check if the url exists for that player (some players are in the field but withdraw)
      if (!RCurl::url.exists(url_scorecard)) next
  
      dat <- fromJSON(url_scorecard, simplifyDataFrame = TRUE)
  
      if (length(dat$p$rnds$n)==0) next
  
  # loop over the number of rounds
      for (j in 1:length(dat$p$rnds$n)){ 
    
        if (length(dat$p$rnds$holes[[1]]) == 0) next
        round_info <- dat$p$rnds$holes[[j]][,1:5]
        round_info <- round_info %>% rename(hole_number = cNum, 
                                            score = sc, 
                                            par_for_day = pDay, 
                                            par_for_tourn = pTot) %>% 
                                     mutate(hole_number = as.numeric(hole_number),
                                            score = as.numeric(score),
                                            par_for_day = as.numeric(par_for_day, 
                                            par_for_tourn = as.numeric(par_for_tourn)))
        # add additional variables
        round_info$round_number <- j
        round_info$pid <- field$TournamentPlayerId[i]
    
        p_name <- strsplit(field$PlayerName[i], ", ")[[1]]
        round_info$player_name <- paste(p_name[2], p_name[1])
    
        round_info$tourn_id <- tourn_id
        round_info_all <- rbind(round_info_all, round_info)
    
    
        if (length(dat$p$rnds$holes[[j]]$shots) == 0 | 
            length(dat$p$rnds$holes[[j]]$shots[[1]]) == 1 |
            length(dat$p$rnds$holes[[j]]$shots[[1]]) == 0) next 
    
        shots <-do.call(rbind, dat$p$rnds$holes[[j]]$shots)

        shots_length <- sapply(dat$p$rnds$holes[[j]]$shots, nrow)
        
        shots <- shots %>% mutate(round_hole_num = as.numeric(n), putt = as.numeric(putt), 
                                  dist = as.numeric(dist), x = as.numeric(x),
                                  y = as.numeric(y), z = as.numeric(z)) %>% 
                           rename(shot_num = n, putt_num = putt, 
                                  distance = dist, x_loc = x, y_loc = y, z_loc = z,
                                  type = t, dist_left = left, tee_shot = tee)
        
        shots$hole_number <- rep(round_info$hole_number, times = shots_length)
        shots <- shots %>% select(-pid)
    
        pbp <- shots %>% left_join(round_info, by = c("hole_number"))

        df <- rbind(df, pbp)
      }
  
    Sys.sleep(2) # pause system for 2 seconds
  }

  if (dim(df)[1] == 0) return(list(NULL, tbl_df(round_info_all)))
  
  df$year <- yr
  df$tournamen_name <- field$tournamen_name[1]
  
  # missing putts to zero
  df$putt_num[is.na(df$putt_num)] <- 0
  
  # reorder the variables
  df <- df %>%  select(pid, player_name, tourn_id, tournamen_name,
                       round_number, hole_number, round_hole_num, shot_num,
                       putt_num, type, prv, tee_shot, cup, from, to, asc,
                       distance, dist_left, x_loc, y_loc, z_loc, club, con, shotText,
                       score, par_for_day, par_for_tourn) %>% rename(shot_text = shotText)
  
  return(list(tbl_df(df), tbl_df(round_info_all)))
}
```


## Scraping in action

Let's use the above play by play function to grab the scorecards from a recent PGA tour tournament; The Quicken Loans National (tourn_id = "471").


```r
pbp <- fetch_pbp(tourn_id = "471", yr = 2016)

pbp_data <- pbp[[1]]


# variable names
names(pbp_data)
```

```
##  [1] "pid"            "player_name"    "tourn_id"       "tournamen_name"
##  [5] "round_number"   "hole_number"    "round_hole_num" "shot_num"      
##  [9] "putt_num"       "type"           "prv"            "tee_shot"      
## [13] "cup"            "from"           "to"             "asc"           
## [17] "distance"       "dist_left"      "x_loc"          "y_loc"         
## [21] "z_loc"          "club"           "con"            "shot_text"     
## [25] "score"          "par_for_day"    "par_for_tourn"
```

```r
# first 10 observations
pbp_data
```

```
## # A tibble: 28,005 x 27
##      pid    player_name tourn_id         tournamen_name round_number
## *  <chr>          <chr>    <chr>                  <chr>        <int>
## 1  29745 Tyler Aldridge      471 Quicken Loans National            1
## 2  29745 Tyler Aldridge      471 Quicken Loans National            1
## 3  29745 Tyler Aldridge      471 Quicken Loans National            1
## 4  29745 Tyler Aldridge      471 Quicken Loans National            1
## 5  29745 Tyler Aldridge      471 Quicken Loans National            1
## 6  29745 Tyler Aldridge      471 Quicken Loans National            1
## 7  29745 Tyler Aldridge      471 Quicken Loans National            1
## 8  29745 Tyler Aldridge      471 Quicken Loans National            1
## 9  29745 Tyler Aldridge      471 Quicken Loans National            1
## 10 29745 Tyler Aldridge      471 Quicken Loans National            1
## # ... with 27,995 more rows, and 22 more variables: hole_number <dbl>,
## #   round_hole_num <dbl>, shot_num <chr>, putt_num <dbl>, type <chr>,
## #   prv <chr>, tee_shot <chr>, cup <chr>, from <chr>, to <chr>, asc <chr>,
## #   distance <dbl>, dist_left <chr>, x_loc <dbl>, y_loc <dbl>,
## #   z_loc <dbl>, club <chr>, con <chr>, shot_text <chr>, score <dbl>,
## #   par_for_day <dbl>, par_for_tourn <chr>
```

We now have some very detailed data that we can begin to analyze.

