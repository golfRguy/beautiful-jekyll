---
layout: post
title: The Hole Story
---


# Introduction

In this post I am going to create a word cloud of golf hole descriptions. In the future, this information could be used to extract some features that could be used to predict some golf outcomes (eg: score, driving accuracy). 

This post serves two purposes- First, it will contain technical details, like programming scripts, for educational/reproducibility purposes. Second, it won't be too dense that someone who isn't interested in the nuts and bolts won't be restricted from following along.



## But first, why?

Some reasons as to why this might be useful

* Increased prediction accuracy
    + With betting webistes like DraftKings, it may be lucrative to know whether certain golfers excel on courses whose holes contain certain textual descriptions. 
  
* Answer some inferential questions
    + Do left/right handed golfers prefer doglegs left, or doglegs right?
    + Does the presence of a fairway bunker or water hazard reduce driving accuracy?



## The data

Navigating to a pga tournament's [course web page](http://www.pgatour.com/tournaments/the-rsm-classic/course.html)  will provide you with a description of a golf hole:


```
## A difficult driving hole with marsh and bunker left and water on the right. The second shot to this green can be made very difficult depending on pin position as a bunker and water protects the right side and a peninsula green provides a difficult back pin placement.
```

The above description may contain some useful predictive information; it is a difficult driving hole, there is a water hazard, and the green is protected by water and bunker. We could scroll through each hole description jotting down notes, or we could write an R function to do this for us.


```r
library(jsonlite)
library(dplyr)

fetch_hole_data <- function(yr, tourn_id) {
 
  course_data <- data.frame()  #data frame to return
  url_seed <- "http://pgatour.com/data/r" 
  
    url_course_info <- paste(url_seed, "/", tourn_id, "/", yr, "/", "course.json"  ,sep="")
    
    if(!RCurl::url.exists(url_course_info)) stop ("The url does not exist")

    dat <- fromJSON(url_course_info) 
    l_course <- length(dat$courses$name) #multiple courses for some tournaments
    hole_data <- vector("list", l_course)
          
    for(j in 1:l_course) {  
      df <- dat$courses$holes[[j]]
      if (length(df) == 0) next
            
      df <-df %>% select(number, parValue, yards, body) %>% 
                  rename(hole_number = number, par = parValue) %>%
                  mutate(hole_number = as.numeric(hole_number), 
                        par = as.numeric(par), yards = as.numeric(yards))
            
     df$course_name <-rep(dat$courses$name[j], 18)
     df$year <- yr
     df$Permnum <-dat$permNum
     df$Coursenum <-dat$courses$number[j]
     hole_data[[j]] <- df
    }
    course_data <- do.call(plyr::rbind.fill, hole_data)
  
  return(course_data)
}
```





I looped this function over the years, 2014-2016, and extracted all unique holes with descriptions. This left us with 810 unique holes (or 45 courses worth of information). The data looks like this:


```r
library(knitr)
hole_data <- hole_data %>%  arrange(year, tourn_id, course_number, hole_number)
kable(head(hole_data))
```



 hole_number   par   yards  body                                                                                                                                                                                                                                                                                                                                                                                                                    course_name     year  tourn_id   course_number 
------------  ----  ------  ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  -------------  -----  ---------  --------------
           1     4     382  The fairway bunkers, short left and long right, will make the player choose to drive over the left bunker, or to lay up short of the right side bunker. The green is open to the left side and is guarded by a bunker just short of the green, on the right side.                                                                                                                                                       La Quinta CC    2014  002        202           
           2     4     434  This long and narrow par four requires a precise tee shot. There are large eucalyptus trees on the left and a fairway bunker on the right. The second shot must be a good one. The green is uphill and slightly elevated with deep bunkers on each side.                                                                                                                                                                La Quinta CC    2014  002        202           
           3     3     202  This picturesque par three features water and bunkers on the left. The proper club selection is required for good position on this two tiered green. If the green is missed on the right side there is a great opportunity to get up and down for par.                                                                                                                                                                  La Quinta CC    2014  002        202           
           4     4     384  The tee shot on four has to be long enough to carry the lake, but short enough to stay out of the aiming bunkers on the left side of the fairway. A slight fade is a good choice on this dogleg right, burt not too much, since the approach shot is easier from the left side of the fairway.                                                                                                                          La Quinta CC    2014  002        202           
           5     5     516  The first par five rewards a long drive aimed at the bunkers on the left side of the fairway. There is an opportunity to reach this green in two. However, there is only a small opening in front of the green where the ball can enter. For the player who wishes to lay up, the shot should favor the left side deep down the fairway. This will leave the player with a short pitch and a chance to made a birdie.   La Quinta CC    2014  002        202           
           6     5     527  The tee shot, on the second of back to back par fives, should favor the left side, just right of the fairway bunkers on the left. Since the fairway tilts to the right, there should be some distance added to the drive. The second shot should stop just short of the next set of bunkers, which will leave the player with a full wedge into the green.                                                              La Quinta CC    2014  002        202           

## Text mining

We can apply some basic text mining tools to this data set to search for potential features. For this, I used the [tm package](https://cran.r-project.org/web/packages/tm/index.html)  and [NLP package](https://cran.r-project.org/web/packages/NLP/NLP.pdf). Because spelling isn't consistent (eg. dog leg, dog-leg, dogleg) between descriptions, we do some data cleaning, and we put the descriptions in a corpus (set of text documents) for analysis.

In what follows, I transform the text to lowercase, remove punctuation, numbers, and excess whitespace. I also remove stop words. What are stopwords? They are essentially common words that we should filter out before begining our analysis, like- i, me, my, myself, we, our, ours, ourselves, you, your, etc... 





```r
#replace &nbsp with a space
hole_data$body <- gsub("&nbsp", " ", hole_data$body)
hole_data$body <- gsub("-", " ", hole_data$body) #remove dashes
hole_data$body <- gsub("dog leg", "dogleg", hole_data$body) #combine dog leg

#put the descriptions in a corpus (i.e. set of documents)
hole_corpus <- Corpus(VectorSource(hole_data$body))

# a function to clean the corpus (remove numbers, whitespace, etc...)
clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  return(corpus)
}

hole_corpus <- clean_corpus(hole_corpus)

#make a document term matrix
dtm <- DocumentTermMatrix(hole_corpus)

dtm_mat <- as.matrix(dtm)

#word frequencies
word_freqs <- colSums(dtm_mat)
word_freqs <- sort(word_freqs, decreasing = T)
word_names <- names(word_freqs)
```

## Hitting paydirt

### Wordcloud
Let's make a pretty little word cloud to visualize our text data. A wordcloud is a visual depiction of word frequencies. Larger words appear more frequently in our hole descriptions. 


```r
library(wordcloud)
library(RColorBrewer)

my_palette <- brewer.pal(n = 4,name = "BrBG")
n <- 50
set.seed(1225)
wordcloud(word_names[1:n], word_freqs[1:n], random.order = T, colors = my_palette)
```
{% include image.html url="/img/the_hole_story_files/unnamed-chunk-7-1.png" description="Golf hole description word cloud" %}

The most frequent word is green (presumably all holes have greens). However, we can already see some useful information. The words "bunker" and "bunkers" appear equally as frequent. Is it important to note that there are lots of bunkers on a hole, or one particular menacing bunker? Second, the words "left"" and "right" have almost identical frequencies. Perhaps these words can help us identify which way a dogleg bends, or where a water hazard is located. 

These individual words may prove useful, but I suspect the information they provide is too course, and we should instead examine some n-grams.

### n-grams

An n-gram is essentially a block of text made up of "n" words. For example, "Some call golf a sport", has the 2-grams (or bi-grams): "Some call", "call golf", "golf a", "a sport". Let's create a list of all of the bi-grams and make another wordcloud.


```r
#let's have a look at some bi-grams

# transform corpus back to one long string
hole_text_clean <- unlist(sapply(hole_corpus, `[`, "content"))

# split into words
hole_words <- strsplit(hole_text_clean, " ", fixed = T)
hole_words <- unlist(hole_words)
hole_words <- hole_words[hole_words != ""]
names(hole_words) <- NULL #remove the attributes

# get the bigrams
hole_bigrams = vapply(ngrams(hole_words, 2), paste, "", collapse = " ")

# count using xtabs
hole_bigrams_count <- as.data.frame(xtabs(~hole_bigrams)) %>% tbl_df() %>% arrange(desc(Freq))

my_palette2 <- brewer.pal(n = 6,name = "BrBG")

wordcloud(hole_bigrams_count$hole_bigrams, hole_bigrams_count$Freq, min.freq = 25,
          random.order = T, colors = my_palette2)
```

```
## Warning in wordcloud(hole_bigrams_count$hole_bigrams, hole_bigrams_count
## $Freq, : second shot could not be fit on page. It will not be plotted.
```

<div class="figure" style="text-align: center">
<img src="https://github.com/golfRguy/golfRguy.github.io/blob/master/img/the_hole_story_files/unnamed-chunk-7-1.png" alt="Golf hole bi-gram wordcloud"  />
<p class="caption">Golf hole bi-gram wordcloud</p>
</div>

The above shows us another level of information. We can see frequently appearing terms like, "dogleg left", "dogleg right", "fairway bunker", "green protected", and "must avoid" among other words. Perhaps we should also have a look at frequently appearing tri-grams:


```r
# get the trigrams
hole_trigrams = vapply(ngrams(hole_words, 3), paste, "", collapse = " ")

# count using xtabs
hole_trigrams_count <- as.data.frame(xtabs(~hole_trigrams)) %>% tbl_df() %>% arrange(desc(Freq))

my_palette3 <- brewer.pal(n = 2,name = "Dark2")

wordcloud(hole_trigrams_count$hole_trigrams, hole_trigrams_count$Freq, min.freq = 10,
          random.order = T, colors = my_palette3)
```

<div class="figure" style="text-align: center">
<img src="https://github.com/golfRguy/golfRguy.github.io/blob/master/img/the_hole_story_files/unnamed-chunk-8-1.png" alt="Golf hole tri-gram wordcloud"  />
<p class="caption">Golf hole tri-gram wordcloud</p>
</div>

This gives us yet another level of detail. Originally, we only had access to the word "dogleg". Then looking at the bi-grams, we had "dogleg right", and "dogleg left". Now we have even more detail with, "slight dogleg left", and "slight dogleg right". We also see the tri-grams "fairway bunkers left" and "fairway bunkers right", as well as the interesting "par good score". 

The point is that we now have access to different levels of detail when creating predictive features from these textual descriptions. 


In a future post I will see if any of the discoveries above prove useful in predicting  average score, driving distance, driving accuracy, green in regulation percentage, and average number of putts. 





