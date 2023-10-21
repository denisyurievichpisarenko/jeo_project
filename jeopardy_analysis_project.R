library(tidyverse)
library(stringr)
library(tidytext)
library(stopwords)
library(textstem)
library(ggrepel)

jeopardy <- read.csv('https://raw.githubusercontent.com/denisyurievichpisarenko/jeo_project/main/jeopardy_data.csv')
jeopardy %>%
  group_by(Round) %>%
  summarise(n())
jeopardy %>%
  filter(Round == "Jeopardy!") -> jeopardy

jeopardy%>%
  group_by(Category)%>%
  summarise(n())%>%
  rename('Count' = 'n()') -> jeo_themes

jeo_themes%>%
  slice_max(Count, n=20) -> popular_themes

jeopardy %>%
  right_join(popular_themes, by='Category') -> jeo

preprocess <- function(x){
  v <- c()
  x <- str_remove_all(x, "[><)(/,':;!?&.]")
  for(word in lemmatize_words(tolower(scan(text = x, what = '')))){
    if(!(word %in% stopwords("en"))){
      v <- append(v, word)
    }
  }
  return(v)
}

jeo$Lemma <- lapply(jeo$Question, preprocess)

bag <- c()
for(vec in jeo$Lemma){
  for(el in vec){
    bag <- append(bag, el)
  }
}

freqs <- as.data.frame(table(bag))
freqs$frequency <- freqs$Freq/length(bag)

sum_freq <- function(x){
  summa <- 0
  for(w in x[[1]]){
    #print(w)
    summa <- summa + freqs[freqs$bag == w, ]$frequency
    ratio <- summa/length(x[[1]])
    l <- log(ratio)
  }
  return(l)
}

jeo$Sum_freq <- as.numeric(lapply(jeo$Lemma, sum_freq))

#nrm <- function(x){(x-min(x))/(max(x)-min(x))}

#jeo[order(jeo$Sum_freq, decreasing = TRUE), ]

jeok <- jeo[jeo$Value %in% c('$100', '$200', '$300', '$400', '$500', '$600', '$800', '$1000'), ]

boxplot(Sum_freq~Value,
        data=jeok,
        #main="Different boxplots for each month",
        xlab="Value",
        ylab="Overall frequency of items",
        col="green",
        border="brown"
)

jeok%>%
  ggplot(aes(Sum_freq))+
  geom_histogram()+
  facet_wrap(~Category)+
  labs(x = "Value of question",
       y = "Medium frequency of a word in a question",
       title = "Frequency of words in a question dependent on its value",
       caption = "data from 
       https://www.reddit.com/r/datasets/comments/1uyd0t/200000_jeopardy_questions_in_a_json_file/")+
  theme_bw()

jeok%>%
  ggplot(aes(Value, Sum_freq)) +
  geom_violin(fill='purple')+
  labs(x = "Value of question",
       y = "Medium frequency of a word in a question",
       title = "Frequency of words in a question dependent on its value",
       caption = "data from 
       https://www.reddit.com/r/datasets/comments/1uyd0t/200000_jeopardy_questions_in_a_json_file/")+
  theme_bw()

find_capital <- function(x){
  xs <- scan(text = x, what = '')
  ans <- FALSE
  for(w in xs){
    if(substr(w, 1, 1) != tolower(substr(w, 1, 1))){
      ans <- TRUE
    }
  }
  return(ans)
}

jeok$IsProper <- lapply(jeok$Answer, find_capital)

jeok_true <- jeok[jeok$IsProper == TRUE, ]

jeok_true%>%
  group_by(Category, Value)%>%
  summarize(n())%>%
  rename('Count' = 'n()') -> jeol

jeok%>%
  group_by(Category, Value)%>%
  summarize(n())%>%
  rename('Count' = 'n()') -> jeol_gen

jeol_gen %>% left_join(jeol, by = c('Category', 'Value')) -> jeom
jeom[is.na(jeom)] <- 0
jeom$Ratio <- jeom$Count.y/jeom$Count.x


jeom%>%
  ggplot(aes(x = Value, y = Category, fill = Ratio))+
  geom_tile()+
  labs(x = "Value",
       y = "Category",
       title = "Number of proper names in dependence of value and category",
       caption = "data from 
       https://www.reddit.com/r/datasets/comments/1uyd0t/200000_jeopardy_questions_in_a_json_file/")

jeom %>% group_by(Category) %>% summarise(mean(Ratio)) %>% rename('Mean_ratio' = 'mean(Ratio)') -> jeon
jeom %>% group_by(Value) %>% summarise(mean(Ratio)) %>% rename('Mean_ratio' = 'mean(Ratio)') -> jeop

jeon%>%
  ggplot(aes(reorder(Category, Mean_ratio), Mean_ratio))+
  geom_col(fill = 'dark green')+
  coord_flip()+
  labs(x = "",
       y = "",
       title = "",
       caption = "data from 
       https://www.reddit.com/r/datasets/comments/1uyd0t/200000_jeopardy_questions_in_a_json_file/")+
  theme_bw() 

jeop%>%
  ggplot(aes(reorder(Value, Mean_ratio), Mean_ratio))+
  geom_col(fill = 'dark blue')+
  coord_flip()+
  labs(x = "",
       y = "",
       title = "",
       caption = "data from 
       https://www.reddit.com/r/datasets/comments/1uyd0t/200000_jeopardy_questions_in_a_json_file/")+
  theme_bw()

jeok_true%>%
  ggplot(aes(fct_rev(fct_infreq(Category)), fill=Value))+
  geom_bar()+
  coord_flip()+
  labs(x = "Category",
       title = "",
       subtitle = "data from 
       https://www.reddit.com/r/datasets/comments/1uyd0t/200000_jeopardy_questions_in_a_json_file/")

jeok %>% group_by(Category) %>% summarise(Med_freq = mean(Sum_freq)) -> jeor
jeos <- merge(jeon, jeor, by = 'Category')

jeos%>%
  ggplot(aes(Med_freq, Mean_ratio))+
  geom_point()+
  labs(x = "Average frequency of words in questions",
       y = "Ratio of proper names in answers",
       title = "Frequency of words and proper names in Jeopardy",
       caption = "data from 
       https://www.reddit.com/r/datasets/comments/1uyd0t/200000_jeopardy_questions_in_a_json_file/")+
  geom_text_repel(aes(label = Category), size = 3.4, max.overlaps = Inf)+ #size is choosen quite randomly: I tried to fit it but finally gave up :(
  theme_bw()
