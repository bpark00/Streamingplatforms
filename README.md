# Streamingplatforms
##Overview

>- In today's society, streaming platforms have become a worldwide trend in mainstream media, where a good majority of people have owned at least one subscription in their day-to-day lives. Whether it is from Netflix, Hulu, Amazon Prime, or Disney+, there are a boatload of streaming platforms catered to every audience member's demographic and personal preferences. This project explores the thousands of movies and TV shows available across said platforms and how age demographics, specific actors, genres, and the actual amount of content comes into play using the particular coding techniques we have discussed throughout the course. Some techniques covered include, but are not limited to, hypothesis testing, tackling the dplyr package, and data visualization. 



##GOALS

>- The goal of our project is to discover which platforms provide content geared towards specific age groups or are content heavy in one specific genre, discover which platforms would be best fit for a person that prefers access to more movies or TV shows, and if the actors in movies leads to a better IMDb score. We felt that streaming platforms and movies play such a huge role in the lives of so many people in today's society and is very relatable and well understood by many. The questions that we answered in this project were ones that we specifically were interested in which was also a huge motivation for us since it was an enjoyable topic to look into, and we felt that it could help many others when trying to answer these same questions when deciding which streaming platform to purchase a subscription to.



##WHY

>- We felt this topic is a very current one that can affect most people especially around our age. When we were discussing what to research we realized that we have never taken the time to really dive in and make an informed decision when it came to having a streaming platform subscription. Rather we realized we were just buying so many different ones without really looking at the overlaps and dissimilarities of each platform and what we truly wanted out of a streaming service. Once we realized that we had never looked deeply into our subscriptions we realized so many others were out there just like us, so we chose to finally buckle down and find answers to questions that would help us make a more informed decision about our streaming subscriptions.

##OUTLINE

Questions we have answered:

1.Do the streaming platforms gear their content towards a certain age group more than others?

2.Does an "A list" actor always guarantee a higher rating on IMDb? Do some platforms have more content with "A list" actors?

3.Are movies and TV shows always improving its quality through the time? (See average rate from two platforms by year)

4.Do certain platforms contain more movies in specific genres than other genres?

5.Do certain streaming platforms have more movies vs tv shows or vice versa? 

##DATA

>- The following data was found on Kaggle and we were able to download the two .csv files directly to our computer and upload onto R studio. 
\newline https://www.kaggle.com/code/ruchi798/movies-and-tv-shows-eda/data <- Movie and TV Shows Data
\newline https://www.kaggle.com/datasets/wrandrall/imdb-new-dataset?resource=download <- IMDb Database
>- We found the data with information regarding actors from IMDb.
\newline https://www.imdb.com/list/ls058011111/ <- A-list actors

\newpage
## Loading Packages
```{r message=FALSE, echo = FALSE}
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(ggpubr)
```

## Getting Data
Description of data:
\newline Our primary dataset contains information regarding Movies and TV shows on four different streaming platforms.
\newline X: row number
\newline ID: Unique TV show ID
\newline Title: name of TV shows
\newline Year: the year in which the tv show was produced
\newline Age: target age group
\newline IMDb: IMDb rating
\newline Rotten.Tomatoes: Rotten Tomatoes rating
\newline Netflix: whether the tv show is found on Netflix
\newline Hulu: whether the tv show is found on Hulu
\newline Prime.Video: whether the tv show is found on Prime Video
\newline Disney.: Whether the tv show is found on Disney
\newline Type: Movie or TV show. Move is 0, TV show is 1.
\newline The IMDb database provided us with the information regarding the names of actors in each film and show and the genre each falls into, and the Top 1000 Actors dataset provided us with the information needed to tell whether a film or show contained any A-list actors.
```{r, warning=FALSE}
tv_show <- read.csv(file = "tv_shows.csv") %>%
  separate(IMDb, c("IMDb", "I-fullRate"), sep = "/") %>%
  separate(Rotten.Tomatoes, c("Rotten.Tomatoes", "R-fullRate"), sep = "/") %>%
  mutate(IMDb = as.numeric(IMDb)/10,
         Rotten.Tomatoes = as.numeric(Rotten.Tomatoes)/100) %>%
  select(c(-1,-2,-7,-9)) %>% mutate(MT = "TV")
head(tv_show)

movie <- read.csv(file = "MoviesOnStreamingPlatforms.csv") %>%
  separate(Rotten.Tomatoes, c("Rotten.Tomatoes", "R-fullRate"), sep = "/") %>%
  mutate(Rotten.Tomatoes = as.numeric(Rotten.Tomatoes)/100) %>%
  select(-1,-2,-7) %>% mutate(MT = "Movie")
head(movie)

imdb  <- read.csv("imdb_database.csv") %>%
  rename(Title=Movie.Name)
head(imdb)
Alist <- read.csv(file = "Top 1000 Actors and Actresses.csv") %>%
  select(Name, Known.For) %>% rename(Title=Known.For)
head(Alist)
```

## Cleanning Data
```{r, warning=FALSE, echo=FALSE}
# Basic Data
data <-
  bind_rows(tv_show, movie) %>%             #Combine two data sets
  rename(Disney=Disney.) %>%                 #Change the name of "Disney." to "Disney"
  mutate(Age = ifelse(Age=="", NA, Age)) %>% #Add NA to missing values in Age column
  pivot_longer(6:9, names_to = "Platform", values_to = "OnPlatform") %>% #Tidy data
  filter(OnPlatform == 1) %>%
  select(-9)

head(data)
```

\newpage

## Analysis
1. Do the streaming platforms gear their content towards a certain age group more than others?
```{r message=FALSE, echo=FALSE}
agedemo = data %>% group_by(Age, Platform) %>% summarise(n = n()) %>% mutate(prop = n/sum(n))

agedemo %>% filter(Age %in% c("13+", "16+", "18+", "all")) %>% ggplot(aes(x = Age, y=n, fill = Platform)) +
  geom_bar(position = "dodge", stat = 'identity') +
  scale_y_sqrt() +
  geom_text(
    aes(label = n, y = n + 0.05),
    position = position_dodge(0.9),
    vjust = 0,
    size = 7.5 / .pt
    ) + labs(title = "Streaming Platforms Content Based on Age Groups ", subtitle = "Do streaming platforms contain more content geared towards specific age groups?") + ylab("Number of Shows/Movies")+
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 5, color = "red")
        )
```

>- Disney has the most "family friendly" content with most of their content being for all ages or 13+, whereas there are very few content geared towards ages 16+ or 18+. 

>- Netflix has the most content geared towards individuals 18+, so it might not make as much sense for families with very young children to have a Netflix subscription vs a Disney subscription, however Netflix does have more content for younger ages than platforms Hulu and Prime Video. 

>- Prime Video has content mostly 18+ similar to Netflix, so for individuals that are older, or families with older children it would make more sense to own a Prime Video subscription, but there isn't as much content in this category as Netflix. 

>- Hulu has content mostly geared towards ages 16+ and 18+, so once again it would not make as much sense for families with young children to have a Hulu subscription as it would for these families to have a Disney subscription. Hulu has the least content for 13+ and all ages than all the other platforms. 


\newpage

2. Does an "A list" actor always guarantee a higher rating on IMDb?

```{r, warning=FALSE, message=FALSE, echo=FALSE}
library(stringr)
# Tidying up the data to get rid of the brackets and quotations on the Actor's names
imdbact <- imdb %>% select(Title, Actors, Score) %>% mutate(Actors = str_replace_all(imdb$Actors, c("\\[|\\]" = "", "'" = "")))

# combining the imdb, Alist actors, and TV shows/Movies databases into one table based off their Movie title
imdb_alist <- left_join(imdbact, Alist, by = "Title") %>% left_join(., data, by = "Title") %>% group_by(Title) %>% select(Title, Actors, Name, Score, MT, Platform) %>% filter(!is.na(MT), !is.na(Platform)) %>% mutate(is_Alist = ifelse(is.na(Name), FALSE, TRUE))


# Linear Regression 
 # For just movies
Alist_movie <- imdb_alist %>% filter(MT == "Movie")
Alist_movie
mod2 <- lm(is_Alist ~ Score, data = Alist_movie)
summary(mod2)$coefficients

qt(0.05/2, 6442, lower.tail=FALSE)

t.test(Alist_movie$Score, Alist_movie$is_Alist, alternative = "greater", conf.level = 0.95)

 # For just TV shows
Alist_TV <- imdb_alist %>% filter(MT == "TV")
Alist_TV
mod3 <- lm(is_Alist ~ Score, data = Alist_TV)
summary(mod3)$coefficients

qt(0.05/2, 4080, lower.tail = FALSE)

t.test(Alist_TV$Score, Alist_TV$is_Alist, alternative = "greater", conf.level = 0.95)

# table with the counts for each platform
with(imdb_alist, table(Platform))


```
Null Hypothesis: A movie with an A list actor rates the same as a movie/TV show without an A list actor.

Alt Hypothesis: A movie with an A list actor rates higher than a movie/TV show without an A list actor.

>- Based on the linear regression and comparing the t-score to the critical t-score, we can conclude that this is evidence that there is a statistical significance when a movie/TV show that contains an A-list actor generates more popularity than a movie/TV show without one.


\newpage

3. Are movies and TV shows always improving its quality through the time? (See average rate from two platforms by year)
```{r, message=FALSE, warning=FALSE, echo=FALSE}

##Rotten Tomato Scores for both Movies and TV Shows
ratings = data %>%
  group_by(Year, Rotten.Tomatoes) %>%
  arrange(Year) %>%
  na.omit()
p1 = ratings %>% ggplot(aes(x = Year, y = Rotten.Tomatoes)) +
  geom_point(size=0.5) +
  geom_smooth() + labs(title = "Rotten Tomatoes Scores of Movies and TV Shows Over Time", subtitle = "Looking at the Rotten Tomatoes scores specifically 
are we able to conclude that TV Shows and Movies have received higher ratings
as the years have passed and new media is being produced?") + ylab("Rotten Tomatoes Rating") +
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 5, color = "red")
        )
p1

###Rotten Tomato Scores for Movies Only 
ratingsmovie = movie %>%
  group_by(Year, Rotten.Tomatoes) %>%
  arrange(Year) %>% na.omit()
p2 = ratingsmovie %>% ggplot(aes(x = Year, y = Rotten.Tomatoes)) +
  geom_point(size=0.5) +
  geom_smooth() + labs(title = "Rotten Tomatoes Scores of Movies Over Time", subtitle = "Looking at the Rotten Tomatoes scores specifically
are we able to conclude that and Movies have received higher ratings 
as the years have passed and new media is being produced?") + ylab("Rotten Tomatoes Rating")+
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 5, color = "red")
        )
p2
###Average Rotten Tomato Scores for Movies
ratingsmovie2 = movie %>%
  group_by(Year) %>% summarise(Mean_Score = mean(Rotten.Tomatoes)) %>% arrange(Year) %>% na.omit()
p3 = ratingsmovie2 %>% ggplot(aes(x = Year, y = Mean_Score)) +
  geom_point(size=0.5) +
  geom_smooth() + labs(title = "Average Rotten Tomatoes Scores of Movies Over Time", subtitle = "Looking at the Rotten Tomatoes scores specifically
are we able to conclude that and Movies have received higher ratings
as the years have passed and new media is being produced?") + ylab("Rotten Tomatoes Rating")+
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 5, color = "red")
        )
p3
ggarrange(p2, p3)


###Ratings for TV Shows Only
ratingstv = tv_show %>%
  group_by(Year, IMDb, Rotten.Tomatoes) %>%
  arrange(Year) %>%
  na.omit()

p4 = ratingstv %>% ggplot(aes(x = Year, y = IMDb)) +
  geom_point(size=0.5) +
  geom_smooth() + labs(title = "IMDb Rating of TV Shows Over Time", subtitle = "Looking at the IMDb specifically
are we able to conclude that TV Shows have received higher ratings
as the years have passed and new media is being produced?") + ylab("IMDb Rating")+
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 5, color = "red")
        )

p5 = ratingstv %>% ggplot(aes(x = Year, y = Rotten.Tomatoes)) +
  geom_point(size=0.5) +
  geom_smooth() + labs(title = "Rotten Tomatoes Scores of TV Shows Over Time", subtitle = "Looking at the Rotten Tomatoes scores specifically 
are we able to conclude that TV Shows have received higher ratings 
as the years have passed and new media is being produced?") + ylab("Rotten Tomatoes Rating")+
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 5, color = "red")
        )

ggarrange(p4, p5)


###Average Ratings for TV Shows Only
ratingstv2 = tv_show %>%
  group_by(Year) %>% summarise(Mean_IMDb = mean(IMDb), Mean_Rotten.Tomato = mean(Rotten.Tomatoes)) %>%
  arrange(Year) %>%
  na.omit()

p6 = ratingstv2 %>% ggplot(aes(x = Year, y = Mean_IMDb)) +
  geom_point(size=0.5) +
  geom_smooth() + labs(title = "Average IMDb Scores of TV Shows Over Time", subtitle = "Looking at the average IMDb scores specifically
are we able to conclude that and TV shows have received higher ratings
as the years have passed and new media is being produced?") + ylab("IMDb Score")+
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 5, color = "red")
        )

p7 = ratingstv2 %>% ggplot(aes(x = Year, y = Mean_Rotten.Tomato)) +
  geom_point(size=0.5) +
  geom_smooth() + labs(title = "Average Rotten Tomato Scores of TV Shows Over Time", subtitle = "Looking at the average Rotten Tomato scores specifically
are we able to conclude that and TV shows have received higher ratings 
as the years have passed and new media is being produced?") + ylab("Rotten Tomato Score")+
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 5, color = "red")
        )

ggarrange(p6, p7)

ggarrange(p4, p5, p6, p7)
```

>- The dataset provides only the Rotten Tomato scores for movies, but both the IMDb scores and Rotten Tomato scores for TV Shows. 

>- When looking at the graphic comparing the years the movies and TV shows came out vs the Rotten Tomatoes rating we can see between a bit before 1950 through right before 2000 we see a slight increase in Rotten Tomato scores, but a decrease through the present. However, this is just a general trend as there are way too many movies and shows to be able to draw an absolute conclusion about ratings in this way.

>- The scores, especially through the 2000s, seem to be very spread out with some movies and shows receiving extremely high scores and some receiving extremely low scores. This definitely affects the way that the smoother gets placed in the graphic and makes it difficult to actually say that there was a definitive decrease in Rotten Tomato scoring over time. 

>- The graphic showing just the Rotten Tomato scores of movies still seems to be way too large of a sample to draw any definitive conclusions, but the graphic showing the average Rotten Tomato scores of movies in each year shows a bit more. We can see that over time the average scores of movies that came out fluctuated a bit in their scores. It does not seem that the average scores of movies has consistently increased or decreased over time. It looks as though from 1914 until right before 1950 the average Rotten Tomato score decreased, then increased until around 2000, and has since been in a decline. 

>- The dataset provided both the IMDb and Rotten Tomato scores for TV shows. Looking at the graphics showing the IMDb scores and Rotten Tomato scores of all TV shows we still cannot draw a direct conclusion about the trends of scores over time as the sample size is just too large. Looking at the graphics showing the average IMDb scores and average Rotten Tomato scores of TV shows there is no consistent trend of scores increasing or decreasing consistently. Both graphics look to increase and decrease within the same years which is interesting since the scoring methods used for IMDb and Rotten Tomato scores are different.

>- IMDb scores are based on users of IMDb submitting a score and a review of the movie, and Rotten Tomato scores are based on critics that are approved by the Rotten Tomato creators. Overall, it would seem IMDb scores are a bit more accurate as anyone can submit a review not jut a critic that has been approved.

>- Overall, it seems as though the average rating of scores for both movies and TV shows do not follow a specific pattern of increasing or decreasing, but rather there are some years where scores increase and some where they decrease.


\newpage

4. Do certain platforms contain more movies in specific genres than other genres?

```{r, warning = FALSE, message=FALSE, echo=FALSE}
genres <-
  data %>%
  left_join(imdb)  %>%
  select(Title, Movie.Type, Platform) %>%
  arrange(Movie.Type) %>%
  unique() %>%
  separate(Movie.Type, c("genres1", "genres2", "genres3"), sep = ",") %>%
  mutate(genres2 = str_sub(genres2, 2, -1),
         genres3 = str_sub(genres3, 2, -1)) %>%
  pivot_longer(2:4, names_to = "Genres", values_to = "genres") %>%
  filter(!is.na(genres)) %>%
  select(-Genres) %>%
  group_by(Platform, genres) %>%
  summarise(Number = n())

genres %>%
  ggplot(aes(x = reorder(genres, Number), y=Number)) +
  geom_bar(position = "dodge", stat = 'identity') +
  scale_y_sqrt() +
  geom_text(
    aes(label = Number, y = Number + 0.5),
    position = position_dodge(0.9),
    vjust = 0,
    size = 4 / .pt) +
  facet_wrap(~Platform) +
  labs(x = "Genres", y = "Number of Movies", title = "Breakdown of Movie Content for Platforms Based on Genre", subtitle = "Are we able to draw conclusions about the genres of movies each platform tends to give users access to?") +
  theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 5, color = "red")
        )
```


>- Disney has more family, adventure, animation movies. It is obvious that the kinds of movie on Disney's platform has a more evenly distributed than other platforms.

>- Netflix, Prime Video and Hulu all have more drama, comedy movies than any other kinds of movies.

>- As we can see different platforms have different genres represented more or less heavily through the movies that are available. However, we must take this with a grain of salt, since certain genres of movies may be more popular than others and some movies can fall into more than one genre. For example, we see comedy and drama are among the most highly shown genres on all platforms, but these two genres have more movies in them than genres like war or western overall, so it would make sense to see these genres more represented among the platforms since more movies are available in these two genres than all others.


\newpage

5. Do certain streaming platforms have more movies vs tv shows or vice versa?
```{r message=FALSE, echo=FALSE}
data %>% group_by(MT, Platform) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = Platform, y = n, fill = MT)) +
  geom_text(
    aes(label = n, y = n + 0.05),
    position = position_dodge(0.9),
    vjust = 0,
    size = 7.5 / .pt
  ) +
   geom_bar(position = "dodge", stat = 'identity')  + labs(title = "Amount of TV Shows vs Movies on Streaming Platforms", subtitle = "Do streaming platforms have more TV shows or Movies, or an equal amount of both?", fill = "Type") + ylab("Amount of Movies/TV shows")+
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 5, color = "red")
        )
```

>- Disney, Netflix, and Prime Video have more movies than TV shows, Hulu has more TV shows than movies.
