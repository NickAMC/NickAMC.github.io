Data 607 Assignment 2
================
Nick Climaco
2023-02-01

# Loading packages

``` r
# Loading packages
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(DBI)
library(RMySQL)
library(ggplot2)
```

## Intro:

Google Forms was used to collect the data from friends and family. There
are six recent popular movies for analysis: Puss in Boots: The Last
Wish, Black Panther, Avatar: The Way of Water, M3GAN, INFINITY POOL,
Everything Everywhere All At Once.

The rating scale is from 1-5, where 1 is the least liked and 5 is the
most liked. After collection time, data downloaded from Google Forms as
a .csv file then uploaded that .csv file to MySQL Online Database.

## Connecting to database and loading in the data

``` r
# connecting to online sql database
connect_db <- dbConnect(MySQL(), user = "sql9594950", password = rstudioapi::askForPassword("Database password"),
    host = "sql9.freesqldatabase.com", port = 3306, dbname = "sql9594950")

# Load the data from SQL database into R
data <- dbGetQuery(connect_db, "SELECT * FROM movie_reviews;")
head(data)
```

    ##                       COL 1     COL 2 COL 3                        COL 4
    ## 1                 Timestamp     Name:  Age: Puss In Boots: The Last Wish
    ## 2 2023/02/01 8:47:28 AM EST      John    23                            3
    ## 3 2023/02/01 8:47:50 AM EST     James    20                            3
    ## 4 2023/02/01 8:48:12 AM EST    Gareth    30                            4
    ## 5 2023/02/01 8:50:59 AM EST Christian    23                             
    ## 6 2023/02/01 8:51:23 AM EST   Jillian    19                            3
    ##           COL 5                    COL 6 COL 7         COL 8
    ## 1 Black Panther Avatar: The Way of Water M3GAN INFINITY POOL
    ## 2             3                        2                   5
    ## 3             5                              2             2
    ## 4             5                        5     1             1
    ## 5             5                        5     1             2
    ## 6             3                        5     1             3
    ##                               COL 9
    ## 1 Everything Everywhere All At Once
    ## 2                                 5
    ## 3                                 4
    ## 4                                 5
    ## 5                                  
    ## 6

``` r
# remove the first row and rename the columns
df <- data %>%
    slice(-1) %>%
    rename(Timestamp = "COL 1", Name = "COL 2", Age = "COL 3", Puss_in_Boots_The_Last_Wish = "COL 4",
        Black_Panther = "COL 5", Avatar_The_Way_of_Water = "COL 6", M3GAN = "COL 7",
        INFINITY_POOL = "COL 8", Everything_Everywhere_All_At_Once = "COL 9", )
head(df)
```

    ##                   Timestamp      Name Age Puss_in_Boots_The_Last_Wish
    ## 1 2023/02/01 8:47:28 AM EST      John  23                           3
    ## 2 2023/02/01 8:47:50 AM EST     James  20                           3
    ## 3 2023/02/01 8:48:12 AM EST    Gareth  30                           4
    ## 4 2023/02/01 8:50:59 AM EST Christian  23                            
    ## 5 2023/02/01 8:51:23 AM EST   Jillian  19                           3
    ## 6 2023/02/01 8:51:57 AM EST     Andre  15                           3
    ##   Black_Panther Avatar_The_Way_of_Water M3GAN INFINITY_POOL
    ## 1             3                       2                   5
    ## 2             5                             2             2
    ## 3             5                       5     1             1
    ## 4             5                       5     1             2
    ## 5             3                       5     1             3
    ## 6             3                       3     3             3
    ##   Everything_Everywhere_All_At_Once
    ## 1                                 5
    ## 2                                 4
    ## 3                                 5
    ## 4                                  
    ## 5                                  
    ## 6                                 3

``` r
# checking to see the data type of these columns
typeof(df$Age)
```

    ## [1] "character"

``` r
typeof(df$Black_Panther)
```

    ## [1] "character"

``` r
# movies rating are character type, changing it to a numeric
df <- df %>%
    mutate_at(vars(Age, Puss_in_Boots_The_Last_Wish, Black_Panther, Avatar_The_Way_of_Water,
        M3GAN, INFINITY_POOL, Everything_Everywhere_All_At_Once), as.integer)

head(df)
```

    ##                   Timestamp      Name Age Puss_in_Boots_The_Last_Wish
    ## 1 2023/02/01 8:47:28 AM EST      John  23                           3
    ## 2 2023/02/01 8:47:50 AM EST     James  20                           3
    ## 3 2023/02/01 8:48:12 AM EST    Gareth  30                           4
    ## 4 2023/02/01 8:50:59 AM EST Christian  23                          NA
    ## 5 2023/02/01 8:51:23 AM EST   Jillian  19                           3
    ## 6 2023/02/01 8:51:57 AM EST     Andre  15                           3
    ##   Black_Panther Avatar_The_Way_of_Water M3GAN INFINITY_POOL
    ## 1             3                       2    NA             5
    ## 2             5                      NA     2             2
    ## 3             5                       5     1             1
    ## 4             5                       5     1             2
    ## 5             3                       5     1             3
    ## 6             3                       3     3             3
    ##   Everything_Everywhere_All_At_Once
    ## 1                                 5
    ## 2                                 4
    ## 3                                 5
    ## 4                                NA
    ## 5                                NA
    ## 6                                 3

``` r
typeof(df$Age)
```

    ## [1] "integer"

``` r
typeof(df$Black_Panther)
```

    ## [1] "integer"

## Cleaning the data

Replacing the NA values for the median of each column. Removing the
timestamp column.

``` r
# clean the data by changing NA to the median of each column
df <- df %>%
    mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .)) %>%
    select(-Timestamp)
head(df)
```

    ##        Name Age Puss_in_Boots_The_Last_Wish Black_Panther
    ## 1      John  23                           3             3
    ## 2     James  20                           3             5
    ## 3    Gareth  30                           4             5
    ## 4 Christian  23                           3             5
    ## 5   Jillian  19                           3             3
    ## 6     Andre  15                           3             3
    ##   Avatar_The_Way_of_Water M3GAN INFINITY_POOL Everything_Everywhere_All_At_Once
    ## 1                       2     1             5                                 5
    ## 2                       5     2             2                                 4
    ## 3                       5     1             1                                 5
    ## 4                       5     1             2                                 5
    ## 5                       5     1             3                                 5
    ## 6                       3     3             3                                 3

## Analysis

Which of the six movies in the survey was the highest rated?

We are gonna use the sum of each movie rating and calculate the
percentage out of the highest rating the movie could get based on the
size of the survey. e.g. if there are n participants the max rating
would be 5 \* n.

``` r
max_rating = as.numeric(5 * count(df))
max_rating
```

    ## [1] 70

``` r
df_overall_rating <- df %>%
    select(-Name, -Age) %>%
    summarize_all(sum)
head(df_overall_rating)
```

    ##   Puss_in_Boots_The_Last_Wish Black_Panther Avatar_The_Way_of_Water M3GAN
    ## 1                          41            57                      59    22
    ##   INFINITY_POOL Everything_Everywhere_All_At_Once
    ## 1            41                                64

``` r
ratings_list <- unname(slice(df_overall_rating, 1)) %>%
    as.numeric()
df_3 <- data.frame(Movies = c(colnames(df_overall_rating)), Ratings = ratings_list)

df_3 <- df_3 %>%
    mutate(Score = round(Ratings/max_rating, 2) * 100) %>%
    arrange(desc(Ratings))
df_3
```

    ##                              Movies Ratings Score
    ## 1 Everything_Everywhere_All_At_Once      64    91
    ## 2           Avatar_The_Way_of_Water      59    84
    ## 3                     Black_Panther      57    81
    ## 4       Puss_in_Boots_The_Last_Wish      41    59
    ## 5                     INFINITY_POOL      41    59
    ## 6                             M3GAN      22    31

## Visualization

``` r
df_3 %>%
    ggplot(aes(x = Score, y = Movies, fill = Movies)) + geom_bar(stat = "identity") +
    ylab("Score") + geom_text(aes(label = Score), hjust = 1.5) + ggtitle("Movie Ratings") +
    theme(legend.position = "none", axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5))
```

![](Assignment-2---SQL-and-R_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Conclusion

The results of the survey conducted via Google Forms have indicated that
the movie “Everything Everywhere All at Once” was the most favored among
the movie choices presented. On the other hand, “M3GAN” was the least
liked, with lowest number of favorable scale.

``` r
# disconnect from the database
dbDisconnect(connect_db)
```

    ## [1] TRUE
