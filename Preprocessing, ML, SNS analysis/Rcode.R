rm(list=ls())
forum <- read.csv(file.choose(), sep=";", stringsAsFactors = FALSE)
forum <- forum[,1:2]
install.packages("tau")
install.packages("tm")
install.packages("hunspell")
install.packages("SnowballC")
library("tau")
library("tm")
library("hunspell")
library("SnowballC")

#convert encoding
forum = lapply(forum, function(x) iconv(x, "WINDOWS-1252","UTF-8"))
Sys.setlocale('LC_ALL','C') 

#remove text between brackers
i=0
while(i<as.numeric(length(forum$posts))){
  forum$Posts[[i]] = gsub("\\s*\\([^\\)]+\\)","",forum$Posts[[i]])
  i=i+1
}

forum$Usernames = tolower(forum$Usernames)
forum$Usernames = removePunctuation(forum$Usernames)
forum$Posts = tolower(forum$Posts)
forum$Posts = removePunctuation(forum$Posts)

#split posts into individual words
i=1
splitPosts = list()
for(str in forum$Posts){splitPosts[[i]] = strsplit(str, " "); i=i+1}

#spelling correction posts
i=1
while(i<(as.numeric(length(splitPosts)))+1){
  item = splitPosts[[i]][[1]]
  j=1;
  while(j<(as.numeric(length(item)))+1){
    word=item[[j]]
    if(hunspell_check(word)==FALSE){
      if(identical(hunspell_suggest(word)[[1]], character(0))==FALSE)
      {
        splitPosts[[i]][[1]][[j]] = hunspell_suggest(word)[[1]][[1]]
      }
    }
    j=j+1;
  }
  i=i+1;
}

library(data.table)
lstDataTitle <- Map(as.data.frame, splitPosts)
forum$Posts <- rbindlist(lstDataTitle)

#remove stopwords and stem words
Usernames = Corpus(VectorSource(forum$Usernames))
Usernames <- tm_map(Usernames, removeWords, stopwords("english"))
Usernames <- tm_map(Usernames,stemDocument)
Posts = Corpus(VectorSource(forum$Posts))
Posts <- tm_map(Posts, removeWords, stopwords("english"))
Posts <- tm_map(Posts,stemDocument)

install.packages("e1071")
install.packages("tm")
install.packages("plyr")
library(plyr)
library(e1071)
library(tm)

#Create data.frame
train_Frame <- read.csv(file.choose(), sep=";", header=TRUE, stringsAsFactors = FALSE)

#Load SimpleCorpus into data.frame
#Loader <- function(dataframe) {
#  i=1
#  postinglist <- dataframe
#  totallength <- as.numeric(length(postinglist))
#  while(i < totallength)
#  {
#    train_Frame[i,1] <- postinglist[[i]][1]
#    i = i + 1
#  }
#  train_Frame[,1]
#}

train_Frame$Posts <- Loader(Posts)

#Trainingset and testset determine the first 500 messages which lead to a training and testset respectively.
#Totalset <- sample(1:length(Posts[[1]]),715)

#Result of first run is used:
Trainingset <- c(5122,14443,17403,14107,1054,22712,23676,20625,8309,12064,24308,19743,6237,26560,26481,11787,902,22727,16318,14561,19113,2824,20042,4321,20783,11952,1852,15710,17127,14210,27263,5045,20979,10647,10166,13266,2729,21455,7756,4181,20989,10447,18933,17847,21039,3453,25946,5498,20475,18458,25198,22507,7820,36,13106,2995,26027,21369,16024,12615,16842,22,26015,25964,21054,2721,15446,18276,23503,5102,3313,11091,7641,2868,10130,11679,7491,13547,11867,26790,26190,11506,11709,10241,9031,13590,7290,2587,1103920147,15521,6854,20177,27968,455,26034,3598,24204,26283,27256,22292,21165,28159,18684,9904,19210,24373,4373,3212,11735,9585,7019,14713,4035,4832,4091,26440,26807,24003,13411,8013,9482,16846,20970,7764,17578,5316,2883,4799,6704,11112,27687,25686,3145,20958,8300,11751,18770,5634,12273,23308,10126,243,2591,1905,5231,8197,22185,12834,2609,27860,11678,4239,8495,16172,8885,7763,12197,16880,2485,18015,5300,21284,12637,2749,15896,5523,23796,13309,3216, 18622,10520,23071,15562,19,25978,21568,11357,9385,11585,16405,25045,23290,11109,7007,26504,18721,24868,7548,25626,19281,27277,347,4360,24604,576,4233,6357,11134,27588,613,15296,27910,22822,2997,5025,15058,2709,7083,25393,19277,7291,24430,23650,24011,18913,8780,1537,23852,14010,8581,20916,15786,7406,13881,17168,12827,10486,17470,22491,2708,12952,5464,13485,13791,10100,11158,19814,16061,25583,18176,7747,757,19591,24477,8135,9254,21952,203,10223,11876, 3747,8783 ,1736,25259,20454,14676,23023,18375 ,9471,11979,17342,  422,14762,23876,24630,18398, 6753,27093,19516,11140, 7389,  244,19267,24745,16386,15462,17984,19762, 6161,10415,20006,27615,17663,14135, 9981,  331, 6205,21034, 1007, 9222,10594, 8507, 9676,16312,27596, 2794,14361,18853,13341,24352,15020,25358, 5459,17028,26006,23525,25677,23207,25569,22475,19913, 9956,24131,11344,10660, 6605,19845 ,6546, 4915, 5015,23291,8811,19787,10333,  846,17932 ,8026,11524,19192, 9896,21777,15235, 7317,23868, 1918,11259,18166,18629,16049,14389,23767,16414,17140,24743,12883,19592,25603,22097,21466,12941,16165,17858,11393,23473, 5208,22874,15373,19273, 6343,10019,22642,18671, 8327,20311, 5835, 7860,27817, 4140,10240,27209,24220, 16331, 1292,19828, 7807,25877,27756,25933, 1161,20730,20728,21829,11061,13215 ,7453,24366,15701,16276,  792,18932,  563,7059 ,7750 ,4212,14710,22519,25084,14910,13779,14683,13668, 4672,24329,14703, 2258,2891,25436, 5291, 5469, 2004,17233, 8032,16152, 5646,26737, 5915, 8352,16189,10559,2671, 8152 ,8694, 7728, 3914,26513,19612,18997,11574,13690, 4382, 4510, 4981, 1730,11942, 7327,19032,17468,23618, 7679, 2502,18831,24071,25382,14399,22851,13647,25014,6967,12733,23004,19344,12146,12373, 8509,23040, 1674,11468,12599, 5864, 4725,16142,18050 ,2592,  660, 1796,19523,24105,13131 ,2002,21462,  993,12011,28085 ,1165,13097,18394,13015,  724, 3861,10146,19831, 9357,11771,  336,10183,12749,25158,16443,11491,10906, 5354,15099,20548,12017,6204)
Testset <- c(17178,26684, 7898,24628,19465, 1636 ,2900 ,1818,16770,21163 , 955,11343,10341, 1256,13012,16687, 4605, 1465,27827, 5357,19996,25542,
             2962,24068,16560,25649,17052,26801,27438,11046,19376, 9047 , 460, 3271,25457,25459,14119,13018 ,3499,14384, 5925,24737,25070,20240,21549,23111,11761,27098, 9892, 5171,17323, 2093,24950, 6662,23278,19445,27161, 7109,16232,  156, 4843,14272, 8951,23106,21554,18726,14711,  193,27861,24240,12627,24319, 2331,24112,15646, 2395,12669,16327,16876,22395,21042,20206 ,8118,20503,10990, 9868,15086,16177,15762,  189,12453, 7392,11705,20179,26828,20317 ,6577, 4585,18609,4377, 9593,21765,16081,16222 ,7357,15005,10716,17515,10062 ,5008 ,3800,15794, 3391,23347 ,8806,12329,18309,17304,25753,10867,20624,13424,18782,18784 ,2680,18925, 5521, 5840, 5301,10950, 2088,15722, 1222,20431)

#Combine with scoring table
train_Scored <- na.omit(train_Frame)
ScoresTest <- read.csv(file.choose(), sep=";", header= TRUE, stringsAsFactors = FALSE)
ScoredTest <- na.omit(ScoresTest)

TrainedData0 <- subset(train_Scored, Score %in% 0)
TrainedData1 <- subset(train_Scored, Score %in% 1)
TrainedData2 <- subset(train_Scored, Score %in% 2)

TrainSplitted0 <- as.data.frame(matrix(unlist(strsplit(TrainedData0[[2]], " "))))
TrainSplitted1 <- as.data.frame(matrix(unlist(strsplit(TrainedData1[[2]], " "))))
TrainSplitted2 <- as.data.frame(matrix(unlist(strsplit(TrainedData2[[2]], " "))))

TrainSplitted0["Score"] <- 0
TrainSplitted1["Score"] <- 1
TrainSplitted2["Score"] <- 2

TrainSplitted <- rbind(TrainSplitted0, TrainSplitted1, TrainSplitted2)
names(TrainSplitted) <- c("Words", "Score")

TrainSplitted <- aggregate(Score~Words,data=TrainSplitted,FUN=mean)
TrainSplitted$Score <- round(TrainSplitted$Score)

TestedData0 <- subset(ScoredTest, Score %in% 0)
TestedData1 <- subset(ScoredTest, Score %in% 1)
TestedData2 <- subset(ScoredTest, Score %in% 2)

TestSplitted0 <- as.data.frame(matrix(unlist(strsplit(TestedData0[[2]], " "))))
TestSplitted1 <- as.data.frame(matrix(unlist(strsplit(TestedData1[[2]], " "))))
TestSplitted2 <- as.data.frame(matrix(unlist(strsplit(TestedData2[[2]], " "))))

TestSplitted0["Score"] <- 0
TestSplitted1["Score"] <- 1
TestSplitted2["Score"] <- 2

TestSplitted <- rbind(TestSplitted0, TestSplitted1, TestSplitted2)
names(TestSplitted) <- c("Words", "Score")

TestSplitted <- aggregate(Score~Words,data=TestSplitted,FUN=mean)
TestSplitted$Score <- round(TestSplitted$Score)

nb_model <- naiveBayes(as.factor(Score)~as.factor(Words),data = TrainSplitted)
nb_test_predicted <- predict(nb_model,TestSplitted[,-2])
mean(nb_test_predicted==TestSplitted$Score) #73% is correctly forecasted.

TotalSplitted <- as.data.frame(matrix(unlist(strsplit(train_Frame[[2]], " "))))
TotalSplitted[,2] <- NA
nb_test_predicted <- predict(nb_model,TotalSplitted[,-2])
TotalSplitted[,2] <- nb_test_predicted

Userscorer <- function(tobescoreddataframe, scoredwordsframe)
{
  i = 1
  j = 1
  while(i < length(tobescoreddataframe[[2]]))
  {
    wordlist <- strsplit(tobescoreddataframe[[2]][i], " ")
    while(!is.na(wordlist[[1]][j]))
    {
      location <- i
      if(tobescoreddataframe[[1]][i] == scoredwordsframe[[1]][i])
      {
        tobescoreddataframe[[2]][location] <- scoredwordsframe[[2]][i]
        j = j + 1
      }
    }
  }
  tobescoreddataframe <- tobescoreddataframe[[2]] > (4*(1/3))
  tobescoreddataframe[[1]]
}

Allusers <- Userscorer(ScoresTest, SplittedTotal)

install.packages("twitteR")
install.packages("httr")
library("twitteR")
library("httr")

consumer_key <- '3eaeFDhgz3TN67ZUTkMTDLrJy'
consumer_secret <- 'w6AQ8CzhaoOwPBtNpkzdK53EXNCifCRapcwUXsa3i3JuWKDuyG'
access_token <- '236867611-mOhzcMHLwjzIx6qVvvgJHmx65Ae3Dt2wRzlD0ujn'
access_secret <- 'nOyXF6s2NbeLe7nntRnMRhbXjz1tB1IiDzpW8zKrJNpd6'
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

listUsers <- lookupUsers(Allusers, includeNA=FALSE)

#Instagram
install.packages("instaR")
library("instaR")

full_url <- oauth_callback()
full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")
print(full_url)

app_name <- "socialwebproject"
client_id <- "92ffdb2d753a4f6e927a040f8f948276"
client_secret <- "89deec3b29c84f23ae80c9b417a41034"
scope = "public_content"

instagram <- oauth_endpoint(
  authorize = "https://api.instagram.com/oauth/authorize",
  access = "https://api.instagram.com/oauth/access_token")
myapp <- oauth_app(app_name, client_id, client_secret)

my_oauth <- instaOAuth(app_id="92ffdb2d753a4f6e927a040f8f948276", app_secret="89deec3b29c84f23ae80c9b417a41034")

ig_oauth <- oauth2.0_token(instagram, myapp,scope="basic",  type = "application/x-www-form-urlencoded",cache=FALSE)
tmp <- strsplit(toString(names(ig_oauth$credentials)), '"')
token <- tmp[[1]][30]

listUsers <- instaR::getUser(Allusers, token=my_oauth)

#Facebook

install.packages("Rfacebook")
install.packages("devtools")
library("devtools")
library("Rfacebook")
require (Rfacebook)

fb_oauth <- fbOAuth(app_id="424075978035807", app_secret="859aa88022d4166bc47673f8645ba95e",extended_permissions = TRUE)
save(fb_oauth, file="fb_oauth")

load("fb_oauth")

listUsers <- Rfacebook::getUsers(Allusers,token=fb_oauth)