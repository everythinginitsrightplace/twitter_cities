library(tidyverse)
library(rtweet)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(gganimate)
#devtools::install_github("dgrtwo/gganimate")
library(rlang)
library(ggraph)
library(igraph)
library(hrbrthemes)
library(ggalt)
library(ggthemes)
library(scales)
scales::show_col(ipsum_pal()(9))
twitter_token <- create_token(app = "kazan_twitter_behaviour_app", consumer_key = "EqkvWPpv0NOpCBd1MqHiqvaBc", consumer_secret = "aIY7qQVFlzGCfF0B267S3UqatjvXM2wVowFaBGuKxA8xtih21Z")
# tweets containing #WorldCupRussia2018
WorldTwitter <- search_tweets("#Samara")
KazanTwitter <- search_tweets("#Kazan")
DubNtnStrngthNmbrs <- rtweet::search_tweets2(
  c("\"#казань\"",
    "#самара", "#нижнийновгород", "#волгоград", "#екатеринбург", "#ростовнадону"),
  n = 50000, retryonratelimit = TRUE)

DubNtnStrngthNmbrs %>% dplyr::glimpse(78)
DubNtnStrngthNmbrs %>% count(query)
ggplot(DubNtnStrngthNmbrs, aes(query)) + geom_histogram(stat = "count", aes(fill = query)) +
  labs(title="Распределение городов в собранной базе твитов", x="Города", y="Число")+
  scale_fill_manual(name = "Хештеги городов" , values = c('"#казань"' = "red", "#волгоград" = "green", "#екатеринбург" = "orange", "#нижнийновгород" = "purple", "#ростовнадону" = "yellow", "#самара" = "blue"))+
  theme_modern_rc()

  
  
  
  
  
  
  
  
tibble::as_tibble(base::intersect(x = base::names(DubNtnStrngthNmbrs),
                                  y = base::names(rtweet::users_data(DubNtnStrngthNmbrs))))
# get user data
UsersDubNtnStrngthNmbrs <- rtweet::users_data(DubNtnStrngthNmbrs)
UsersDubNtnStrngthNmbrs %>% glimpse(78)
tibble::as_tibble(
  intersect(x = base::names(DubNtnStrngthNmbrs),
            y = base::names(rtweet::tweets_data(DubNtnStrngthNmbrs)))) %>%
  utils::head(20)
DubNtnStrngthNmbrs %>%
  dplyr::select(text) %>%
  utils::head(10)



gg_ts_plot <- DubNtnStrngthNmbrs %>%
  rtweet::ts_plot(., by = "15 minutes") +
  ggthemes::theme_gdocs() +
  ggplot2::theme(plot.title =
                   ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = "Дни",
    y = "Частота публикации твитов",
    title = "Твиты #казань, #волгоград, #екатеринбург, #нижнийновгород, #ростовнадону, #самара",
    caption = "\nДанные собраны при помощи API Twitter и пакета rtweet")+
  geom_line(color = "pink")+
  theme_modern_rc(base_family = "Roboto Condensed", base_size = 10,plot_title_family = "Roboto Condensed", plot_title_size = 15, axis_title_family = "Roboto Condensed", axis_title_size = 15, axis_title_face = "plain", axis_title_just = "rt")
gg_ts_plot


DubNtnStrngthNmbrsLoc <- rtweet::lat_lng(DubNtnStrngthNmbrs)
DubNtnStrngthNmbrsLoc %>% names() %>% tail(2)
DubNtnStrngthNmbrsLoc %>% dplyr::distinct(lng) %>% base::nrow()
DubNtnStrngthNmbrsLoc %>% dplyr::distinct(lat) %>% base::nrow()
DubNtnStrngthNmbrsLoc <- DubNtnStrngthNmbrsLoc %>% dplyr::rename(long = lng)
World <- ggplot2::map_data("world")
World %>% glimpse(78)


ggWorldMap <- ggplot2::ggplot() +
  ggplot2::geom_polygon(data = World,
                        aes(x = long,
                            y = lat,
                            group = group),
                        fill = "black",
                        color = "white",
                        alpha = 0.6)
ggWorldMap +
  ggplot2::theme_void() +
  ggplot2::labs(title = "Basic World Map (geom_polygon)")
ggsave(filename = "ggWorldMap.png",
       width = 6.5,
       height = 4,
       units = "in")




gg_Merc_title <- "Твиты с хештегами #казань, #волгоград, #нижнийновгород, #самара, #екатеринбург, #ростовнадону, 17 июня - 26 июня 2018 года"
gg_Merc_cap <- "Tвиты собраны при помощи пакета rtweet"
gg_mercator_dubstrngth <- ggWorldMap +
  ggplot2::coord_quickmap() +
  ggplot2::geom_point(data = DubNtnStrngthNmbrsLoc,
                      aes(x = long, y = lat),
                      size = 2, # reduce size of points
                      color = "orange") +
  # add titles/labels
  ggplot2::labs(title = gg_Merc_title,
                caption = gg_Merc_cap) +   
  ggplot2::theme_void() 
gg_mercator_dubstrngth

ggplot2::ggsave(filename = "gg_mercator_dubstrngth.png",
                width = 14.4,
                height = 7.43,
                units = "in")



#################################################
#################################################
#################################################
#################################################
#################################################
# convert query to factor (you'll see why later)
DubNtnStrngthNmbrsLoc$query <- factor(DubNtnStrngthNmbrsLoc$query,
                                      labels = c("#казань", "#волгоград", "#екатеринбург", "#нижнийновгород", "#ростовнадону", "#самара"))
# define titles
ggDubWT_title <- 'Твиты по интересующим нас хештегам #казань", "#волгоград", "#екатеринбург", "#нижнийновгород", "#ростовнадону", "#самара"'
ggDubWT_cap <- "Данные собраны при помощи пакета rtweet  "

#  create world map

ggWorld2 <- ggplot2::ggplot() +
  ggplot2::geom_map(data = World, map = World,
                    aes(x = long,
                        y = lat,
                        map_id = region),
                    size = 0.009,
                    fill = "grey82",
                    alpha = 0.4)
ggDubWinkelTrip <- ggWorld2 +
  ggplot2::geom_point(data = DubNtnStrngthNmbrsLoc,
                      aes(x = long,
                          y = lat),
                      color = "firebrick",
                      size = 0.4) +
  # add Winkel tripel layer
  ggalt::coord_proj("+proj=wintri") +       
  ggplot2::labs(
    title = ggDubWT_title,
    caption = ggDubWT_cap)
#  add the twiiter data layer

ggDubWinkelTrip
###################################################
###################################################
###################################################
###################################################


DubNtnStrngthNmbrsLoc %>%
  # identify observations with complete location information
  dplyr::filter(!is.na(long) |
                  !is.na(lat)) %>%
  # get the sorted count
  dplyr::select(followers_count, screen_name) %>%
  # arrange these descending
  dplyr::arrange(desc(followers_count)) %>% head(10)



DubNtnStrngthNmbrsLoc %>%
  # remove observations without location information
  dplyr::filter(!is.na(long) |
                  !is.na(lat)) %>%
  # arrange data
  dplyr::arrange(desc(followers_count)) %>%
  # remove followers with more than 10,000 followers
  dplyr::filter(followers_count < 10000) %>%
  ggplot2::qplot(followers_count,
                 data = .,
                 geom = "freqpoly",
                 color = "white", size = 5) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(title = "Количество подписчиков аккаунтов, использующих интересующие нас хештеги",
    caption = ggDubWT_cap, x = "Количество подписчиков", y = "Аккаунты")+ 
  guides(color = FALSE,
         size = FALSE)+
  theme_modern_rc(base_family = "Roboto Condensed", base_size = 10,plot_title_family = "Roboto Condensed", plot_title_size = 15, axis_title_family = "Roboto Condensed", axis_title_size = 15, axis_title_face = "plain", axis_title_just = "rt")



#################################
#################################
####################################
##################################
###################################

DubAnimateData <- DubNtnStrngthNmbrsLoc %>%
  # remove observations without location information
  dplyr::filter(!is.na(long) |
                  !is.na(lat)) %>%
  # arrange data descending
  dplyr::arrange(desc(followers_count)) %>%
  # remove the follower_count that are above 10,000
  dplyr::filter(followers_count < 10000) %>%
  # select only the variables we will be visualizing
  dplyr::select(user_id,
                status_id,
                screen_name,
                followers_count,
                friends_count,
                favourites_count,
                created_at,
                text,
                long,
                hashtags,
                lat)
DubAnimateData %>% glimpse(78)

ggWorld2 +
  geom_point(aes(x = long,
                 y = lat,
                 size = followers_count),
             data = DubAnimateData,
             color = "royalblue", alpha = .2) +
  ggplot2::scale_size_continuous(range = c(1, 6),
                                 breaks = c(500, 1000, 2000,
                                            4000, 6000, 8000)) +
  labs(size = "Followers") +
  ggalt::coord_proj("+proj=wintri") +
  ggthemes::theme_hc() +
  ggplot2::theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()) +
  ggplot2::theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) +
  ggplot2::theme(plot.title = ggplot2::element_text(
    face = "bold", size = 12)) +
  ggplot2::labs(title = "#DubNation & #StrengthInNumbers",
                subtitle = "Tweets and followers")
#################################################
###################################################
##############################################
##################################################
###############################################

library(tibble)
library(lubridate)
# min(DubAnimateData$created_at) # "2018-06-09 07:43:46 UTC"
# max(DubAnimateData$created_at) # "2018-06-10 02:36:31 UTC"
# create data frame foe the beginning of the animation
EmptyAnimateDataBegin <- tibble(
  created_at = as_datetime("2018-06-09 07:43:46 UTC"),
  followers_count = 0,
  long = 0,
  lat = 0)
EmptyAnimateDataBegin


# create data frame for the end of the animation
EmptyAnimateDataEnd <- tibble(
  created_at = seq(as_datetime("2018-06-26 03:00:00 UTC"),
                   as_datetime("2018-06-26 04:00:00 UTC"),
                   by = "min"),
  followers_count = 0,
  long = 0,
  lat = 0)
EmptyAnimateDataEnd



DubMap <- ggWorld2 +
  geom_point(aes(x = long,
                 y = lat,
                 size = followers_count,
                 frame = created_at,
                 cumulative = TRUE),
             data = DubAnimateData,
             color = "royalblue",
             alpha = .2) +
  # transparent frame 1
  geom_point(aes(x = long,
                 y = lat,
                 size = followers_count,
                 frame = created_at,
                 cumulative = TRUE),
             data = EmptyAnimateDataBegin,
             alpha = 0) +
  # transparent frame 2
  geom_point(aes(x = long,
                 y = lat,
                 size = followers,
                 frame = created_at,
                 cumulative = TRUE),
             data = EmptyAnimateDataEnd,
             alpha = 0) +
  scale_size_continuous(range = c(1, 6),
                        breaks = c(500, 1000, 2000, 4000, 6000, 8000)) +
  labs(size = 'Followers') +
  ggalt::coord_proj("+proj=wintri") +
  ggthemes::theme_hc() +
  ggplot2::theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()) +
  ggplot2::theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) +
  ggplot2::labs(title = "#DubNation & #StrengthInNumbers",
                subtitle = "tweets and followers")
library(gganimate)
gganimate(DubMap, interval = .2, "DubMap.gif")
