library(tidyverse)
library(gganimate)


# Load data ---------------------------------------------------------------


data <- read.csv("http://cowid.netlify.com/data/full_data.csv")
data <-
  data %>% replace_na(.,
                      list(
                        new_cases = 0,
                        new_deaths = 0,
                        total_cases = 0,
                        total_deaths = 0
                      )) %>%
  filter(location != "Worldwide") %>%
  mutate(date = as.Date(date)) %>%  as_tibble()


# Prepare data ------------------------------------------------------------


data <- data %>%  mutate(img = case_when(location == "Norway" ~ "NO",
                                         location == "China" ~ "CN",
                                         location == "Italy"  ~ "IT",
                                         location == "Thailand" ~ "TH",
                                         location == "Australia" ~ "AU",
                                         location == "Singapore" ~ "SG",
                                         location == "France" ~ "FR",
                                         location == "Japan"~ "JP",
                                         location == "Malaysia" ~ "MY",
                                         location == "South Korea" ~ "KR",
                                         location == "United States" ~ "US",
                                         location == "Vietnam" ~ "VN",
                                         location == "Canada" ~ "CA",
                                         location == "Germany" ~ "DE",
                                         location == "Iran" ~ "IR",
                                         location == "Bahrain" ~ "BH",
                                         location == "Kuwait" ~ "KW",
                                         location == "Spain" ~ "ES",
                                         location == "United Kingdom" ~ "UK",
                                         location == "Switzerland" ~ "CH",
                                         location == "Netherlands" ~ "NL",
                                         TRUE ~ "BZ"
                                         ),
                         link = paste0("https://www.countryflags.io/", img, "/shiny/64.png")
                         ) %>% 
  filter(!location %in% c("China","World", "International"))


data_agg <- data %>%
  group_by(date) %>%
  mutate(frame_id = group_indices()) %>%
  ungroup()

flourish_data <- data_agg %>% arrange(frame_id) %>%  
  select(location, frame_id, link, total_cases) %>% 
  pivot_wider(names_from = frame_id, values_from = total_cases, values_fill = list(total_cases = "0")) %>% 
  write.csv(file = "flourish.csv")


# Bar Chart Race (R-version) -----------------------------------------------------
# Not ready


data_agg <-
  map_df(
    unique(data_agg$frame_id),
    ~ data_agg %>% filter(frame_id == .x &
                            !location %in% c("China", "International")) %>%
      arrange(-total_cases) %>%
      mutate(
        ordering = 1:nrow(.),
        indicator = case_when(max(ordering) <= 10 ~ 1,
                              TRUE ~ 0)
      )
  ) %>%
  filter(indicator == 0 & ordering <= 10) %>%
  arrange(frame_id, ordering)


data_use <- data_agg %>% mutate(curr_year = frame_id)
my_font <- 'Quicksand'
my_background <- 'antiquewhite'
my_pal <- c('#F8AFA8', '#74A089') 
my_theme <- my_theme <- theme(
  text = element_text(family = my_font),
  rect = element_rect(fill = my_background),
  plot.background = element_rect(fill = my_background, color = NA),
  panel.background = element_rect(fill = my_background, color = NA),
  panel.border = element_blank(),
  plot.title = element_text(face = 'bold', size = 20),
  plot.subtitle = element_text(size = 14),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_line(color = 'grey75'),
  panel.grid.minor.x = element_line(color = 'grey75'),
  legend.position = 'none',
  plot.caption = element_text(size = 8),
  axis.ticks = element_blank(),
  axis.text.y =  element_blank()
)

theme_set(theme_light() + my_theme)

ggplot(aes(ordering, group = location), data = data_use) +
  geom_tile(aes(
    y = total_cases / 2,
    height = total_cases,
    width = 0.9
  ), alpha = 0.9) +
  scale_fill_manual(values = my_pal) +
  geom_text(
    aes(y = total_cases, label = location),
    family = my_font,
    nudge_y = -2,
    size = 3
  ) +
  geom_text(aes(y = total_cases, label = total_cases),
            family = my_font,
            nudge_y = 0.5) +
  geom_text(
    aes(
      x = 1,
      y = 18.75,
      label = paste0(curr_year)
    ),
    family = my_font,
    size = 8,
    color = 'gray45'
  ) +
  coord_cartesian(clip = "off", expand = FALSE) +
  coord_flip() +
  labs(
    title = 'Covid-19 Development (Outside China)',
    subtitle = 'Development day-by-day after outbreak',
    caption = 'data source: ourworldindata.org',
    x = '',
    y = ''
  ) +
  transition_states(frame_id,
                    transition_length = 7,
                    state_length = 3) +
  ease_aes('cubic-in-out')


#labs(title = "Date: {frame_time}")
