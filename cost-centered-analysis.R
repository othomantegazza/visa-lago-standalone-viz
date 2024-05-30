library(tidyverse)
library(janitor)
library(ggforce)

theme_set(theme_minimal())

schengen_2023 <- 
  read_csv("data/2023-country-level-clean.csv")

visa_cost <- 80 # euro
size_scale <- 2.8455
text_size <- 4

# Load development indicators -----------------------------------

dev_i <-
  # read_csv('data/WDIData.csv') %>% 
  read_csv('data/WDICSV.csv') %>% 
  clean_names() 


# Population, total ---------------------------------------------

# 2022 is the last date available
pop_c_2022 <- 
  dev_i %>% 
  filter(
    indicator_name == 'Population, total'
  ) %>% 
  select(
    country_name,
    country_code,
    indicator_name, 
    indicator_code, 
    pop_2019 = x2019,
    pop_2021 = x2021,
    pop_2022 = x2022
  ) %>% 
  pivot_longer(
    cols = c(
      pop_2019,
      pop_2021,
      pop_2022
    ),
    names_to = 'year',
    values_to = 'pop'
  ) %>% 
  mutate(year = year %>%
           str_remove('pop_') %>%
           as.numeric()) %>% 
  filter(year == 2022)


# GDP per capita, non scaled ------------------------------------


gdp_c_simple_2022 <- 
  dev_i %>% 
  filter(
    indicator_name == 'GDP per capita (current US$)'
  ) %>% 
  select(
    country_name,
    country_code,
    indicator_name, 
    indicator_code, 
    gdp_c_simple_2019 = x2019,
    gdp_c_simple_2021 = x2021,
    gdp_c_simple_2022 = x2022
  ) %>%  
  pivot_longer(
    cols = c(
      gdp_c_simple_2019,
      gdp_c_simple_2021,
      gdp_c_simple_2022
    ),
    names_to = 'year',
    values_to = 'gdp_c_simple'
  ) %>% 
  mutate(year = year %>%
           str_remove('gdp_c_simple_') %>%
           as.numeric()) %>% 
  filter(year == 2022)


# put everything together ---------------------------------------

schengen__2023_cost <- 
  schengen_2023 %>% 
  left_join(
    pop_c_2022 %>% 
      select(country_code, pop), 
    by = c(
      "consulate_country_code" = "country_code"
      )
    ) %>% 
  left_join(
    gdp_c_simple_2022 %>% 
      select(country_code, gdp_c_simple), 
    by = c(
      "consulate_country_code" = "country_code"
    )
  ) %>% 
  mutate(cost_lost = not_issued*visa_cost,
         cost_estimate = tot_request*visa_cost)

# rejection bar chart -------------------------------------------

nudge <- .7

for_barchart <- 
  schengen__2023_cost %>% 
  drop_na(consulate_country_continent) %>% 
  mutate(
    consulate_country_continent = consulate_country_continent %>% 
      {
        case_when(
          . %in% c("Africa", "Asia") ~ .,
          TRUE ~ "Others"
        )
      }
  ) %>% 
  summarise(
    cost_estimate = cost_estimate %>% sum(na.rm = T),
    cost_lost = cost_lost %>% sum(na.rm = T),
    tot_request = tot_request %>% sum(na.rm = T),
    not_issued = not_issued %>% sum(na.rm = T),
    .by = consulate_country_continent
  ) %>% 
  pivot_longer(
    cols = c(
      cost_estimate,
      cost_lost
    ),
    values_to = "cost",
    names_to = "type"
  ) %>% 
  mutate(cost_ratio = cost/sum(cost),
         .by = type) %>% 
  mutate(
    consulate_country_continent = factor(
      consulate_country_continent, 
      levels = c(
        "Others",
        "Asia",
        "Africa"
      )
    )
  ) 
# arrange(consulate_country_continent, type) %>% 
# mutate(next_cost = cost_ratio[c(2:n(), 1)]) 

for_barchart_con <- 
  for_barchart %>% 
  filter(consulate_country_continent %in% c("Asia", "Africa")) %>% 
  select(-cost, -tot_request, -not_issued) %>% 
  pivot_wider(names_from = type, values_from = cost_ratio) %>% 
  mutate(cost_estimate = cumsum(cost_estimate),
         cost_lost = cumsum(cost_lost))

for_barchart_labels <- 
  for_barchart_con %>% 
  bind_rows(
    tibble(
      consulate_country_continent = "Others",
      cost_lost = 0,
      cost_estimate = 0)
  ) %>% 
  rename(
    cost_lost_positon = cost_lost,
    cost_estimate_position = cost_estimate
  ) %>% 
  mutate(
    cost_lost_positon = cost_lost_positon[c(n(),1:(n()-1))],
    cost_estimate_position = cost_estimate_position[c(n(),1:(n()-1))]
  ) %>% 
  left_join(
    for_barchart %>% 
      select(-cost_ratio) %>% 
      pivot_wider(
        names_from = type,
        values_from = cost
      ),
    by = join_by(consulate_country_continent)
  ) %>% 
  mutate(ratio_rejected = not_issued/tot_request) %>% 
  mutate(perc_rejection = not_issued/sum(not_issued),
         perc_request = tot_request/sum(tot_request))

custom_currency_scale <- 
  scales::label_currency(
    prefix = "€",
    accuracy = .1,
    scale_cut = scales::cut_short_scale()
  )

custom_number_scale <- 
  scales::label_number(
    accuracy = .1,
    scale_cut = scales::cut_short_scale()
  )

custom_percent_scale <- 
  scales::label_percent(
    accuracy = .1
  )

for_barchart %>% 
  mutate(type = type %>% {
    case_when(
      . == "cost_lost" ~ "Value lost to visa rejection (80 euro per rejected visa)",
      . == "cost_estimate" ~ "Total count of visa applications",
    )
  } %>% 
    str_wrap(15)) %>% 
  ggplot() +
  aes(x = cost_ratio,
      y = type) +
  geom_col(
    aes(fill = consulate_country_continent),
    colour = "black",
    width = nudge
  ) +
  geom_segment(
    data = for_barchart_con,
    aes(
      y = 1 + nudge/2,
      yend = 2 - nudge/2,
      x = cost_estimate,
      xend = cost_lost
    ),
    linetype = "11"
  ) +
  geom_text(
    data = for_barchart_labels,
    aes(
      x = cost_lost_positon,
      y = 2 + nudge/2,
      label = consulate_country_continent
    ),
    size = text_size,
    hjust = -0.1,
    vjust = -.5
  ) +
  scale_x_continuous(
    expand = expansion(.005)
  ) +
  geom_text(
    data = for_barchart_labels,
    aes(
      x = cost_lost_positon,
      y = 2 + nudge/2,
      label = paste(
        custom_percent_scale(perc_rejection),
        custom_currency_scale(cost_lost),
        sep = "\n"
      )
    ),
    size = text_size,
    hjust = -0.1,
    vjust = 1.2
  ) +
  geom_text(
    data = for_barchart_labels,
    aes(
      x = cost_estimate_position,
      y = 1 + nudge/2,
      label = paste(
        custom_percent_scale(perc_request),
        custom_number_scale(tot_request),
        sep = "\n"
      )
    ),
    size = text_size,
    hjust = -0.1,
    vjust = 1.2
  ) +
  scale_fill_manual(
    values = c(
      Africa = colorspace::lighten('#228833', .5),
      Asia = colorspace::lighten('#4477AA', .5),
      Others = "grey80"
    )
  ) +
  guides(fill = "none") +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    axis.text.y = element_text(
      vjust = .5, 
      size = text_size*size_scale,
      colour = "black"
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank()
  )

ggsave(filename = "output/barchart-value-lost.jpeg",
       width = 8,
       height = 2.9)

# rejection rates and costs -------------------------------------

schengen__2023_cost %>% 
  filter(consulate_country_continent %in% c("Africa", "Asia")) %>%
  mutate(cost_lost_perc = cost_lost/cost_estimate) %>%
  # mutate(cost_lost = (cost_lost + 1) * 80) %>% # effective cost by visa
  select(
    country_name,
    cost_lost_perc,
    cost_lost,
    consulate_country_continent
  ) %>% 
  arrange(cost_lost_perc) %>% 
  mutate(
    order = seq(
      from = min(cost_lost_perc, na.rm = T),
      to = max(cost_lost_perc, na.rm = T),
      length.out = n()
    )
  ) %>% 
  ggplot() +
  aes(
    x = 1,
    colour = consulate_country_continent
  ) +
  geom_point(
    aes(y = cost_lost_perc,
        size = cost_lost),
  ) +
  geom_text(
    aes(
      x = 2,
      y = order,
      label = country_name
    ),
    hjust = 0,
    size = 2.5
  )  +
  geom_segment(
    aes(
      xend = 1.99,
      y = cost_lost_perc,
      yend = order
    ),
    linetype = "11"
  ) +
  labs(y = "Percent rejected)") +
  scale_x_continuous(
    expand = expansion(mult = c(.1, .5))
  ) +
  scale_y_continuous(
    labels = scales::percent
  ) +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# circle model --------------------------------------------------

schengen__2023_cost %>% 
  # filter(consulate_country %in% c("ALGERIA", "PAKISTAN")) %>% 
  summarise(
    cost_lost = cost_lost %>% sum(na.rm = T),
    cost_estimate = cost_estimate %>% sum(na.rm = T),
    .by = consulate_country_continent
  ) %>% 
  ggplot() +
  geom_circle(
    aes(
      x0 = 1,
      y0 = cost_estimate,
      r = cost_estimate
    )
  ) +
  geom_circle(
    aes(
      x0 = 1,
      y0 = cost_lost,
      r = cost_lost
    )
  ) +
  coord_fixed() +
  facet_wrap(facets = "consulate_country_continent")

# linear model --------------------------------------------------

cost_2023_for_lm <- 
  schengen__2023_cost %>% 
  summarise(
    cost_estimate = cost_estimate %>% sum(na.rm = T),
    pop = pop %>% sum(na.rm = T),
    cost_lost = cost_lost %>% sum(na.rm = T),
    .by = consulate_country_continent
  ) %>% 
  drop_na()

fit <- 
  lm(cost_lost ~ cost_estimate + 0,
     data = cost_2023_for_lm,
     weights = pop)

cost_2023_fitted <- 
  fit %>% 
  broom::augment(newdata = cost_2023_for_lm)

cost_2023_fitted %>% 
  ggplot() +
  aes(
    x = cost_estimate,
    y = cost_lost
  ) + 
  # geom_smooth(
  #   method = "lm",
  #   se = FALSE,
  # ) +
  # geom_line(
  #   aes(y = .fitted)
  # ) +
  geom_abline(
    intercept = 0,
    slope = fit %>% broom::tidy() %>% pull(estimate)
  ) +
  labs(x = "Total expense on Schengen Short Stay Visa",
       y = "Expense on rejected visa")+
  geom_segment(
    aes(xend = after_stat(x),
        yend = .fitted),
    linetype = "11"
  ) +
  geom_point(
    aes(fill = consulate_country_continent,
        size = pop),
    # size = 5,
    shape = 21
  )  +
  scale_x_continuous(
    labels = scales::label_currency(
      prefix = "€",
      accuracy = 1,
      scale_cut = scales::cut_short_scale()
     )
  ) +
  scale_y_continuous(
    labels = scales::label_currency(
      prefix = "€",
      accuracy = 1,
      scale_cut = scales::cut_short_scale()
    )
  ) +
  scale_size(range = c(1, 10))

schengen__2023_cost %>% 
  ggplot() +
  aes(x = gdp_c_simple,
      y =ratio_rejected,
      colour = consulate_country_continent) +
  # geom_point() +
  geom_text(
    aes(label = country_name,
        size = tot_request)
  ) +
  scale_x_log10()

schengen__2023_cost %>% 
  ggplot() +
  aes(x = tot_request, y = cost_estimate) +
  geom_point() +
  geom_segment(
    aes(xend = tot_request,
        yend = cost_lost)
  )

schengen__2023_cost %>% 
  ggplot() +
  aes(
    x = gdp_per_capita,
    # y = cost_lost
    y = cost_estimate
    ) +
  geom_point()

schengen__2023_cost %>% 
  ggplot() +
  aes(x = pop,
      y = cost_lost,
      colour = con) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() 

schengen__2023_cost %>% 
  ggplot() +
  aes(x = gdp_per_capita,
      y = cost_estimate/pop,
      colour = consulate_country_continent) +
  geom_point() 

schengen__2023_cost %>% 
  summarise(
    pop = pop %>% sum(na.rm = T),
    cost_estimate = cost_estimate %>% sum(na.rm = T),
    .by = consulate_country_continent
    ) %>% 
  ggplot() +
  aes(
    x = pop,
    y = cost_estimate
  ) + 
  geom_smooth(
    method = "lm",
    se = FALSE,
    ) +
  geom_point(
    aes(fill = consulate_country_continent),
    size = 5,
    shape = 21
    )

schengen__2023_cost %>% 
  summarise(
    pop = pop %>% sum(na.rm = T),
    cost_lost = cost_lost %>% sum(na.rm = T),
    .by = consulate_country_continent
  ) %>% 
  ggplot() +
  aes(
    x = pop,
    y = cost_lost
  ) + 
  geom_smooth(
    method = "lm",
    se = FALSE,
  ) +
  geom_point(
    aes(fill = consulate_country_continent),
    size = 5,
    shape = 21
  )

schengen__2023_cost %>% 
  ggplot() +
  aes(
    x = pop,
    y = cost_lost/pop
  ) + 
  geom_smooth(
    aes(colour = consulate_country_continent),
    method = "lm",
    se = FALSE,
  ) +
  geom_point(
    aes(fill = consulate_country_continent),
    size = 5,
    shape = 21
  ) +
  scale_y_log10() +
  scale_x_log10()

schengen__2023_cost %>% 
  filter(consulate_country_continent %in% c("Africa", "Asia")) %>% 
  ggplot() +
  aes(
    colour = consulate_country_continent,
    x = gdp_per_capita,
    y = cost_lost / pop,
  ) + 
  geom_smooth(
    # aes(colour = consulate_country_continent),
    method = "lm",
    se = FALSE,
  ) +
  # geom_point(
  #   # aes(colour = consulate_country_continent),
  #   size = 3,
  #   stroke = 2,
  #   shape = 1
  # ) +
  geom_text(
    aes(label = country_name)
  ) +
  stat_ellipse(type = "norm", linetype = 2) +
  stat_ellipse(type = "t") +
  scale_y_log10() +
  scale_x_log10()

schengen__2023_cost %>% 
  filter(consulate_country_continent %in% c("Africa", "Asia")) %>% 
  ggplot() +
  aes(
    colour = consulate_country_continent,
    x = gdp_per_capita,
    y = cost_lost,
    
  ) + 
  geom_smooth(
    # aes(colour = consulate_country_continent),
    method = "lm",
    se = FALSE,
  ) +
  # geom_point(
  #   # aes(colour = consulate_country_continent),
  #   size = 3,
  #   stroke = 2,
  #   shape = 1
  # ) +
  geom_text(
    aes(label = country_name)
  ) +
  stat_ellipse(type = "norm", linetype = 2) +
  stat_ellipse(type = "t") +
  scale_y_log10() +
  scale_x_log10()

schengen__2023_cost %>% 
  filter(consulate_country_continent %in% c("Africa", "Asia")) %>% 
  ggplot() +
  aes(
    colour = consulate_country_continent,
    x = pop,
    y = cost_estimate
  ) + 
  geom_smooth(
    # aes(colour = consulate_country_continent),
    method = "lm",
    se = FALSE,
  ) +
  # geom_point(
  #   # aes(colour = consulate_country_continent),
  #   size = 3,
  #   stroke = 2,
  #   shape = 1
  # ) +
  geom_text(
    aes(label = country_name)
  ) +
  stat_ellipse(type = "norm", linetype = 2) +
  stat_ellipse(type = "t") +
  scale_y_log10() +
  scale_x_log10()

schengen__2023_cost %>% 
  filter(consulate_country_continent %in% c("Africa", "Asia")) %>%
  ggplot() +
  aes(
    colour = consulate_country_continent,
    x = tot_request,
    y = cost_lost,
  ) + 
  geom_smooth(
    # aes(colour = consulate_country_continent),
    method = "lm",
    se = FALSE,
  ) +
  # geom_point(
  #   # aes(colour = consulate_country_continent),
  #   size = 3,
  #   stroke = 2,
  #   shape = 1
  # ) +
  geom_text(
    aes(label = country_name)
  ) +
  stat_ellipse(type = "norm", linetype = 2) +
  stat_ellipse(type = "t") +
  scale_y_log10() +
  scale_x_log10()


schengen__2023_cost %>% 
  filter(consulate_country_continent %in% c("Africa", "Asia")) %>%
  ggplot() +
  aes(y = cost_lost/cost_estimate,
      x = 1) +
  geom_text(
    aes(
      label = country_name
    )
  )


schengen__2023_cost %>% 
  ggplot() +
  aes(x = gdp_c_simple,
      y =ratio_rejected,
      colour = consulate_country_continent) +
  # geom_point() +
  geom_text(
    aes(label = country_name,
        size = tot_request)
  ) +
  scale_x_log10()

schengen__2023_cost %>% 
  filter(consulate_country %in% c("ALGERIA", "PAKISTAN")) %>% 
  ggplot() +
  geom_circle(
    aes(
      x0 = c(1,2),
      y0 = cost_estimate,
      r = cost_estimate
    )
  ) +
  geom_circle(
    aes(
      x0 = c(1,2),
      y0 = cost_lost,
      r = cost_lost
    )
  ) +
  coord_fixed() +
  facet_wrap(facets = "consulate_country")


