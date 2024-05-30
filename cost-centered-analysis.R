library(tidyverse)
library(janitor)
library(ggforce)
library(gtable)
library(grid)
library(gridtext)
library(ggrepel)
library(glue)

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
header_y <- 1.51

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

tot_requests <- 
  for_barchart_labels %>% 
  pull(tot_request) %>% 
  sum()

requests_africa <- 
  for_barchart_labels %>% 
  filter(consulate_country_continent == "Africa") %>% 
  pull(tot_request)

requests_asia <- 
  for_barchart_labels %>% 
  filter(consulate_country_continent == "Asia") %>% 
  pull(tot_request)

requests_others <- 
  for_barchart_labels %>% 
  filter(consulate_country_continent == "Others") %>% 
  pull(tot_request)

custom_currency_scale <- 
  scales::label_currency(
    prefix = "€ ",
    accuracy = 1,
    scale_cut = scales::cut_short_scale()
  )

custom_number_scale <- 
  scales::label_number(
    accuracy = .1,
    scale_cut = scales::cut_short_scale()
  )

custom_percent_scale <- 
  scales::label_percent(
    accuracy = 1
  )

p_cost <-
  for_barchart %>% 
  filter(type == "cost_lost") %>% 
  ggplot() +
  aes(x = cost_ratio,
      y = type) +
  geom_col(
    aes(fill = consulate_country_continent),
    colour = "black",
    width = nudge
  ) +
  geom_text(
    data = for_barchart_labels,
    aes(
      x = cost_lost_positon,
      y = 1 + nudge/2,
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
      y = 1 + nudge/2,
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
  annotate(
    x = 0,
    xend = 1,
    y = header_y,
    yend = header_y,
    geom = "segment",
    arrow = arrow(length = unit(5, "mm"))
  ) +
  annotate(
    geom = "text",
    y = 1 + nudge/2,
    x = 1.03,
    label = glue::glue(
      "Total Applications:\n\n",
      "- Africa {requests_africa %>% custom_number_scale()}",
      " ({custom_percent_scale(requests_africa/tot_requests)})\n",
      "- Asia {requests_asia %>% custom_number_scale()}",
      " ({custom_percent_scale(requests_asia/tot_requests)})\n",
      "- Others  {requests_others %>% custom_number_scale()}",
      " ({custom_percent_scale(requests_others/tot_requests)})"
    ),
    size = text_size,
    hjust = 0,
    vjust = 1
  ) +
  geom_label(
    data = . %>% 
      summarise(cost = sum(cost)),
    aes(y = header_y,
        x = 0,
        label = paste(
          "Tot:",
          cost %>% custom_currency_scale())
    ),
    hjust = 0,
    fill = "white",
    label.size = 0
  ) +
  scale_fill_manual(
    values = c(
      Africa = colorspace::lighten('#228833', .3),
      Asia = colorspace::lighten('#4477AA', .3),
      Others = "grey80"
    )
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.003, .27))) +
  guides(fill = "none") +
  labs(title = "Cost of Schengen Visa Rejections In 2023",
       subtitle = "Estimated on a € 80 application fee") +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank()
  )

# side_text <- 
#   textbox_grob(
#     glue::glue(
#       "Total Visa Applications:<br>",
#       "- Africa,<br>",
#       "- Asia,<br>",
#       "- Others"
#     ),
#     use_markdown = F,
#     vjust = 1
#     )
# 
# p_cost_grob <- p_cost %>% ggplotGrob() 
# grid.newpage();p_cost_grob %>% 
#   gtable_add_cols(widths = unit(3, "cm")) %>% 
#   gtable_add_grob(grobs = side_text,
#                   l = 14, t = 9) %>% 
#   # gtable_show_layout() %>% 
#   grid.draw()

ggsave(
  filename = "output/barchart-value-lost.jpeg", 
  plot = p_cost,
  width = 8,
  height = 3
  )


# uk data -------------------------------------------------------

library(tidyverse)
library(janitor)
library(countrycode)
library(grid)

cb_palette <- 
  c(Africa = '#339944',
    Americas = '#DDCC55',
    Asia = '#5588BB',
    Europe = '#EE6677',
    Oceania = '#AA3377')

visa_uk_mar <- 
  read_tsv('data/UK-visas-summary-mar-2023-tables.tsv')

visa_uk_dec <- 
  read_tsv('data/UK-visas-summary-dec-2023-tables.tsv')

custom_currency_scale_uk <- 
  scales::label_currency(
    prefix = "£ ",
    accuracy = .1,
    scale_cut = c(M=1e6)#scales::cut_short_scale()
  )

plot_uk_visa <- function(visa_uk, 
                         year_ending = "March 2023",
                         show_cost = TRUE) {
  
  visa_uk <- 
    visa_uk %>% 
    mutate(grant_rate = grant_rate %>% str_remove('%') %>% as.numeric()) %>% 
    mutate(grant_rate = grant_rate/100,
           rej_rate = 1-grant_rate) %>% 
    mutate(continent = countrycode(nationality,
                                   origin = 'country.name',
                                   destination = 'continent'))
  
  p <-
    visa_uk %>% 
    arrange(desc(rej_rate)) %>% 
    mutate(nationality = nationality %>% as_factor()) %>% 
    ggplot() +
    aes(y = nationality,
        x = rej_rate) +
    geom_segment(aes(yend = after_stat(y),
                     xend = 0),
                 linewidth = .5,
                 lty = '11') +
    geom_point(aes(size = resolved,
                   fill = continent),
               alpha = 1,
               pch = 21) +
    geom_vline(xintercept = 0,
               linewidth = 1.5) +
    labs(x = "Visa Rejection Rate",
         y = "Nationality",
         size = "Size:\nApplications Received",
         fill = "Colour:\nContinent",
         title = "UK Visitor Visa Rejection Rate by Nationality",
         subtitle = glue::glue("Year Ending {year_ending}")) +
    guides(fill = guide_legend(override.aes = list(size = 5)),
           size = guide_legend(override.aes = list(shape = 21,
                                                   fill = "white"))) +
    scale_x_continuous(limits = c(0, 1.05),
                       labels = scales::percent,
                       expand = expansion(0, 0),
                       breaks = c(0, .2, .4, .6, .8, 1)) +
    scale_size_continuous(range = c(0, 15),
                          labels = scales::comma) +
    scale_fill_manual(values = cb_palette) +
    theme_minimal(base_size = text_size*size_scale) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(colour = 'black',
                                            size = .1),
          # axis.title = element_text(hjust = 1),
          # legend.justification = "top"
          )
  
  if(show_cost) {
    p <-
      p +
      geom_text_repel(
        data = . %>%
          filter(
            nationality %in% c(
              "Nigeria",
              "Pakistan",
              "Albania",
              "Algeria",
              "Bangladesh",
              "Ghana",
              "Morocco"
            )
          ),
        aes(
          x = rej_rate,
          label = paste(
            refusal %>% `*`(100) %>% map_chr(custom_currency_scale_uk)
          ),
          hjust = -.2 - (resolved/max(resolved))/4
        ),
        colour = 'black',
        min.segment.length = 0, 
        nudge_x = .03,
        size = 3,
        direction = 'y',
        force = .1, 
        segment.size = 0.2
      ) 
  }
  
  # p <- ggplotGrob(p)

  # grid.newpage()
  # grid.draw(p)
  # grid.text(x = 0.76,
  #           y = 0.4,
  #           gp = gpar(fontsize = 12),
  #           label = str_wrap(
  #             glue::glue("Visitor visa applications and outcomes, by nationality, in the year ending {year_ending}. Top 20 nationalities applying for visitor visas in the year. Data relates to entry clearance visa applications made outside the UK. Includes main applicants and dependants."),
  #             width = 28),
  #           hjust = 0,
  #           vjust = 1)
  return(p)
}

p_uk <- plot_uk_visa(visa_uk_dec, year_ending = "December 2023")

ggsave(
  filename = "output/uk-for-cost-analysis.jpeg",
  plot = p_uk,
  height = 8,
  width = 8
)


# rejection rate model ------------------------------------------

country_level_2023 <- read_csv("data/2023-country-level-clean.csv")


cb_palette <- 
  c(Africa = '#228833',
    Americas = '#CCBB44',
    Asia = '#4477AA',
    Europe = '#EE6677',
    Oceania = '#AA3377')

highlight_countries <- c(
  'DZA',
  'NGA',
  'GHA',
  'SEN',
  'MAR',
  'ZAF'
)

plot_model <- function(modelled_data,
                       year,
                       gdp_year,
                       title =  glue("Schengen Visa Rejection Rate in {year}"),
                       model = TRUE,
                       width = 10,
                       ratio = 4/3) {
  country_level_scatter <- 
    modelled_data %>% 
    drop_na(consulate_country_continent) %>% 
    # drop_na(GDP_per_capita_eq_2021, ratio_rejected) %>% 
    ggplot() +
    aes(x = gdp_per_capita,
        colour = consulate_country_continent,
        y = ratio_rejected) +
    {if(model)geom_line(aes(y = .fitted),
                        colour = 'grey30',
                        lty = '31',
                        size = 0.8)} +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    geom_point(aes(size = tot_request),
                           alpha = .9) +
    labs(x = glue('GDP per Capita from previous year in equivalent dollars ($)'),
         y = 'Percent of Applications Rejected',
         size = "Size:\nApplications Received",
         colour = "Colour:\nContinent",
         title = title,
         subtitle = "(By country in which the visa application was lodged)") +
    guides(colour = guide_legend(override.aes = list(size = 5)),
           size = guide_legend(override.aes = list(shape = 21,
                                                   fill = "white"))) +
    scale_size_continuous(labels = scales::comma,
                          range = c(0, 15),
                          breaks = c(1e3, 1e4, 1e5, 1e6)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, NA)) +
    scale_x_continuous(limits = c(0, NA)) + 
    scale_colour_manual(values = cb_palette) +
    theme_minimal(base_size = text_size*size_scale) 
  
  
  ggsave(filename = glue('output/{title}-labels-WIDE.jpg'), 
         plot = country_level_scatter +
           geom_text_repel(
             data = . %>% filter(
               consulate_country_code %in% highlight_countries
             ),
             mapping = aes(label = consulate_country),
             colour = 'black',
             min.segment.length = 0, 
             # nudge_x = 2e4,
             # nudge_y = .1, 
             xlim = c(3e4, NA),
             ylim = c(0.25, NA),
             size = 3,
             direction = 'y',
             force = .1, 
             segment.size = 0.2,
           ),
         # theme(legend.position = "bottom",
         #       legend.box="vertical"),
         width = width,
         height = width/ratio) 
  
  ggsave(filename = glue('output/{title}-WIDE.jpg'),
         plot = country_level_scatter,
         # theme(legend.position = "bottom",
         #       legend.box="vertical"),
         width = width,
         height = width/ratio)
  
  
  return(country_level_scatter)
}

set.seed(1);plot_model(
  country_level_2023,
  year = 2023,
  gdp_year = 2022,
  title = "Schengen Visa Rejection Rate in 2023"
)

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


