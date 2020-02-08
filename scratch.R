# scratch

# TABLE WITH KABLE
df = read_csv(get_path('all_scores.csv')) %>%
  # ignore datetime - time zones mess things up anyway
  mutate(date = as_date(datetime)) %>%
  select(-datetime) %>%
  select(date, everything())

colnames(df) %<>% gsub('[.]*\\_prob1', '', .)
colnames(df) %<>% gsub('result\\_[.]*', '', .)

# Today's games
df_today = df %>% 
  filter(between(date, Sys.Date(), Sys.Date() + 1)) %>%
  select(-result, -starts_with('pg'), -season_week)


# Count number of games today for row packing
n_today = sum(df_today$date == Sys.Date())

date_today = paste(format(Sys.Date(), '%a, %b %d'), "| Probability Home Team Wins, Home vs. Away")
date_tom = format(Sys.Date() + 1, '%a, %b %d')

# Format the prediction columns with formattabl
p_cols = df_today %>% select(-team1, -team2, -date)

# TODO: https://www.displayr.com/formattable/
color_pred = function(x){
  # """ This works but not as well
  x = formattable::percent(x, digits=0)
  x = ifelse(x > 0.5,
             color_tile('transparent', green)(x * c(x > 0.5)),
             color_tile(red, 'transparent')(x * c(x < 0.5)))
  x
}

color_map = function(x){
  cell_spec(
    percent(x, 1), 
    color = "white", # font color
    bold = T,
    background = case_when(
      # x > 0.65 ~ green, 
      x > 0.65 ~ dk_blue, 
      # x < 0.65 & x >= 0.5 ~ green3,
      x < 0.65 & x >= 0.5 ~ light_blue,
      x < 0.5 & x >= 0.35 ~ orange,
      x < 0.35 ~ red,
      TRUE ~ gray,
    )
  )
}

# https://stackoverflow.com/a/54748839/2138773
df_today %>%
  select(-date) %>%
  mutate(
    game = paste(team1, 'vs', team2),
    elo = color_map(elo),
    `carm-elo` = color_map(`carm-elo`),
    raptor = color_map(raptor),
    v01 = color_map(v01),
    v02 = color_map(v02)
  ) %>%
  select(-team1, -team2) %>%
  select(game, everything()) %>%
  kable('html', escape=F) %>%
  kable_styling(c('striped', 'hover'), full_width = F) %>%
  pack_rows(date_today, 1, n_today, label_row_css = css) %>%
  pack_rows(date_tom, n_today+1, nrow(df_today), label_row_css = css)



# YESTERDAY WITH KABLE----
# TODO: https://www.displayr.com/formattable/
# Get the green check for correct predictions - it's cool
# Then tally up at the bottom
# color_pred_correct <- function(result, p){
#   truth = (p > 0.5) & (result == 1)
#   p = formatter("span",
#       style = x ~ style(color = ifelse(truth, "green", "red")),
#       x ~ icontext(ifelse(truth, "ok", "remove"), ifelse(truth, p, p)))
#   return(p)
# }
# df_yesterday %>%
#   # mutate(
#   #   
#   # ) %>%
#   formattable(
#     list(
#       elo = formatter(
#         "span",
#         style = ~ style(color = ifelse((result == 1 & elo > 0.5) | (result == 0 & elo < 0.5), green, red)),
#         ~ icontext(ifelse((result == 1 & elo > 0.5) | (result == 0 & elo < 0.5), "ok", "remove"), percent(elo, 1))
#       )
#     )
#   ) #%>%
# kable(escape=F, digits=3) %>%
# kable_styling(c('striped', 'hover'), full_width = F)




# CALCULATE RECORDS -----
mod_accs = c()
df_proj = df
models = c('raptor', 'v02', 'carm-elo', 'elo')
for (mod in models){
  res_mod = paste0(mod, '_result')
  pred = as.numeric(df[[mod]] > 0.5)
  df_proj[[res_mod]] = ifelse(is.na(df$result), pred, df$result)
}
df_proj$elo_result == df_proj$result

# assert predictions are the same
# https://projects.fivethirtyeight.com/2020-nba-predictions/
res_name = 'elo_result'
res_name = 'v02_result'
res_name = 'raptor_result'
res_name = 'carm-elo_result'
res_name = 'result'
df_proj %>%
  filter(!is.na(get(res_name))) %>%
  mutate(
    result1 = get(res_name),
    result2 = ifelse(get(res_name) == 1, 0, 1)
  ) %>%
  select(team1, team2, result1, result2) %>%
  # reshape long to be team | result
  gather('team', 'result', -team1, -team2) %>%
  mutate(team = ifelse(team == 'result1', team1, team2)) %>%
  select(-team1, -team2) %>%
  # Count wins and losses
  group_by(team) %>%
  summarize(wins = sum(result == 1),
            losses = sum(result == 0),
            n_games = wins + losses) %>%
  mutate(pct = round(wins/(wins + losses), 2) * 100) %>%
  arrange(desc(pct))

