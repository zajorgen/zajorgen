library(hoopR)
library(dplyr)

# Pull all D1 men's teams
teams <- espn_mbb_teams()

# Filter for the teams you wa
selected_teams <- teams %>%
  filter(display_name %in% c("BYU Cougars", "Gonzaga Bulldogs", "Purdue Boilermakers", "Houston Cougars", "Duke Blue Devils", "Arkansas Razorbacks", "UConn Huskies", "Arizona Wildcats", "Clemson Tigers", "Florida Gators", "Georgetown Hoyas", "	
Indiana Hoosiers", "Kansas Jayhawks", "Louisville Cardinals", "North Carolina Tar Heels", "Ole Miss Rebels", "Purdue Boilermakers", "Saint Mary's Gaels", "Texas Tech Red Raiders" ))

# Inspect the results"
print(selected_teams)

team.ids <- selected_teams$team_id

# Pull stats for BYU (first team ID)
byu_stats <- espn_mbb_team_stats(team_id = team.ids[1],year = 2024, season_type = "regular")

# See the column names and first few rows
colnames(byu_stats)
head(byu_stats)

library(purrr)

# Function to pull stats for a single team
get_team_stats <- function(id) {
  df <- espn_mbb_team_stats(team_id = id, season_type = "regular", year = 2024)
  
  # Keep only the columns you want (adjust as needed)
  cols_to_keep <- intersect(c("team_id", "short_name", "general_assist_turnover_ratio", 
                              "defensive_steals", "defensive_turnover_points"), colnames(df))
  df %>% select(all_of(cols_to_keep))
}

# Map over all team IDs
all_team_stats <- map_dfr(team.ids, get_team_stats)

# Preview results
all_team_stats

# selected_teams already has team_id and team_name columns
all_team_stats <- all_team_stats %>%
  left_join(selected_teams %>% select(team_id, short_name), by = "team_id") %>%
  select(team_id, short_name, everything())  # optional: move team_name to front

all_team_stats

library(ggplot2)
library(tidyr)

# Pivot data to long format for plotting
plot_data <- all_team_stats %>%
  pivot_longer(cols = c( "defensive_steals", "defensive_turnover_points"),
               names_to = "stat",
               values_to = "value")

# Create grouped bar chart
ggplot(plot_data, aes(x = short_name, y = value, fill = stat)) +
  geom_col(position = "dodge") +
  labs(title = "2024 Defensive Metrics",
       x = "Team",
       y = "Value",
       fill = "Stat") +
  theme_minimal()


# Basic bar chart for assist turnover ratio
ggplot(all_team_stats, aes(x = short_name, y = general_assist_turnover_ratio, fill = short_name)) +
  geom_col() +                       # geom_col() creates bars
  labs(
    title = "Assists to Turnovers - 2024 Season",
    x = "Team",
    y = "Ratio"
  ) +
  theme_minimal() +                  # clean minimal theme
  theme(legend.position = "none")    # hide legend since team names are on x-axis


install.packages("plotly")
install.packages("broom")

library(ggplot2)
library(plotly)
library(broom)   # for tidy regression results

# Example: Scatter plot of steals vs turnover points

# Fit linear model
model <- lm(defensive_turnover_points ~ defensive_steals, data = all_team_stats)
model_summary <- tidy(model)

# Create a text string for the regression equation
equation_text <- paste0(
  "y = ", round(model_summary$estimate[1], 2), " + ",
  round(model_summary$estimate[2], 2), "x"
)

# Create interactive plot
p <- ggplot(all_team_stats, aes(x = defensive_steals, y = defensive_turnover_points, label = short_name)) +
  geom_point(size = 3, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Interactive Regression: Steals vs Turnover Points",
    subtitle = equation_text,
    x = "Steals",
    y = "Points off Turnovers"
  ) +
  theme_minimal()

ggplotly(p, tooltip = c("short_name", "defensive_turnover_points", "defensive_steals"))

#regression plot with logos#
install.packages("ggimage")
library(ggimage)
library(ggplot2)
library(dplyr)
install.packages("ggplot2")


# Pull team data (including logos)
team_info <- hoopR::espn_mbb_teams() %>%
  select(team_id, logo)

# Join with your stats tibble
all_team_stats <- all_team_stats %>%
  left_join(team_info, by = "team_id")

library(ggimage)
library(ggplot2)

ggplot(all_team_stats, aes(x = defensive_steals, y = defensive_turnover_points)) +
  geom_image(aes(image = logo), size = 0.08) +
  labs(
    title = "Steals vs Turnover Points with Team Logos",
    x = "Steals",
    y = "Points off Turnovers"
  ) +
  theme_minimal()


