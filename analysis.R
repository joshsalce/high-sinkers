# Conclusion: If you have a sinker and throw it high, you have to live in the shadow zone.
# Vertical for comparable swing-and-miss to FF, and horizontal for better BIP results

source("startup.R")

source("do.R")

# Pull, concatenate Statcast data for each season
data24 <- GetSeasonStatcastData("2024-03-28", "2024-09-30")

data23 <- GetSeasonStatcastData("2023-03-30", "2023-10-01")

data22 <- GetSeasonStatcastData("2022-03-31", "2022-10-02")

data21 <- GetSeasonStatcastData("2021-04-01", "2021-10-03")

statcast_data_bind <- bind_rows(data21, data22, data23,data24) %>%
  filter(balls < 4 & strikes < 3 & !(pitch_type %in% c("FA", "PO", "EP"))) %>%
  mutate(
    high_in_zone = ifelse(
      zone %in% c(1:3, 11:12) & ((abs(plate_x) <= 1.108333) & (plate_z <= 3.833333)), 1, 0
    ),
    IsShadow = ifelse(
      (plate_z >= 3.166667 & plate_z <= 3.833333)
      & (abs(plate_x) <= 1.108333),
      1, 0
    )
  ) %>%
  arrange(game_year, game_date, game_pk, at_bat_number, pitch_number) 

# Checks ===============================================================================================================

statcast_data <- add_id_cols(statcast_data_bind)

# Filter for sinker data 
si_data <- statcast_data %>%
  filter(
    pitch_type == "SI" 
  ) 

# Get high four-seam data for comparison
high_ff_data <- statcast_data %>%
  filter(
    pitch_type == "FF" 
    & zone %in% c(1:3, 11:12)
  ) 

## Sinkers Overall =====================================================================================================

# Get Up-In-Zone%
si_overall_grouped <- 
  si_data %>%
  group_by(pitcher, player_name, game_year, p_throws) %>%
  summarise(
    N = n(),
    Up_In_Zone_pct = (sum(high_in_zone) / N)*100,
  ) %>% 
  ungroup() %>%
  filter(N >= 50) %>%
  mutate(next_year = game_year + 1)

si_year_to_year <- 
  merge(
  si_overall_grouped %>%
    select(player_name, game_year, Up_In_Zone_pct, next_year),
  si_overall_grouped %>%
    select(player_name, game_year, Up_In_Zone_pct),
  by.x = c("player_name", "next_year"),
  by.y = c("player_name", "game_year")
) %>%
  rename(
    Up_In_Zone_pct_prev = Up_In_Zone_pct.x,
    Up_In_Zone_pct_next = Up_In_Zone_pct.y
  )

# Check: Predictiveness of Up-In-Zone%
ggplot(si_year_to_year, aes(x = Up_In_Zone_pct_prev, y = Up_In_Zone_pct_next)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(method = "pearson", label.x = 5, label.y = 42, size = 5) +
  labs(
    title = "Year 2 Up-In-Zone% vs. Year 1 Up-In-Zone%, Sinkers 2021-2024",
    x = "Year 1 Up-In-Zone%", y = "Year 2 Up-In-Zone%"
  ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18)
  )

## High Sinkers ========================================================================================================

high_si_data <- 
  si_data %>%
  filter(high_in_zone == 1)

# Perform summary info calculations for high sinekrs
high_si_grouped <- 
  high_si_data %>%
  group_by(pitcher, player_name, game_year, p_throws) %>%
  summarise(
    N = n(),
    Shadow_Zone_pct = sum(IsShadow, na.rm = TRUE) / N,
    velo = mean(effective_speed, na.rm = TRUE),
    vmov_abs = mean(pfx_z * 12, na.rm = TRUE),
    hmov_abs = mean(abs(pfx_x) * 12, na.rm = TRUE),
    mean_arm_angle = mean(arm_angle, na.rm = TRUE),
    mean_rel_height = mean(release_pos_z, na.rm = TRUE),
    RV_100 = mean(delta_pitcher_run_exp, na.rm = TRUE) * 100,
    whiff_pct = 1 - (sum(contact_value, na.rm = TRUE) / sum(swing_value, na.rm = TRUE)),
    babip = sum(babip_value, na.rm = TRUE) / sum(ifelse(foul_value == 0, 1, 0), na.rm = TRUE),
    wOBAcon = sum(ifelse(foul_value == 0, woba_value, NA), na.rm = TRUE) / 
      sum(ifelse(foul_value == 0, woba_denom, NA), na.rm = TRUE),
    xwOBAcon = mean(ifelse(foul_value == 0, estimated_woba_using_speedangle, NA), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(N >= 50) %>%
  mutate(next_year = game_year + 1)

# Create dataset to predict probability of whiff given movement (velo excluded)
si_knn_data <- 
  high_si_data %>% 
  filter(!is.na(contact_value) & arm_angle >= 0) %>%
  mutate(
    vmov = round(pfx_z * 12, 1),
    hmov = round(abs(pfx_x) * 12, 1)
  ) %>%
  select(hmov, vmov, swing_value, contact_value, foul_value, woba_value, woba_denom)

# Train KNN model, predict probabilities for the grid
k <- 200  # Number of neighbors
knn_predictions <- knn(train = si_knn_data[, c("hmov", "vmov")],
                       test = si_knn_data[, c("hmov", "vmov")],
                       cl = si_knn_data$contact_value,
                       k = k,
                       prob = TRUE)

# Extract predicted probabilities
si_knn_data$Probability <- 1 - attr(knn_predictions, "prob")

si_hex_data <- 
  si_knn_data %>%
  mutate(
    HMOV = round(hmov),
    VMOV = round(vmov)
  ) %>%
  group_by(HMOV, VMOV) %>%
  summarise(
    N = n(),
    Probability = mean(Probability)
  ) %>%
  ungroup()

# Create a heatmap for Whiff% by Shape
ggplot(si_hex_data, aes(x = HMOV, y = VMOV, fill = Probability)) +
  geom_hex(stat = "identity") +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = mean(si_knn_data$Probability, na.rm = TRUE), 
    name = "Whiff%"
  ) +
  labs(title = "High Sinker Whiff%, by Horizontal & Vertical Movement",
       x = "Horizontal Movement (in.)",
       y = "Vertical Movement (in.)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(si_hex_data$HMOV), max(si_hex_data$HMOV), by = 5)) +
  scale_y_continuous(breaks = seq(min(si_hex_data$VMOV), max(si_hex_data$VMOV), by = 5)) +
  theme(
    panel.grid = element_blank(), # Remove grid lines for clean look
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # Add black rectangle around the axes
  )

# Group pitches by inches of movement
si_knn_data$HMOV_group <- cut(si_knn_data$hmov, breaks = seq(0, 26, by = 1), include.lowest = TRUE)
si_knn_data$VMOV_group <- cut(si_knn_data$vmov, breaks = seq(-7, 26, by = 1), include.lowest = TRUE)

si_shape_data <- 
  si_knn_data %>%
  group_by(HMOV_group, VMOV_group) %>%
  summarise(
    N = n(),
    whiff_pct = 1 - (sum(contact_value, na.rm = TRUE) / sum(swing_value, na.rm = TRUE)),
    wOBAcon = sum(ifelse(foul_value == 0, woba_value, NA), na.rm = TRUE) / 
      sum(ifelse(foul_value == 0, woba_denom, NA), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(N >= 50)

# 2D scatter plot with size and color gradient
ggplot(si_shape_data, aes(x = HMOV_group, y = VMOV_group)) +
  geom_point(aes(size = N, fill = whiff_pct), shape = 21, color = "black") + 
  scale_size_continuous(range = c(2, 9)) +  # Adjust the dot size
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits = c(0, 0.5),
                       midpoint = 0.18, name = "Whiff%") + # Gradient based on N
  labs(title = "High Sinkers Observed Whiff%, by Horizontal & Vertical Bins", 
       x = "Horizontal Movement Bin", 
       y = "Vertical Movement Bin", 
       size = "Size (N)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 18)
  )

ggplot(si_shape_data, aes(x = HMOV_group, y = VMOV_group)) +
  geom_point(aes(size = N, fill = wOBAcon), shape = 21, color = "black") + 
  scale_size_continuous(range = c(2, 9)) +  # Adjust the dot size
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       midpoint = 0.327, name = "wOBAcon") + # Gradient based on N
  labs(title = "High Sinkers Observed wOBAcon, by Horizontal & Vertical Bins", 
       x = "Horizontal Movement Bin", 
       y = "Vertical Movement Bin", 
       size = "Size (N)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 18)
  )

# Join yearly aggregate data to itself
high_si_year_to_year <-
  merge(
    high_si_grouped %>%
      select(player_name, game_year, RV_100, whiff_pct, babip, wOBAcon, next_year),
    high_si_grouped %>%
      select(player_name, game_year, RV_100, whiff_pct, babip, wOBAcon),
    by.x = c("player_name", "next_year"),
    by.y = c("player_name", "game_year")
) %>%
  rename(
    `RV/100 Year 1` = RV_100.x,
    `RV/100 Year 2` = RV_100.y,
    `Whiff% Year 1` = whiff_pct.x,
    `Whiff% Year 2` = whiff_pct.y,
    `BABIP Year 1` = babip.x, 
    `BABIP Year 2` = babip.y, 
    `wOBAcon Year 1` = wOBAcon.x,
    `wOBAcon Year 2` = wOBAcon.y
  )

# Calculate the correlation matrix for key metrics, met into grid
corr_matrix <- cor(high_si_year_to_year %>% select(-player_name, -next_year, -game_year), use = "complete.obs")
corr_matrix[1:4, 5:8]

corr_melt <- melt(corr_matrix[1:4, 5:8])

# Create the heatmap
ggplot(data = corr_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) + # Adds correlation values
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  labs(title = "Correlation Heatmap: High Sinkers",
       x = "",
       y = "",
       fill = "Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 11),
    plot.title = element_text(hjust = 0.5, size = 18)
  )

ggplot(high_si_grouped, aes(x = velo, y = RV_100)) + 
  geom_point() + 
  ylim(c(-12, 12)) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(method = "pearson", label.x = 82.5, label.y = 10.5, size = 5) +
  labs(
    title = "RV/100 vs. Velocity, High Sinkers 2021-2024",
    x = "Velocity (mph)", y = "Pitch RV/100"
  ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18)
  )

ggplot(high_si_grouped, aes(x = hmov_abs, y = RV_100)) + 
  geom_point() + 
  ylim(c(-12, 12)) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(method = "pearson", label.x = 0, label.y = 10.5, size = 5) +
  labs(
    title = "RV/100 vs. Horizontal Movement, High Sinkers 2021-2024",
    x = "Horizontal Movement (in.)", y = "Pitch RV/100"
  ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18)
  )

ggplot(high_si_grouped, aes(x = vmov_abs, y = RV_100)) + 
  geom_point() + 
  ylim(c(-12, 12)) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(method = "pearson", label.x = -10, label.y = 10.5, size = 5) +
  labs(
    title = "RV/100 vs. Vertical Movement, High Sinkers 2021-2024",
    x = "Vertical Movement (in.)", y = "Pitch RV/100"
  ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18)
  )

# Calculate average whiff% for high four-seams
global_high_ff_whiff_pct <- 
  high_ff_data %>%
  summarise(global_whiff_pct = 1 - (sum(contact_value, na.rm = TRUE) / sum(swing_value, na.rm = TRUE))) %>%
  pull()

# Calculate overall summary metrics, find leaders in Whiff% compared to average FF?
high_si_overall <- 
  high_si_data %>%
  group_by(pitcher, player_name, p_throws) %>%
  summarise(
    N = n(),
    Shadow_Zone_pct = sum(IsShadow, na.rm = TRUE) / N,
    velo = mean(effective_speed, na.rm = TRUE),
    vmov_abs = mean(pfx_z * 12, na.rm = TRUE),
    hmov_abs = mean(abs(pfx_x * 12), na.rm = TRUE),
    mean_arm_angle = mean(arm_angle, na.rm = TRUE),
    mean_rel_height = mean(release_pos_z, na.rm = TRUE),
    RV_100 = mean(delta_pitcher_run_exp, na.rm = TRUE) * 100,
    whiff_pct = 1 - (sum(contact_value, na.rm = TRUE) / sum(swing_value, na.rm = TRUE)),
    whiff_pct_above_ff = whiff_pct - global_high_ff_whiff_pct,
    ev = mean(launch_speed, na.rm = TRUE),
    babip = sum(babip_value, na.rm = TRUE) / sum(ifelse(foul_value == 0, 1, 0), na.rm = TRUE),
    wOBAcon = sum(ifelse(foul_value == 0, woba_value, NA), na.rm = TRUE) / 
      sum(ifelse(foul_value == 0, woba_denom, NA), na.rm = TRUE),
    xwOBAcon = mean(ifelse(foul_value == 0, estimated_woba_using_speedangle, NA), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(N >= 50)

ggplot(high_si_overall, aes(x = whiff_pct_above_ff)) + 
  geom_histogram(fill = "blue") +
  labs(
    title = "Whiff% over Average Four-Seam Whiff%, High Sinkers 2021-2024",
    x = "Whiff% - Average FF Whiff%", y = "Count"
  ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18)
  )

## Aside: Steven Matz
# Calcuate average Whiff rate for neighborhood of sinkers (Matz)
high_si_overall %>%
  filter(pitcher == 571927) %>%
  select(whiff_pct, whiff_pct_above_ff)

si_knn_data %>% 
  filter((hmov >= 13 & hmov <= 17) & (vmov >= 10 & vmov <= 14)) %>%
  summarise(Whiff_pct = 1 - (sum(contact_value, na.rm = TRUE) / sum(swing_value, na.rm = TRUE))) %>%
  pull()

# High Sinker Whiff Rate very high, slightly above the average four-seamer
ggplot(high_si_overall %>%
         mutate(col = ifelse(pitcher == 571927, 'red', 'black')), 
       aes(x = velo, y = whiff_pct)) + 
  geom_point(aes(colour = col), 
             size = ifelse(high_si_overall$pitcher == 571927, 2.5, 1.5)) + 
  scale_color_identity() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Whiff% vs. Velocity, High Sinkers 2021-2024",
    x = "Velocity (mph)", y = "Whiff%"
  ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18)
  )

# Check: Above average-vertical break (for all sinkers)
si_overall <- 
  si_data %>%
  group_by(pitcher, player_name, p_throws) %>%
  summarise(
    N = n(),
    velo = mean(effective_speed, na.rm = TRUE),
    vmov_abs = mean(pfx_z * 12, na.rm = TRUE),
    hmov_abs = mean(abs(pfx_x * 12), na.rm = TRUE),
    mean_arm_angle = mean(arm_angle, na.rm = TRUE),
    mean_rel_height = mean(release_pos_z, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(N >= 50 & vmov_abs >= 0 & mean_rel_height >= 4)

ggplot(si_overall %>%
         mutate(col = ifelse(pitcher == 571927, 'red', 'black')), 
       aes(x = mean_rel_height, y = vmov_abs)) +
  geom_point(aes(colour = col), 
             size = ifelse(si_overall$pitcher == 571927, 2.5, 1.5)) +
  scale_color_identity() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Vertical Movement vs. Release Height, High Sinkers 2021-2024",
    y = "Vertical Movement (in.)", x = "Release Height (ft.)"
  ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18)
  )

  
## Location  ===========================================================================================================

# Cut locations into 1/4 inch by 1/4 inch squares
high_si_data$plate_x_group <- cut(high_si_data$plate_x, breaks = seq(-5, 5, by = 0.25), include.lowest = TRUE)
high_si_data$plate_z_group <- cut(high_si_data$plate_z, breaks = seq(-5, 5, by = 0.25), include.lowest = TRUE)

high_ff_data$plate_x_group <- cut(high_ff_data$plate_x, breaks = seq(-5, 5, by = 0.25), include.lowest = TRUE)
high_ff_data$plate_z_group <- cut(high_ff_data$plate_z, breaks = seq(-5, 5, by = 0.25), include.lowest = TRUE)

# Calculate summary stats by location bin
high_si_loc <- high_si_data %>%
  filter(abs(plate_x) < 1.25 & plate_z < 4) %>%
  group_by(plate_x_group, plate_z_group) %>%
  summarise(
    N = n(),
    RV_100 = round(mean(delta_pitcher_run_exp, na.rm = TRUE), 3)*100,
    whiff_pct = 1 - (sum(contact_value, na.rm = TRUE) / sum(swing_value, na.rm = TRUE)),
    wOBAcon = sum(ifelse(foul_value == 0, woba_value, NA), na.rm = TRUE) / 
      sum(ifelse(foul_value == 0, woba_denom, NA), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(N > 75) %>%
  mutate(Freq = N / sum(N))

high_ff_loc <- high_ff_data %>%
  filter(abs(plate_x) < 1.25 & plate_z < 4) %>%
  group_by(plate_x_group, plate_z_group) %>%
  summarise(
    N = n(),
    RV_100 = round(mean(delta_pitcher_run_exp, na.rm = TRUE), 3)*100,
    whiff_pct = 1 - (sum(contact_value, na.rm = TRUE) / sum(swing_value, na.rm = TRUE)),
    wOBAcon = sum(ifelse(foul_value == 0, woba_value, NA), na.rm = TRUE) / 
      sum(ifelse(foul_value == 0, woba_denom, NA), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(N > 75) %>%
  mutate(Freq = N / sum(N))

# Create heatmaps per metric, pitch type
ggplot(high_si_loc, aes(x = plate_x_group, y = plate_z_group, fill = RV_100)) +
  geom_bin2d() + 
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0.0, limits = c(-6, 5), 
    name = "RV_100"
  ) +
  labs(title = "High Sinkers, RV/100 by Location Bin", 
       x = "Horiztontal Location (ft.)", y = "Vertical Location (ft.)") +
  geom_hline(yintercept = 6) + 
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(high_ff_loc, aes(x = plate_x_group, y = plate_z_group, fill = RV_100)) +
  geom_bin2d() + 
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0.0, limits = c(-6, 5), 
    name = "RV_100"
  ) +
  labs(title = "High Four-Seams, RV/100 by Location Bin", 
       x = "Horiztontal Location (ft.)", y = "Vertical Location (ft.)") +
  geom_hline(yintercept = 6) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(high_si_loc, aes(x = plate_x_group, y = plate_z_group, fill = Freq)) +
  geom_bin2d() + 
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", limits = c(0, 0.05),
    name = "Frequency"
  ) +
  labs(title = "High Sinkers, Frequency by Location Bin", 
       x = "Horiztontal Location (ft.)", y = "Vertical Location (ft.)") +
  geom_hline(yintercept = 6) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(high_ff_loc, aes(x = plate_x_group, y = plate_z_group, fill = Freq)) +
  geom_bin2d() + 
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", limits = c(0, 0.05),
    name = "Frequency"
  ) +
  labs(title = "High Four-Seams, Frequency by Location Bin", 
       x = "Horiztontal Location (ft.)", y = "Vertical Location (ft.)") +
  geom_hline(yintercept = 6) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(high_si_loc, aes(x = plate_x_group, y = plate_z_group, fill = whiff_pct)) +
  geom_bin2d() + 
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", limits = c(0, 0.55), midpoint = 0.2,
    name = "Whiff%"
  ) +
  labs(title = "High Sinkers, Whiff% by Location Bin", 
       x = "Horiztontal Location (ft.)", y = "Vertical Location (ft.)") +
  geom_hline(yintercept = 6) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(high_ff_loc, aes(x = plate_x_group, y = plate_z_group, fill = whiff_pct)) +
  geom_bin2d() + 
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", limits = c(0, 0.55), midpoint = 0.2,
    name = "Whiff%"
  ) +
  labs(title = "High Four-Seams, Whiff% by Location Bin", 
       x = "Horiztontal Location (ft.)", y = "Vertical Location (ft.)") +
  geom_hline(yintercept = 6) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Filter to the horizontal outer shadows of the zone: high SI make their $$$ relative to FF
# Not just about locating a pitch well, but to its strengths (FF, high and slightly out of the zone, SI inside)
bind_rows(high_si_data, high_ff_data) %>%
  filter(abs(plate_x) > 0.8333333) %>%
  group_by(pitch_type) %>%
  summarise(
    N = n(),
    RV_100 = mean(delta_pitcher_run_exp, na.rm = TRUE) * 100,
    wOBAcon = sum(ifelse(foul_value == 0, woba_value, NA), na.rm = TRUE) / 
      sum(ifelse(foul_value == 0, woba_denom, NA), na.rm = TRUE),
  )

# High FF perform better on swings (holds for fastballs in-the-zone)
bind_rows(high_si_data, high_ff_data) %>%
  filter(swing_value == 1 & abs(plate_x) <= 0.8333333 & plate_z <= 3.5) %>%
  group_by(pitch_type) %>%
  summarise(
    N = n(),
    RV_100 = mean(delta_pitcher_run_exp, na.rm = TRUE) * 100,
    wOBAcon = sum(ifelse(foul_value == 0, woba_value, NA), na.rm = TRUE) / 
      sum(ifelse(foul_value == 0, woba_denom, NA), na.rm = TRUE),
  )

# Calculate percent of high sinkers in the "inside shadow zone"
zone_plus_shadow_si <- 
  high_si_data %>%
  mutate(
    In_Zone_Shadow = ifelse(
      IsShadow == 1 & abs(plate_x) <= 0.8333333 & plate_x <= 3.5, 1, 0
    )
  ) %>%
  group_by(pitcher, player_name) %>%
  summarise(
    N = n(),
    Zone_Shadow = (sum(In_Zone_Shadow) / N)*100,
    RV_100 = round(mean(delta_pitcher_run_exp, na.rm = TRUE), 3)*100
  ) %>%
  ungroup() %>%
  filter(N >= 200)

# Who can throw in the zone and in the Shadow zone?
zone_plus_shadow_si %>% 
  arrange(desc(Zone_Shadow)) %>%
  head(10)

# Create new same/oppo handedness indicator column
high_si_data <- 
  high_si_data %>%
  mutate(same_hand = ifelse(p_throws == stand, "Yes", "No"))

high_si_same_hand <- 
  high_si_data %>%
  filter(same_hand == "Yes") %>%
  group_by(plate_x_group, plate_z_group) %>%
  summarise(
    N = n(),
    RV_100 = round(mean(delta_pitcher_run_exp, na.rm = TRUE), 3)*100
  ) %>%
  ungroup() %>%
  filter(N > 75) %>%
  mutate(Freq = N / sum(N))

high_si_oppo_hand <- 
  high_si_data %>%
  filter(same_hand == "No") %>%
  group_by(plate_x_group, plate_z_group) %>%
  summarise(
    N = n(),
    RV_100 = round(mean(delta_pitcher_run_exp, na.rm = TRUE), 3)*100
  ) %>%
  ungroup() %>%
  filter(N > 75) %>%
  mutate(Freq = N / sum(N))

# Plot location to same/oppo handed high sinkers
ggplot(high_si_same_hand, aes(x = plate_x_group, y = plate_z_group, fill = Freq)) +
  geom_bin2d() + 
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", limits = c(0, 0.05),
    name = "Frequency"
  ) +
  labs(title = "High Sinkers vs. Same-Handed Batters, Frequency by Location Bin", 
       x = "Horiztontal Location (ft.)", y = "Vertical Location (ft.)") +
  geom_hline(yintercept = 6) + 
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(high_si_oppo_hand, aes(x = plate_x_group, y = plate_z_group, fill = Freq)) +
  geom_bin2d() + 
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", limits = c(0, 0.05),
    name = "Frequency"
  )  +
  labs(title = "High Sinkers vs. Opposite-Handed Batters, Frequency by Location Bin", 
       x = "Horiztontal Location (ft.)", y = "Vertical Location (ft.)") +
  geom_hline(yintercept = 6) + 
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
