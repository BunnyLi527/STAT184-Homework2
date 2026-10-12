library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(forcats)
library(purrr)
sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

sales_raw
# Problem 1 Part A: Text and numerical transformations

sales_clean <- sales_raw %>%
  mutate(
    item_clean = str_to_lower(str_squish(item_raw)),
    amount = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw) / 100,
    net_amount = amount * (1 - discount_rate),
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20 ~ "large",
      TRUE ~ "small"
    )
  )

sales_clean %>%
  select(
    sale_id,
    item_clean,
    amount,
    discount_rate,
    net_amount,
    size
  )
# Part A output:
# sale_id 1: pen, amount 10, discount_rate 0, net_amount 10, size small
# sale_id 2: pen, amount 20, discount_rate 0.1, net_amount 18, size small
# sale_id 3: notebook, amount NA, discount_rate 0.05, net_amount NA, size unrecorded
# sale_id 4: pen, amount 15, discount_rate 0, net_amount 15, size small
# sale_id 5: folder, amount 30, discount_rate 0.2, net_amount 24, size large
# sale_id 6: folder, amount 25, discount_rate 0.1, net_amount 22.5, size large
# sale_id 7: eraser, amount NA, discount_rate 0, net_amount NA, size unrecorded
# sale_id 8: pen, amount 20, discount_rate 0, net_amount 20, size large

# Problem 1 Part B: Validate codes and split strings

sales_codes <- sales_clean %>%
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(code_clean, "^(ST|ON)-\\d{3}$")
  )

invalid_codes <- sales_codes %>%
  filter(!valid_code) %>%
  select(sale_id, code_raw, code_clean)

invalid_codes
# invalid_codes contains sale_id 5 (BAD) and sale_id 7 (ON-107X).
valid_code_parts <- sales_codes %>%
  filter(valid_code) %>%
  select(sale_id, code_clean) %>%
  separate_wider_delim(
    code_clean,
    delim = "-",
    names = c("channel", "number"),
    cols_remove = FALSE
  ) %>%
  select(sale_id, code_clean, channel, number)

valid_code_parts
# valid_code_parts has 6 rows: ST-101, ON-102, ST-103, ON-104, ST-106, and ON-108.
sale_tags <- sales_clean %>%
  select(sale_id, tags) %>%
  separate_longer_delim(
    tags,
    delim = ";"
  )

sale_tags
# sale_tags has 11 rows after splitting tags.
# separate_wider_delim() splits one column into multiple columns, while separate_longer_delim() splits one value into multiple rows.
# Problem 1 Part C: Grouped proportions and factor categories

customer_summary <- sales_clean %>%
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(net_amount >= 20, na.rm = TRUE),
    large_share = if_else(
      known_n == 0,
      NA_real_,
      large_n / known_n
    ),
    .by = customer_id
  ) %>%
  arrange(customer_id)

customer_summary
# For C1, the numerator is 0 large known sales and the denominator is 1 known net amount, so large_share = 0 / 1 = 0.
# C1: sales_n = 2, known_n = 1, large_n = 0, large_share = 0.
# C2: sales_n = 3, known_n = 3, large_n = 2, large_share = 2/3.
# C5 has no known net amount, so large_share = NA.
sales_factors <- sales_clean %>%
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group = fct_lump_n(
      factor(item_clean),
      n = 2,
      other_level = "Other"
    )
  )

# Levels and counts for item_factor
levels(sales_factors$item_factor)
sales_factors %>%
  count(item_factor)

# Levels and counts for item_group
levels(sales_factors$item_group)
sales_factors %>%
  count(item_group)
# item_factor levels: pen, folder, eraser, notebook.
# item_factor counts: pen = 4, folder = 2, eraser = 1, notebook = 1.
# item_group levels: folder, pen, Other.
# item_group counts: folder = 2, pen = 4, Other = 2.
# fct_infreq() changes the order of factor levels without combining categories.
# fct_lump_n() combines less frequent categories into "Other".
# Problem 2: Missing Values and Joins

reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)

customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

reports_raw
customers
reports_labeled <- reports_raw %>%
  arrange(form, line) %>%
  group_by(form) %>%
  fill(customer_id, .direction = "down") %>%
  ungroup()

reports_labeled
reports_clean <- reports_labeled %>%
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )

reports_clean %>%
  select(
    form,
    line,
    customer_id,
    round,
    primary_amount,
    backup_amount,
    amount
  )
# A recorded primary value, including zero, is kept because backup_amount is used only when the primary value is missing.
reports_complete <- reports_clean %>%
  select(customer_id, round, amount) %>%
  mutate(recorded_row = TRUE) %>%
  complete(
    customer_id,
    round = 1:3,
    fill = list(recorded_row = FALSE)
  ) %>%
  arrange(customer_id, round)

reports_complete
original_rows <- sum(reports_complete$recorded_row)
added_rows <- sum(!reports_complete$recorded_row)

missing_before <- sum(is.na(reports_clean$amount))
missing_after <- sum(is.na(reports_complete$amount))

original_rows
added_rows
missing_before
missing_after
# An explicit missing amount is C3 in round 1, which has a recorded row but amount = NA.
# An implicit missing row is C1 in round 2, because that customer-round combination was absent before complete().
# original_rows = 6, added_rows = 6, missing_before = 1, missing_after = 7.

# Problem 2 Part B: Join and check

# Check customers IDs
customers %>%
  summarise(
    duplicate_ids = any(duplicated(customer_id)),
    missing_ids = any(is.na(customer_id))
  )
# customers has no duplicate or missing customer IDs.

# Check duplicate customer_id-round combinations
reports_clean %>%
  count(customer_id, round) %>%
  filter(n > 1)
customers_marked <- customers %>%
  mutate(customer_found = TRUE)
# reports_clean has no duplicated customer_id-round combinations.

reports_joined <- reports_clean %>%
  left_join(
    customers_marked,
    by = join_by(customer_id),
    relationship = "many-to-one",
    na_matches = "never"
  ) %>%
  mutate(
    customer_found = coalesce(customer_found, FALSE)
  )

reports_joined %>%
  select(
    customer_id,
    round,
    amount,
    region,
    customer_found
  )
reports_joined %>%
  select(
    customer_id,
    round,
    amount,
    region,
    customer_found
  )
# reports_joined:
# C1 round 1: amount 10, North, TRUE
# C1 round 3: amount 15, North, TRUE
# C2 round 1: amount 20, South, TRUE
# C3 round 1: amount NA, region NA, TRUE
# C3 round 2: amount 0, region NA, TRUE
# C4 round 1: amount 30, region NA, FALSE
# IDs found only in reports_clean
reports_only <- reports_clean %>%
  distinct(customer_id) %>%
  anti_join(
    customers %>% distinct(customer_id),
    by = join_by(customer_id),
    na_matches = "never"
  )

reports_only

# IDs found only in customers
customers_only <- customers %>%
  distinct(customer_id) %>%
  anti_join(
    reports_clean %>% distinct(customer_id),
    by = join_by(customer_id),
    na_matches = "never"
  )

customers_only
# reports_only contains C4, while customers_only contains C5.
# C3 has customer_found = TRUE, so the customer exists in customers even though its region is missing.
# C4 has customer_found = FALSE, so its missing region is caused by no matching customer record, not just a missing region value.
# Problem 2 Part C: Check and reshape reports

customers_bad <- bind_rows(
  customers,
  tibble(customer_id = "C2", region = "East")
)

reports_duplicated <- reports_clean %>%
  left_join(
    customers_bad,
    by = join_by(customer_id),
    relationship = "many-to-many"
  )

# Compare row counts
rows_before <- nrow(reports_clean)
rows_after <- nrow(reports_duplicated)

# Compare total amount
total_before <- sum(reports_clean$amount, na.rm = TRUE)
total_after <- sum(reports_duplicated$amount, na.rm = TRUE)

rows_before
rows_after
total_before
total_after
# rows_before = 6, rows_after = 7, total_before = 75, total_after = 95.

# Identify repeated report
reports_duplicated %>%
  count(customer_id, round) %>%
  filter(n > 1)
# The repeated report is C2, round 1.
# The row count increases from 6 to 7 because the C2 round 1 report matches two C2 rows in customers_bad.
# The total amount increases from 75 to 95 because the amount 20 for C2 round 1 is counted twice after the join.

# Using relationship = "many-to-one" would produce an error because C2 has more than one matching row in customers_bad.
# Allowing "many-to-many" does not fix the conflicting customer information; it only permits the join to continue and duplicate matching report rows.

# Part C: Reshape reports

reports_wide <- reports_complete %>%
  select(customer_id, round, amount) %>%
  pivot_wider(
    names_from = round,
    values_from = amount,
    names_prefix = "round_"
  )

reports_wide
# reports_wide has 4 rows:
# C1: round_1 = 10, round_2 = NA, round_3 = 15
# C2: round_1 = 20, round_2 = NA, round_3 = NA
# C3: round_1 = NA, round_2 = 0, round_3 = NA
# C4: round_1 = 30, round_2 = NA, round_3 = NA

# Pivoting back to long form with values_drop_na = TRUE would leave 5 rows because only 5 amounts are non-missing.
# Dropping missing values does not recover unknown amounts; it only removes rows whose amount is NA.

# Problem 3: Two-sample t-tests and simulation
# Part A: Write a t-test function

two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  
  nx <- length(x)
  ny <- length(y)
  
  mean_x <- mean(x)
  mean_y <- mean(y)
  
  var_x <- var(x)
  var_y <- var(y)
  
  if (equal_var) {
    
    pooled_var <- ((nx - 1) * var_x + (ny - 1) * var_y) /
      (nx + ny - 2)
    
    se <- sqrt(pooled_var * (1 / nx + 1 / ny))
    
    df <- nx + ny - 2
    
    method <- "Pooled"
    
  } else {
    
    se <- sqrt(var_x / nx + var_y / ny)
    
    df <- (var_x / nx + var_y / ny)^2 /
      (
        (var_x / nx)^2 / (nx - 1) +
          (var_y / ny)^2 / (ny - 1)
      )
    
    method <- "Welch"
  }
  
  t_stat <- (mean_x - mean_y) / se
  
  p_value <- 2 * pt(
    abs(t_stat),
    df = df,
    lower.tail = FALSE
  )
  
  reject <- p_value < alpha
  
  list(
    method = method,
    t_stat = t_stat,
    df = df,
    p_value = p_value,
    reject = reject
  )
}
# Test data

x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

welch_result <- two_sample_t(x_check, y_check)

pooled_result <- two_sample_t(
  x_check,
  y_check,
  equal_var = TRUE
)

welch_result
pooled_result
# Welch result: t_stat = 3.403072, df = 7.678614, p_value = 0.009907, reject = TRUE.
# Pooled result: t_stat = 3.526436, df = 11, p_value = 0.004744, reject = TRUE.

# Verify with t.test()

welch_ref <- t.test(
  x_check,
  y_check,
  var.equal = FALSE
)

pooled_ref <- t.test(
  x_check,
  y_check,
  var.equal = TRUE
)

welch_ref
pooled_ref
welch_check <- c(
  t_stat = abs(welch_result$t_stat - unname(welch_ref$statistic)) < 1e-8,
  df = abs(welch_result$df - unname(welch_ref$parameter)) < 1e-8,
  p_value = abs(welch_result$p_value - welch_ref$p.value) < 1e-8
)

pooled_check <- c(
  t_stat = abs(pooled_result$t_stat - unname(pooled_ref$statistic)) < 1e-8,
  df = abs(pooled_result$df - unname(pooled_ref$parameter)) < 1e-8,
  p_value = abs(pooled_result$p_value - pooled_ref$p.value) < 1e-8
)

welch_check
pooled_check
# All differences in t_stat, df, and p_value are smaller than 1e-8 for both the Welch and pooled tests.

# Part B: Perform simulation loop

B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null",
               "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)

# Allocate storage in advance
total_rows <- nrow(scenarios) * B * 2

simulation_results <- data.frame(
  scenario = rep(NA_character_, total_rows),
  replicate = rep(NA_integer_, total_rows),
  method = rep(NA_character_, total_rows),
  t_stat = rep(NA_real_, total_rows),
  df = rep(NA_real_, total_rows),
  p_value = rep(NA_real_, total_rows),
  reject = rep(NA, total_rows)
)

# Set seed once before the loops
set.seed(2026)

row_i <- 1

for (s in seq_len(nrow(scenarios))) {
  
  for (b in seq_len(B)) {
    
    x_sim <- rnorm(
      n_x,
      mean = scenarios$mean_x[s],
      sd = scenarios$sd_x[s]
    )
    
    y_sim <- rnorm(
      n_y,
      mean = scenarios$mean_y[s],
      sd = scenarios$sd_y[s]
    )
    
    welch_sim <- two_sample_t(
      x_sim,
      y_sim,
      equal_var = FALSE,
      alpha = alpha
    )
    
    pooled_sim <- two_sample_t(
      x_sim,
      y_sim,
      equal_var = TRUE,
      alpha = alpha
    )
    
    simulation_results[row_i, ] <- list(
      scenarios$scenario[s],
      b,
      welch_sim$method,
      welch_sim$t_stat,
      welch_sim$df,
      welch_sim$p_value,
      welch_sim$reject
    )
    
    simulation_results[row_i + 1, ] <- list(
      scenarios$scenario[s],
      b,
      pooled_sim$method,
      pooled_sim$t_stat,
      pooled_sim$df,
      pooled_sim$p_value,
      pooled_sim$reject
    )
    
    row_i <- row_i + 2
  }
}

head(simulation_results)
nrow(simulation_results)
# simulation_results contains 8000 rows.

# Summarize simulation results

simulation_summary <- simulation_results %>%
  group_by(scenario, method) %>%
  summarise(
    mean_t = mean(t_stat),
    sd_t = sd(t_stat),
    rejection_rate = mean(reject),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(mean_t, sd_t, rejection_rate),
      ~ round(.x, 3)
    )
  )

simulation_summary
# simulation_summary:
# equal_null: Pooled (-0.018, 1.020, 0.038); Welch (-0.019, 1.040, 0.044)
# equal_shift: Pooled (-3.760, 1.080, 0.957); Welch (-3.770, 1.120, 0.956)
# unequal_null: Pooled (0.048, 1.400, 0.139); Welch (0.036, 1.080, 0.061)
# unequal_shift: Pooled (-2.020, 1.360, 0.497); Welch (-1.540, 1.050, 0.282)
# Values above are mean_t, sd_t, and rejection_rate.

# Under equal variances and the null hypothesis, both tests have rejection rates close to 0.05 (Pooled = 0.038, Welch = 0.044).
# Under unequal variances and the null, Welch remains fairly close to 0.05 at 0.061, while the pooled test has an inflated Type I error rate of 0.139.
# When the means differ, both tests have high power under equal variances, while under unequal variances the pooled test has higher rejection rate (0.497) than Welch (0.282).
# However, higher power alone does not make a test better because the pooled test also rejects too often when the null is true; small differences from expected rates can occur because of simulation variation.