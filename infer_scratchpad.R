library(nycflights13)
library(dplyr)
library(stringr)
library(infer)

# https://github.com/topepo/infer
# https://github.com/topepo/infer/tree/master/vignettes
# https://github.com/topepo/infer/blob/master/vignettes/two_sample_t.Rmd
# https://thomasleeper.com/Rcourse/Tutorials/permutationtests.html
# https://ismayc.github.io/talks/data-day-texas-infer/slide_deck.html#53

set.seed(2017)
fli_small <- flights %>% 
        sample_n(size = 500) %>% 
        mutate(half_year = case_when(
                between(month, 1, 6) ~ "h1",
                between(month, 7, 12) ~ "h2"
        )) %>% 
        mutate(day_hour = case_when(
                between(hour, 1, 12) ~ "morning",
                between(hour, 13, 24) ~ "not morning"
        )) %>% 
        select(arr_delay, dep_delay, half_year, 
               day_hour, origin, carrier)
fli_small

# inspect data
glimpse(fli_small)
fli_small %>% group_by(half_year) %>% summarize(count = n(), mean = mean(arr_delay, na.rm = TRUE))


####################


# base t.test
h1_arr_delay <- fli_small %>% filter(half_year == "h1") %>% pull(arr_delay)
summary(h1_arr_delay)
length(h1_arr_delay)

h2_arr_delay <- fli_small %>% filter(half_year == "h2") %>% pull(arr_delay)
summary(h2_arr_delay)
length(h2_arr_delay)

t.test(x = h1_arr_delay, y = h2_arr_delay)


#################


# t_test with infer
# note t_test() gives more info like t_stat, t_df, p_value, alternative, and ci
# calculate() below only gives t_stat
t_test_results <- fli_small %>% 
        t_test(formula = arr_delay ~ half_year, alternative = "two_sided", order = c("h1", "h2"))
t_test_results

# note that reversing order of explanatory variable in order arg just changes whether diff in mean is h1-h2 or h2-h1
fli_small %>% t_test(formula = arr_delay ~ half_year, alternative = "two_sided", order = c("h2", "h1"))

# get observed_t_score
observed_t_score <- t_test_results %>% pull(statistic)
observed_t_score


###############


# just to show function, calculate t stat using specify() and calculate()
# calculate() just gives t_stat, where t_test() output is more informative
# these verbs are used when using randomization methods like bootstrap or permute (see below)
fli_small %>% specify(arr_delay ~ half_year) %>% calculate(stat = "t", order = c("h1", "h2"))


##################


# calculate t_test using bootstrap
t_distro_under_null_hypoth <- fli_small %>% specify(arr_delay ~ half_year) %>% hypothesize(null = "independence") %>%
        generate(reps = 1000, type = "permute") %>% calculate(stat = "t", order = c("h1", "h2"))
t_distro_under_null_hypoth

# visualize distribution
t_distro_under_null_hypoth %>% visualize()

t_distro_under_null_hypoth %>% visualize(obs_stat = observed_t_score, direction = "two_sided")

# get p_value of observed_t_score given t_distro_under_null_hypoth
t_distro_under_null_hypoth %>% get_pvalue(obs_stat = observed_t_score, direction = "two_sided")


################


# visulalize theoretical distribution (not using generate() for randomization methods like bootstrap or permute)
fli_small %>%
        # alt: response = arr_delay, explanatory = half_year
        specify(arr_delay ~ half_year) %>%
        hypothesize(null = "independence") %>%
        # generate() ## Not used for theoretical
        calculate(stat = "t", order = c("h1", "h2")) %>%
        visualize(method = "theoretical", obs_stat = observed_t_score, direction = "two_sided")


#####################


# can visualize both the randomization method distribution and the theoretical distribution
fli_small %>%
        # alt: response = arr_delay, explanatory = half_year
        specify(arr_delay ~ half_year) %>%
        hypothesize(null = "independence") %>%
        generate(reps = 1000, type = "permute") %>%
        calculate(stat = "t", order = c("h1", "h2")) %>% 
        visualize(method = "both", obs_stat = observed_t_score, direction = "two_sided")


########################


# compare pvalues from randomization and theoretical distributions
t_test_results %>% select(p_value)
t_distro_under_null_hypoth %>% get_pvalue(obs_stat = observed_t_score, direction = "two_sided")

