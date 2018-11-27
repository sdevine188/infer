library(nycflights13)
library(dplyr)
library(stringr)
library(infer)

# https://github.com/topepo/infer
# https://github.com/topepo/infer/tree/master/vignettes
# https://github.com/topepo/infer/blob/master/vignettes/two_sample_t.Rmd
# https://thomasleeper.com/Rcourse/Tutorials/permutationtests.html
# https://ismayc.github.io/talks/data-day-texas-infer/slide_deck.html#53
# https://stats.stackexchange.com/questions/20217/bootstrap-vs-permutation-hypotheis-testing
# https://www.rdocumentation.org/packages/infer/versions/0.3.1/topics/conf_int


# note that stackexchange said permutation is more commonly used for tests, whereas bootstrap is more commonly used to get conf_int
# https://stats.stackexchange.com/questions/20217/bootstrap-vs-permutation-hypotheis-testing


# note that hypothesis tests below use "diff in means", but can also swap out for conduct "diff in prop" if dependent variable is a dummy


######################################################################################################################

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

# plot arr_delay for both levels of half_year
# notice the distribution of dependent variable is a bit skewed instead of theoretic normality, though variance may be approx similar
# theoretical t test is cited as reasonably robust to violations of assumptions, 
# but the farther you get from assumptions, the more randomized tests/conf_int can offer improvement over theoretical methods
fli_small %>% ggplot(data = ., aes(x = arr_delay)) + geom_histogram() + facet_grid(rows = vars(half_year))
fli_small %>% ggplot(data = ., aes(x = arr_delay)) + geom_density() + facet_grid(rows = vars(half_year))


####################


# using theory, get conf_int for diff_in_means
h1_arr_delay <- fli_small %>% filter(half_year == "h1") %>% pull(arr_delay)
summary(h1_arr_delay)
length(h1_arr_delay)

h2_arr_delay <- fli_small %>% filter(half_year == "h2") %>% pull(arr_delay)
summary(h2_arr_delay)
length(h2_arr_delay)

# note that t.test assumes theoretical normal distribution of dependent variable, and equal variances, for x and y 
# http://www.csic.cornell.edu/Elrod/t-test/t-test-assumptions.html
# https://statistics.laerd.com/statistical-guides/independent-t-test-statistical-guide.php

# standard error is std_err = sqrt[ (std_dev1^2/n1) + (std_dev2^2/n2) ]
# https://stattrek.com/hypothesis-test/difference-in-means.aspx
h1_arr_delay_sd <- sd(h1_arr_delay, na.rm = TRUE)
h1_arr_delay_n <- length(h1_arr_delay[!is.na(h1_arr_delay)])

h2_arr_delay_sd <- sd(h2_arr_delay, na.rm = TRUE)
h2_arr_delay_n <- length(h2_arr_delay[!is.na(h1_arr_delay)])

std_error <- sqrt( (h1_arr_delay_sd^2 / h1_arr_delay_n) + (h2_arr_delay_sd^2 / h2_arr_delay_n) )
std_error

# conf int is Lower Limit = mean1 - mean2 - (t_stat_for_chosen_conf_level * std_error)
# http://onlinestatbook.com/2/estimation/difference_means.html
h1_arr_delay_mean <- mean(h1_arr_delay, na.rm = TRUE)
h2_arr_delay_mean <- mean(h2_arr_delay, na.rm = TRUE)
mean_diff <- h1_arr_delay_mean - h2_arr_delay_mean
mean_diff

mean_diff - 1.96 * std_error
mean_diff + 1.96 * std_error


####################


# run theoretical t.test
output <- t.test(x = h1_arr_delay, y = h2_arr_delay) %>% tidy()
output
output %>% summarize(std_error = (conf.high - estimate) / 1.96)


#################


# get conf_int for diff_in_means with boostrap method
observed_diff_in_means <- fli_small %>% specify(arr_delay ~ half_year) %>% calculate(stat = "diff in means", order = c("h1", "h2"))
observed_diff_in_means

# get bootstrap_distn for diff in means
# bootstrap just takes resamples of data with replacement (same number of observations), then calculates the diff_in_means on these replicates
# the collection of these diff_in_means across replicates creates diff_in_means sampling distro
diff_in_means_bootstrap_dist <- fli_small %>% specify(arr_delay ~ half_year) %>% generate(reps = 100, type = "bootstrap") %>%
        calculate(stat = "diff in means", order = c("h1", "h2"))
diff_in_means_bootstrap_dist

# visualize diff_in_means_bootstrap_dist
diff_in_means_bootstrap_dist %>% visualize()

# get conf_int for diff_in_means
# conf_int using randomized bootstrap diff_in_means distro method
diff_in_means_bootstrap_dist %>% conf_int(level = 0.95)

# conf_int using theoretical 1.96 * std_error method
# std_error is measure of precision of estimate, calculated as the standard deviation of the estimate's sampling distribution
diff_in_means_bootstrap_dist %>% summarize(se = sd(stat), conf_int_lower = d_hat$stat - 1.96*se, conf_int_upper = d_hat$stat + 1.96*se)
diff_in_means_bootstrap_dist %>% conf_int(type = "se", point_estimate = d_hat)


##################


# conduct same theoretical t_test with infer
# note t_test() gives more info like t_stat, t_df, p_value, alternative, and ci - though not the diff_in_means or std_error
# to get actual t_stat instead, use calculate(stat = "t") as shown below 
t_test_results <- fli_small %>% 
        t_test(formula = arr_delay ~ half_year, alternative = "two_sided", order = c("h1", "h2"))
t_test_results

# add std_error
t_test_results %>% mutate(diff_in_means = observed_diff_in_means$stat, std_error = (upper_ci - diff_in_means) / 1.96)

# note that reversing order of explanatory variable in order arg just changes whether diff in mean is h1-h2 or h2-h1
fli_small %>% t_test(formula = arr_delay ~ half_year, alternative = "two_sided", order = c("h2", "h1"))

# get observed_t_score for use later in visualizing how extreme observed_t_score for diff in means is based on sampling distribution of no diff in means
observed_t_score <- t_test_results %>% pull(statistic)
observed_t_score

# visulalize theoretical t_distribution (not using generate() for randomization permute methods)
fli_small %>%
        specify(arr_delay ~ half_year) %>%
        hypothesize(null = "independence") %>%
        # generate() ## Not used for theoretical
        calculate(stat = "t", order = c("h1", "h2")) %>%
        visualize(method = "theoretical", obs_stat = observed_t_score, direction = "two_sided")


###############


# just to show function, calculate t stat using specify() and calculate()
# calculate() just gives t_stat, where t_test() output is more informative
# these verbs (specify and calculate) are used when using randomization methods like bootstrap or permute (see below)
fli_small %>% specify(arr_delay ~ half_year) %>% calculate(stat = "t", order = c("h1", "h2"))


##################


# use permute to calculate distribution of t_stats under the null hypothesis that there's no diff in means
# infer uses random permutation to get t_stat distro, whereas theory uses theoretical t_stat distro based on assumptions

# note that when using permute to randomly sample, we need to use hypothesize(), but when using bootstrap (see below) we don't need hypothesize()
# basically it creates new replicates where the dependent variable assigned to each independent variable is randomly shuffled
# this can be done exhaustively, or if computational limits require, it can be approximated via large number of replicates
# for each replicate the t_stat is calculated, and the t_stat sampling distro created from all replicate t_stats
# this gives the t_distro under null hypothesis, showing what we'd expect if there is no diff in means, and any diff found is purely random
# then the proportion of replicates with a t_stat equal to or greater than the observed t_stat is taken as the p_value for how unlikely this is
# explanation of permutation tests: http://faculty.washington.edu/kenrice/sisg/SISG-08-06.pdf
# also: http://genomicsclass.github.io/book/pages/permutation_tests.html
set.seed(123)
t_distro_under_null_hypoth <- fli_small %>% specify(arr_delay ~ half_year) %>% hypothesize(null = "independence") %>%
        generate(reps = 1000, type = "permute") %>% calculate(stat = "t", order = c("h1", "h2"))
t_distro_under_null_hypoth

# visualize t_distro_under_null_hypoth
t_distro_under_null_hypoth %>% visualize()

# visualize t_distro_under_null_hypoth along with the observed_t_score to see how extreme it is
t_distro_under_null_hypoth %>% visualize(obs_stat = observed_t_score, direction = "two_sided")

# get p_value for how unlikely observed_t_score is given t_distro_under_null_hypoth
t_distro_under_null_hypoth %>% get_pvalue(obs_stat = observed_t_score, direction = "two_sided")


#################


# use permute to conduct same hypothesis test using calculate(stat = "diff in means") instead of stat = "t"
set.seed(123)
diff_in_means_distro_under_null_hypoth <- fli_small %>% specify(arr_delay ~ half_year) %>% hypothesize(null = "independence") %>%
        generate(reps = 1000, type = "permute") %>% calculate(stat = "diff in means", order = c("h1", "h2"))
diff_in_means_distro_under_null_hypoth

# visualize diff_in_means_distro_under_null_hypoth
diff_in_means_distro_under_null_hypoth %>% visualize()

# visualize diff_in_means_distro_under_null_hypoth along with the observed_t_score to see how extreme it is
diff_in_means_distro_under_null_hypoth %>% visualize(obs_stat = observed_diff_in_means, direction = "two_sided")

# get p_value for how unlikely observed_t_score is given t_distro_under_null_hypoth
diff_in_means_distro_under_null_hypoth %>% get_pvalue(obs_stat = observed_diff_in_means, direction = "two_sided")


#################


# just as an example, show why you do not want to get t_distro_under_null_hypoth using bootstrap method instead of permute
# note that permute seems more centered on 0, which makes since because the null hypothesis is assuming no diff_in_means
# but when we use bootstrap, it's not really generating the t_distro under null if the data it's resampling does happen to have diff_in_means 
t_distro_under_null_hypoth_bootstrap <- fli_small %>% specify(arr_delay ~ half_year) %>% 
        # hypothesize(null = "independence") %>% # don't use hypothesize with bootstrap (it will throw error saying as much)
        generate(reps = 1000, type = "bootstrap") %>% calculate(stat = "t", order = c("h1", "h2"))
t_distro_under_null_hypoth_bootstrap
t_distro_under_null_hypoth

# visualize t_distro_under_null_hypoth
# note that permute seems more centered on 0, which makes since because the null hypothesis is assuming no diff_in_means
# but when we use bootstrap, it's not really generating the t_distro under null if the data it's resampling does happen to have diff_in_means 
t_distro_under_null_hypoth %>% visualize()
t_distro_under_null_hypoth_bootstrap %>% visualize()


################


# can visualize both the randomization method distribution and the theoretical distribution
fli_small %>%
        specify(arr_delay ~ half_year) %>%
        hypothesize(null = "independence") %>%
        generate(reps = 1000, type = "permute") %>%
        calculate(stat = "t", order = c("h1", "h2")) %>% 
        visualize(method = "both", obs_stat = observed_t_score, direction = "two_sided")


########################


# compare pvalues from randomization and theoretical distributions
t_test_results %>% select(p_value)
t_distro_under_null_hypoth %>% get_pvalue(obs_stat = observed_t_score, direction = "two_sided")
diff_in_means_distro_under_null_hypoth %>% get_pvalue(obs_stat = observed_diff_in_means, direction = "two_sided")


##############################################################################################
##############################################################################################
##############################################################################################


# prop test with infer

# get data
mtcars_df <- as.data.frame(mtcars) %>%
        mutate(cyl = factor(cyl),
               vs = factor(vs),
               am = factor(am),
               gear = factor(gear),
               carb = factor(carb))
mtcars_df %>% head()

# get diff_in_props
mtcars_df %>% group_by(vs) %>% summarize(am_prop = mean(as.numeric(as.character(am)), na.rm = TRUE)) %>% 
        gather(key = variable, value = value, - vs) %>% unite(col = united_var, variable, vs) %>% spread(key = united_var, value = value) %>%
        mutate(diff_in_props = am_prop_1 - am_prop_0)

#get diff_in_props with permute
diff_in_props_distro_null_hypoth <- mtcars %>%
        specify(am ~ vs, success = "1") %>%
        hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>%
        calculate(stat = "diff in props", order = c("1", "0"))
diff_in_props_distro_null_hypoth

# visualize diff_in_props_distro_null_hypoth
diff_in_props_distro_null_hypoth %>% visualize()
