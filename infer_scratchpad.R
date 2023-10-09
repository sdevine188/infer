library(nycflights13)
library(dplyr)
library(stringr)
library(infer)
library(boot)

# https://github.com/topepo/infer
# https://github.com/topepo/infer/tree/master/vignettes
# https://github.com/topepo/infer/blob/master/vignettes/two_sample_t.Rmd
# https://thomasleeper.com/Rcourse/Tutorials/permutationtests.html
# https://ismayc.github.io/talks/data-day-texas-infer/slide_deck.html#53
# https://stats.stackexchange.com/questions/20217/bootstrap-vs-permutation-hypotheis-testing
# https://www.rdocumentation.org/packages/infer/versions/0.3.1/topics/conf_int
# http://www2.stat.duke.edu/~banks/111-lectures.dir/lect13.pdf

# https://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704_confidence_intervals/bs704_confidence_intervals_print.html#:~:text=If%20a%2095%25%20confidence%20interval,significant%20difference%20between%20the%20groups.
# If a 95% confidence interval includes the null value, then there is no statistically meaningful or 
# statistically significant difference between the groups. If the confidence interval does not include the 
# null value, then we conclude that there is a statistically significant difference between the groups.

# note that stackexchange said permutation is more commonly used for tests, 
# whereas bootstrap is more commonly used to get conf_int
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


#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////


# get conf_int for diff_in_means with bootstrap method
# note, I'm removing NA values to try and get infer's bootstrap conf_ints to match the boot package
# initially there was a difference, which I thought might because infer and boot handle NA's differently?
# results: dropping NA's didn't have an effect- infer still gets slightly different results than boot
# infer CI: -3.68ish to 10.3ish; boot CI: -2.2ish to 8.4ish (even boot's percentile and normal CI methods) 
# for some reason infer seems more conservative, giving wider CI
observed_diff_in_means <- fli_small %>% select(arr_delay, half_year) %>% 
        filter(!is.na(arr_delay), !is.na(half_year)) %>%
        specify(arr_delay ~ half_year) %>% 
        calculate(stat = "diff in means", order = c("h1", "h2"))
observed_diff_in_means

# get bootstrap_distn for diff in means
# bootstrap just takes resamples of data with replacement (same number of observations), 
# then calculates the diff_in_means on these replicates
# the collection of these diff_in_means across replicates creates diff_in_means sampling distro
set.seed(123)
diff_in_means_bootstrap_dist <- fli_small %>% 
        select(arr_delay, half_year) %>% 
        filter(!is.na(arr_delay), !is.na(half_year)) %>%
        specify(arr_delay ~ half_year) %>%
        hypothesize(null = "independence") %>%
        generate(reps = 1000, type = "bootstrap") %>%
        calculate(stat = "diff in means", order = c("h1", "h2"))
diff_in_means_bootstrap_dist

# visualize diff_in_means_bootstrap_dist
diff_in_means_bootstrap_dist %>% visualize() 

# get conf_int for diff_in_means
# conf_int using randomized bootstrap diff_in_means percentile method
# note that for get_confidence_interval type = se, you need to provide point_estimate = observed_diff_in_means 
# so that it can caluclate observed_diff_in_means +/- (1.96 * se)
# when get_confidence_interval type = "percentile", you don't need to pass observed_diff_in_means
# because it just grabs the .025 and .975 percentiles 
diff_in_means_bootstrap_dist %>% get_confidence_interval(level = 0.95, type = "percentile")
diff_in_means_bootstrap_dist %>% 
        summarize(quantiles = list(enframe(quantile(x = stat, probs = c(.025, .975))))) %>% unnest(quantiles)

# conf_int using theoretical 1.96 * std_error method
# std_error is measure of precision of estimate, 
# calculated as the standard deviation of the estimate's sampling distribution
# note that for get_confidence_interval type = se, you need to provide point_estimate = observed_diff_in_means 
# so that it can caluclate observed_diff_in_means +/- (1.96 * se)
# when get_confidence_interval type = "percentile", you don't need to pass observed_diff_in_means
# because it just grabs the .025 and .975 percentiles 
diff_in_means_bootstrap_dist %>% 
        summarize(se = sd(stat), 
                  conf_int_lower = observed_diff_in_means$stat - 1.96 * se, 
                  conf_int_upper = observed_diff_in_means$stat + 1.96 * se)
diff_in_means_bootstrap_dist %>% get_confidence_interval(level = .95, type = "se", point_estimate = observed_diff_in_means)


#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////


# use permutation test
# note, I'm removing NA values to try and get infer's bootstrap conf_ints to match the boot package
# initially there was a difference, which I thought might because infer and boot handle NA's differently?
# results: dropping NA's didn't have an effect- infer still gets slightly different results than boot
# infer CI: -3.68ish to 10.3ish; boot CI: -2.2ish to 8.4ish (even boot's percentile and normal CI methods) 
# for some reason infer seems more conservative, giving wider CI
observed_diff_in_means <- fli_small %>% select(arr_delay, half_year) %>% 
        filter(!is.na(arr_delay), !is.na(half_year)) %>%
        specify(arr_delay ~ half_year) %>% 
        calculate(stat = "diff in means", order = c("h1", "h2"))
observed_diff_in_means

# get permute_dist for diff in means
# "We can generate the null distribution using permutation, where, for each replicate, 
# each value of degree status will be randomly reassigned (without replacement) to a new number of hours worked 
# per week in the sample in order to break any association between the two."
set.seed(123)
diff_in_means_permute_dist <- fli_small %>% 
        select(arr_delay, half_year) %>% 
        filter(!is.na(arr_delay), !is.na(half_year)) %>%
        specify(arr_delay ~ half_year) %>%
        hypothesize(null = "independence") %>%
        generate(reps = 1000, type = "permute") %>%
        calculate(stat = "diff in means", order = c("h1", "h2"))
diff_in_means_permute_dist

# visualize diff_in_means_permute_dist and diff_in_means_bootstrap_dist
diff_in_means_permute_dist %>% visualize() + shade_p_value(observed_diff_in_means, direction = "two-sided")

# get p_value for permute_dist
diff_in_means_permute_dist %>% 
        get_p_value(obs_stat = observed_diff_in_means, direction = "two-sided")

# note that pnorm gives a very large p_value for observed_diff_in_means, this is because
# the permute_null_dist is normal, but is not standardized and has values far above traditional normal dist 
# where you subtract the mean and divide by standard deviation
# pnorm(q = observed_diff_in_means %>% pull(stat))

# get conf_int for diff_in_means
# note that for permutation test, get_confidence_interval type = percentile seems to give a weird answer
# of large values around zero (-7, 7), but get_confidence_interval type = se gives reasonable conf_int that is
# also very close to the bootstrapped conf_int found above
# conf_int using randomized bootstrap diff_in_means percentile method
# note that for get_confidence_interval type = se, you need to provide point_estimate = observed_diff_in_means 
# so that it can caluclate observed_diff_in_means +/- (1.96 * se)
# when get_confidence_interval type = "percentile", you don't need to pass observed_diff_in_means
# because it just grabs the .025 and .975 percentiles 

# diff_in_means_permute_dist %>% get_confidence_interval(level = 0.95, type = "percentile")
# diff_in_means_permute_dist %>% 
#         summarize(quantiles = list(enframe(quantile(x = stat, probs = c(.025, .975))))) %>% unnest(quantiles)

# conf_int using theoretical 1.96 * std_error method
# std_error is measure of precision of estimate, 
# calculated as the standard deviation of the estimate's sampling distribution
# note that for get_confidence_interval type = se, you need to provide point_estimate = observed_diff_in_means 
# so that it can caluclate observed_diff_in_means +/- (1.96 * se)
# when get_confidence_interval type = "percentile", you don't need to pass observed_diff_in_means
# because it just grabs the .025 and .975 percentiles 
diff_in_means_permute_dist %>% 
        summarize(se = sd(stat), 
                  conf_int_lower = observed_diff_in_means$stat - 1.96 * se, 
                  conf_int_upper = observed_diff_in_means$stat + 1.96 * se)
diff_in_means_permute_dist %>% get_confidence_interval(level = .95, type = "se", point_estimate = observed_diff_in_means)


#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////


# get bootstrap confidence interval using boot package, 
# which can provide the fancier and supposedly better Bias Corrected and Accelerated (BCA) bootstrap confidence interval
# in addition to the standard error version and percentile version
# though as discussed above, note that boot CI seems to be more narrow than infer, even for percentile and normal methods
# http://users.stat.umn.edu/~helwig/notes/bootci-Notes.pdf

# note for the boot package, you have to write a function taking the data argument and the 
# bootstrapped indices argument.  then subset the data to just the indices, compute the statistic, and return results
diff_in_mean_arr_delay <- function(data, indices) {
        h1_mean_arr_delay <- data %>% filter(half_year == "h1") %>%
                filter(row_number() %in% indices) %>% 
                summarize(mean_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% 
                pull(mean_arr_delay)
        h2_mean_arr_delay <- data %>% filter(half_year == "h2") %>%
                filter(row_number() %in% indices) %>% 
                summarize(mean_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
                pull(mean_arr_delay)
        return(h1_mean_arr_delay - h2_mean_arr_delay)
}

set.seed(123) 
# use the same random number sequence each time for 
# resampling each time by setting a seed value.
# this gives reproducible results
diff_in_mean_arr_delay_boot_obj <- boot(data = fli_small, statistic = diff_in_mean_arr_delay, R = 2000)
diff_in_mean_arr_delay_boot_obj
attributes(diff_in_mean_arr_delay_boot_obj)

bca_conf_int <- boot.ci(diff_in_mean_arr_delay_boot_obj, conf = 0.95)
bca_conf_int


#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////


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


#///////////////////////////////


# just to show function, calculate t stat using specify() and calculate()
# calculate() just gives t_stat, where t_test() output is more informative
# these verbs (specify and calculate) are used when using randomization methods like bootstrap or permute (see below)
fli_small %>% specify(arr_delay ~ half_year) %>% calculate(stat = "t", order = c("h1", "h2"))


#//////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////


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
t_distro_under_null_hypoth <- fli_small %>% 
        specify(arr_delay ~ half_year) %>% 
        hypothesize(null = "independence") %>%
        generate(reps = 1000, type = "permute") %>% 
        calculate(stat = "t", order = c("h1", "h2"))
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
diff_in_means_distro_under_null_hypoth <- fli_small %>% specify(arr_delay ~ half_year) %>% 
        hypothesize(null = "independence") %>%
        generate(reps = 1000, type = "permute") %>% 
        calculate(stat = "diff in means", order = c("h1", "h2"))
diff_in_means_distro_under_null_hypoth

# visualize diff_in_means_distro_under_null_hypoth
diff_in_means_distro_under_null_hypoth %>% visualize()

# visualize diff_in_means_distro_under_null_hypoth along with the observed_t_score to see how extreme it is
diff_in_means_distro_under_null_hypoth %>% visualize() + 
        shade_p_value(obs_stat = observed_diff_in_means,
                      direction = "two-sided")

# get p_value showing how unlikely observed_diff_in_means is given diff_in_means_distro_under_null_hypoth
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
diff_in_props_distro_null_hypoth <- mtcars %>% mutate(am = as.character(am), vs = as.character(vs)) %>%
        specify(am ~ vs, success = "1") %>%
        hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>%
        calculate(stat = "diff in props", order = c("1", "0"))
diff_in_props_distro_null_hypoth

# visualize diff_in_props_distro_null_hypoth
diff_in_props_distro_null_hypoth %>% visualize()


#///////////////////////////////////////////////////////////////////////////////////////


# simplified prop test with infer::prop_test vs base prop.test
# note that prop_test is useful in taking tidy record-level data, instead of prop.test which requires
# vector of two samples trials and successes
# but prop_test provides less info in output than prop.test + tidy()


# note that under the hood, prop_test calls table() on response and explanatory variables and passes that to prop.test()
# sum_table <- x %>% select(response_name(x), explanatory_name(x)) %>% 
#         table()
# sum_table <- sum_table[lvls, order]
# prelim <- stats::prop.test(x = sum_table, alternative = alternative, 
#                            conf.level = conf_level, p = p, correct = correct, 
#                            ...)


# use prop_test (with or without formula syntax)
prop_test(gss,
          college ~ sex,
          order = c("female", "male"))
prop_test(gss,
          response = college, 
          explanatory = sex,
          order = c("female", "male"))

# manually get diff_in_proportion
gss %>% mutate(degree_flag = case_when(college == "degree" ~ 1,
                                       TRUE ~ 0)) %>%
        group_by(sex) %>%
        mutate(n = n(),
               degree_complete = sum(degree_flag),
               degree_share = degree_complete / n) %>%
        ungroup() %>%
        distinct(sex, n, degree_complete, degree_share)

gss %>% select(sex, college) %>% table()
gss %>% janitor::tabyl(sex, college)


# confirm answer with prop.test using vector of two sample successes and trials (not record-level tidy data like prop_test)
library(broom)
prop.test(x = c(91, 83), n = c(263, 237), alternative = "two.sided") %>% tidy()

# note that prop_test will drop unused levels for a categorical variable 
prop_test(gss,
          college ~ partyid,
          order = c("dem", "rep"))

# note that prop.test doesn't change depending on the designation of whether 1 or 0 equals a "success"
prop.test(x = c(25, 50), n = c(100, 100), alternative = "two.sided")
prop.test(x = c(75, 50), n = c(100, 100), alternative = "two.sided")



