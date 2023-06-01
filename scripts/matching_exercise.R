# nearest neighbor matching for MNE-domestic pairs
library(MatchIt)

# cdf dataframe w/ columns "oo_cegj_sz", "hely", "nace3d", "emp", "jaras", "megye", "mne01"  

# test on sample first, as it takes some time for larger data
test_size <- 80000
mdf <- cdf[1:test_size]

# matching function
m01 <- matchit(mne01 ~ megye + emp + nace3d, method = "nearest", distance = "mahalanobis", data = mdf)

# summary stats of the matching exercise
#summary(m01)

# get the matched data -- m_data$subclass indicates the matched pairs
m_data <- match.data(m01, data = mdf, distance = "prop.score")
#match_results <- get_matches(m01, data = mdf, distance = "prop.score")
