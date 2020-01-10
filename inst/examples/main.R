options(java.parameters = c("-Xmx6g","-Djava.util.logging.config.file=resources/logging.properties"))

library(pcldar)
nr_topics <- 20
ds_fn <- "nips.txt"
iterations <- 1000
cnf <- new_simple_lda_config(ds_fn,
                          nr_topics = nr_topics, alpha = 0.01,
                          beta = (nr_topics / 50), iterations = iterations,
                          rareword_threshold = 10,
                          stoplist_fn = "stoplist.txt", topic_interval = 10,
                          tmpdir = "/tmp")

ds <- load_lda_dataset(ds_fn,cnf)
lda <- sample_pclda(cnf, ds, iterations = iterations)

phi <- get_phi(lda)
ttm <- get_type_topics(lda)
dens <- calculate_ttm_density(ttm)
zBar <- get_z_means(lda)
theta <- get_theta_estimate(lda)

tw <- get_topwords(lda)
trw <- get_top_relevance_words(lda,cnf)

cat("Top Words:\n")
cat(print_top_words(get_topwords(lda)))

cat("Top Relevance Words:\n")
cat(print_top_words(get_top_relevance_words(lda,cnf)))

