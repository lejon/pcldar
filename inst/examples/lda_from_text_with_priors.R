library(tidyverse)
library(pcldar)
library(magrittr)

nr_topics <- 20
ds_fn <- "na"
iterations <- 1000
cnf <- new_simple_lda_config(ds_fn,
                             nr_topics = nr_topics, alpha = 0.01,
                             beta = (nr_topics / 50), iterations = iterations,
                             rareword_threshold = 10,
                             stoplist_fn = system.file("extdata", "stoplist.txt", package = "pcldar"),
                             topic_interval = 10,
                             tmpdir = "/tmp", topic_priors = "ap_prior.txt")

ds_fn <- system.file("extdata", "100ap.txt", package = "pcldar")
trtextdf <- as.data.frame(readLines(ds_fn))
colnames(trtextdf) <- "line"

trtextdf <- trtextdf %>% dplyr::mutate(line = iconv(line,"latin1", "ASCII"))

doclines <- as.character(trtextdf$line)
dss <- create_lda_dataset(doclines,doclines)
lda <- sample_pclda(cnf, dss[[1]], iterations = iterations, testset=dss[[2]],
                    samplerType="cc.mallet.topics.PolyaUrnSpaliasLDAWithPriors")

phi <- get_phi(lda)
ttm <- get_type_topics(lda)
dens <- calculate_ttm_density(ttm)
zBar <- get_z_means(lda)
theta <- get_theta_estimate(lda)

tw <- get_topwords(lda)
trw <- get_top_relevance_words(lda,cnf)

ll <- get_log_likelihood(lda)
ll

hll <- get_held_out_log_likelihood(lda)
hll

stats <- data.frame(iter=(1:(length(ll)))*10,loglikelihood=ll,heldout_likelihood=hll)
stats %<>% tidyr::gather(type,value,-iter)

ggplot(stats,aes(x=iter,y=value, color=type)) + geom_line() + theme_bw()

cat("Top Words:\n")
cat(print_top_words(get_topwords(lda)))

cat("Top Relevance Words:\n")
cat(print_top_words(get_top_relevance_words(lda,cnf)))

