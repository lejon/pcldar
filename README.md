[![Build Status](https://travis-ci.org/lejon/pcldar.svg?branch=master)](https://travis-ci.org/lejon/pcldar)

# pcldar
R Wrapper for our Partially Collapsed Gibbs sampler for LDA (PCLDA) described in the article 

Magnusson, M., Jonsson, L., Villani, M., & Broman, D. (2017). Sparse Partially Collapsed MCMC for Parallel Inference in Topic Models. Journal of Computational and Graphical Statistics.

```
@article{magnusson2017sparse,
  title={Sparse Partially Collapsed MCMC for Parallel Inference in Topic Models},
  author={Magnusson, M{\aa}ns and Jonsson, Leif and Villani, Mattias and Broman, David},
  journal={Journal of Computational and Graphical Statistics},
  year={2017},
  publisher={Taylor \& Francis}
}
```

The toolkit is Open Source Software, and is released under the Common Public License. You are welcome to use the code under the terms of the license for research or commercial purposes, however please acknowledge its use with a citation:
  Magnusson, Jonsson, Villani, Broman.  "Sparse Partially Collapsed MCMC for Parallel Inference in Topic Models."

The dataset and the stopwords file (stopwords.txt, included in the repository) should be in the same folder as you run the sampler.

```{r}
library(pcldar)
nr_topics <- 20
ds_fn <- "nips.txt"
iterations <- 1000
cnf <- new_simple_lda_config(ds_fn,
                          nr_topics = nr_topics, alpha = 0.01,
                          beta = (nr_topics / 50), iterations = iterations,
                          rareword_threshold = 10, scheme="polyaurn",
                          stoplist_fn = "stoplist.txt", topic_interval = 10,
                          tmpdir = "/tmp")

ds <- load_lda_dataset(ds_fn,cnf)
lda <- sample_pclda(cnf, ds, iterations = iterations)

phi <- get_phi(lda)
ttm <- get_type_topics(lda)
dens <- calculate_ttm_density(ttm)
zBar <- get_z_means(lda)
theta <- get_theta_estimate(lda)

cat("Top Words:\n")
cat(print_top_words(get_topwords(lda)))
```
