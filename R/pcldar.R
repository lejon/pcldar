#' new_simple_lda_config
#'
#' Create a new LDA config file
#'
#' @param dataset_fn filename of dataset (in LDA format)
#' @param nr_topics number of topics to use
#' @param alpha symmetric alpha prior
#' @param beta symmetric beta prior
#' @param iterations number of iterations to sample
#' @param rareword_threshold min. number of occurences of a word to be kept
#' @param scheme sampling sceme ("adlda", "uncollapsed", "collapsed", "lightcollapsed"
#' "efficient_uncollapsed", "spalias", "polyaurn" (default),
#' "spalias_priors", "lightpclda", "lightpcldaw2", "nzvsspalias")
#' @param stoplist_fn filenname of stoplist file (one word per line) (default "stoplist.txt")
#' @param topic_interval how often to print topic info during sampling
#' @param tmpdir temporary directory for intermediate storage of logging data (default "tmp")
#'
#' @importFrom rJava .jnew .jcall
#' @export
new_simple_lda_config <- function(dataset_fn, nr_topics = 20, alpha = 0.01,
                               beta = (nr_topics / 50), iterations = 2000,
                               rareword_threshold = 10, scheme="polyaurn",
                               stoplist_fn = "stoplist.txt", topic_interval = 10,
                               tmpdir = "/tmp") {
  lu <- .jnew("cc.mallet.util.LoggingUtils")
  .jcall(lu,"Ljava/io/File;","checkAndCreateCurrentLogDir",tmpdir);
  #SimpleLDAConfiguration(LoggingUtils logUtil, String scheme,
  #                       Integer noTopics, Double alpha, Double beta, Integer noIters,
  #                       Integer noBatches, Integer rareThreshold, Integer topicInterval,
  #                       Integer startDiagnostic, int seed, String datasetFn)
  slc <- .jnew("cc.mallet.configuration.SimpleLDAConfiguration")
  .jcall(slc,"V","setLoggingUtil",lu)
  .jcall(slc,"V","setScheme",scheme)
  .jcall(slc,"V","setNoTopics",as.integer(nr_topics))
  alpha <- .jnew("java.lang.Double",alpha)
  .jcall(slc,"V","setAlpha",alpha)
  beta <- .jnew("java.lang.Double",beta)
  .jcall(slc,"V","setBeta",beta)
  iters <- .jnew("java.lang.Integer",as.integer(iterations))
  .jcall(slc,"V","setNoIters",iters)
  rare <- .jnew("java.lang.Integer",as.integer(rareword_threshold))
  .jcall(slc,"V","setRareThreshold",rare)
  ti <- .jnew("java.lang.Integer",as.integer(topic_interval))
  .jcall(slc,"V","setTopicInterval",ti)
  di <- .jnew("java.lang.Integer",as.integer(90))
  .jcall(slc,"V","setStartDiagnostic",di)
  btc <- .jnew("java.lang.Integer",as.integer(5))
  .jcall(slc,"V","setNoBatches",btc)
  .jcall(slc,"V","setStoplistFilename",stoplist_fn)
  .jcall(slc,"V","setDatasetFilename",dataset_fn)
  return(slc)
}

#' load_lda_dataset
#'
#' Load an LDA dataset from file. The file should be in LDA format i.e.:
#' <unique id>\\t<doc class>\\t<document content>\\n)
#' The document class is not used in by the LDA sampler. Each line
#' must be concluded with a newline so all other newlines in the
#' document must be removed. The document content CAN have \\t in it.
#'
#' @param fn filename of dataset
#' @param ldaconfig LDA config object
#'
#' @importFrom rJava .jnew .jcall .jcast
#' @export
load_lda_dataset <- function(fn, ldaconfig) {
  util <- .jnew("cc.mallet.util.LDAUtils")
  ds <- .jcall(util,"Lcc/mallet/types/InstanceList;","loadDataset",
        .jcast(ldaconfig,"cc.mallet.configuration.LDAConfiguration"),fn);
  return(ds)
}

#' create_lda_dataset
#'
#' Create an LDA dataset from existing string vector. Each entry
#' in the vector must be a string with the following format:
#' <unique id>\\t<doc class>\\t<document content>\\n)
#' The document class is not used in by the LDA sampler.
#' The document content CAN have \\t in it.
#'
#' @param doclines string vector with document data
#' @param ldaconfig LDA config object
#' @param stoplist_fn filiename of stoplist file
#'
#' @importFrom rJava .jnew .jcall .jarray
#' @export
create_lda_dataset <- function(doclines, ldaconfig, stoplist_fn = "stoplist.txt") {
  stringIterator <- .jnew("cc.mallet.util.StringClassArrayIterator", doclines)
  util <- .jnew("cc.mallet.util.LDAUtils")
  pipe <- .jcall(util,"Lcc/mallet/pipe/Pipe;","buildSerialPipe", stoplist_fn,
                 .jcast(.jnull(),"cc.mallet.types.Alphabet"))

  il <- .jnew("cc.mallet.types.InstanceList",pipe)
  .jcall(il,"V","addThruPipe", .jcast(stringIterator,"java.util.Iterator"))
  #trainingInstances.getAlphabet().stopGrowth();

  return(il)
}

#' sample_pclda
#'
#' Run the PCLDA (default Polya Urn) sampler
#'
#' @param ldaconfig LDA config object
#' @param ds LDA dataset
#' @param iterations number of iterations to run
#' @param samplerType Java class of the sampler. Must implement the LDASampler interface
#'
#' @importFrom rJava .jnew .jcall .jarray .jcast .jnull
#' @export
sample_pclda <- function(ldaconfig, ds, iterations = 2000, samplerType="cc.mallet.topics.PolyaUrnSpaliasLDA") {
  #.jconstructors(samplerType)
  lcfg <- .jcast(ldaconfig,"cc.mallet.configuration.LDAConfiguration")
  ex <- tryCatch(lda <- .jnew(samplerType,lcfg), NullPointerException = function(ex) ex)
  #lda <- .jnew("cc.mallet.topics.SpaliasUncollapsedParallelLDA",.jcast(slc,"cc.mallet.configuration.LDAConfiguration"))
  .jcall(lda,"V", "addInstances", ds)
  .jcall(lda,"V", "sample", as.integer(iterations))
  return(lda)
}

#' print_top_words
#'
#' Print the 'top words' from a sampled word matrix
#' obtained using 'get_topwords(lda)'
#'
#' @param word_matrix top word matrix
#'
#' @importFrom rJava .jnew .jcall
#' @export
print_top_words <- function(word_matrix) {
  util <- .jnew("cc.mallet.util.LDAUtils")
  return(util$formatTopWords(.jarray(word_matrix,dispatch = T)))
}

#' extract_vocabulary
#'
#' Extract the vocabulary as a string vector from an MALLET Alphabet
#'
#' @param alphabet Java MALLET Alphabet object, obtained by 'get_alphabet(lda)'
#'
#' @importFrom rJava .jnew .jcall
#' @export
extract_vocabulary <- function(alphabet) {
  util <- .jnew("cc.mallet.util.LDAUtils")
  #.jcall(util,"[Ljava/lang/String;", "extractVocabulaty", alph)
  return(.jcall(util,"[S", "extractVocabulaty", alphabet))
}

#' get_alphabet
#'
#' Get the Alphabet (as a java object) from an LDA sampler
#'
#' @param lda LDA sampler object
#'
#' @importFrom rJava .jnew .jcall
#' @export
get_alphabet <- function(lda) {
  return(.jcall(lda,"Lcc/mallet/types/Alphabet;","getAlphabet"))
}

#' get_theta_estimate
#'
#' Get an estimate of the document topic distribution
#' (the theta matrix) from an LDA sampler
#'
#' @param lda LDA sampler object
#'
#' @importFrom rJava .jcall
#' @export
get_theta_estimate <- function(lda) {
  theta  <- .jcall(lda,"[[D","getThetaEstimate",simplify = TRUE)
  return(theta)
}

#' get_z_means
#'
#' Get the mean of the topic indicators from an LDA sampler
#'
#' @param lda LDA sampler object
#'
#' @importFrom rJava .jcall
#' @export
get_z_means <- function(lda) {
  zb  <- .jcall(lda,"[[D","getZbar",simplify = TRUE)
  return(zb)
}

#' get_type_topics
#'
#' Get the type/topic matrix from and LDA sampler
#'
#' @param lda LDA sampler object
#'
#' @importFrom rJava .jcall
#' @export
get_type_topics <- function(lda) {
  ttm  <- .jcall(lda,"[[I","getTypeTopicMatrix",simplify = TRUE)
  return(ttm)
}

#' get_phi
#'
#' Get the word/topic distribution (phi matrix) from an LDA sampler
#'
#' @param lda LDA sampler object
#'
#' @importFrom rJava .jcall
#' @export
get_phi <- function(lda) {
  phi  <- .jcall(lda,"[[D","getPhi",simplify = TRUE)
  return(phi)
}

#' get_topwords
#'
#' Get the top words per topic from an LDA sampler
#'
#' @param lda LDA sampler object
#' @param nr_words number of top words per topic to retrieve
#'
#' @importFrom rJava .jnew .jcall .jarray
#' @export
get_topwords <- function(lda,nr_words=20) {
  util <- .jnew("cc.mallet.util.LDAUtils")
  alph <- .jcall(lda,"Lcc/mallet/types/Alphabet;","getAlphabet")
  typeTopicMatrix  <- .jcall(lda,"[[I","getTypeTopicMatrix", simplify = TRUE)
  alphSize <- .jcall(alph,"I", "size")
  nrTopics <- .jcall(lda,"I","getNoTopics")
  tw <- .jcall(util,"[[Ljava/lang/String;", "getTopWords",
               as.integer(nr_words),
               as.integer(alphSize),
               as.integer(nrTopics),
               .jarray(typeTopicMatrix,dispatch = T),
               alph,simplify = TRUE)
  tw
}

#' get_top_relevance_words
#'
#' Get the 'top relevance words' (weighted version of top words)
#' per topic from an LDA sampler
#'
#' @param lda LDA sampler object
#' @param config LDA config object
#' @param nr_words number of top words per topic to retrieve
#' @param lambda lambda value when calculating the relevance
#'
#' @importFrom rJava .jnew .jcall .jarray
#' @export
get_top_relevance_words <- function(lda,config,nr_words=20, lambda=0.6) {
  util <- .jnew("cc.mallet.util.LDAUtils")
  alph <- .jcall(lda,"Lcc/mallet/types/Alphabet;","getAlphabet")
  typeTopicMatrix <- .jcall(lda,"[[I","getTypeTopicMatrix", simplify = TRUE)
  alphSize <- .jcall(alph,"I", "size")
  nrTopics <- .jcall(lda,"I","getNoTopics")
  betaD <- .jcall(config,"Ljava/lang/Double;","getBeta",as.numeric(0.01))
  beta <- .jcall(betaD,"D","doubleValue")
  rw <- .jcall(util,"[[Ljava/lang/String;", "getTopRelevanceWords",
                as.integer(nr_words),
                as.integer(alphSize),
                as.integer(nrTopics),
                .jarray(typeTopicMatrix,dispatch = T),
                as.numeric(beta),
                as.numeric(lambda),
                alph,
               simplify = TRUE)
  rw
}

#' calculate_ttm_density
#'
#' Calculate the density (sparsity) of the type topic matrix
#'
#' @param typeTopicMatrix type topic matrix, obtained by 'get_type_topics(lda)'
#'
#' @importFrom rJava .jcall .jarray
#' @export
calculate_ttm_density <- function(typeTopicMatrix) {
  util <- .jnew("cc.mallet.util.LDAUtils")
  return(.jcall(util,"D", "calculateMatrixDensity",.jarray(typeTopicMatrix,dispatch = T)))
}




