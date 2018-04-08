
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

#' @importFrom rJava .jnew .jcall
#' @export
load_lda_dataset <- function(fn, ldaconfig) {
  util <- .jnew("cc.mallet.util.LDAUtils")
  ds <- .jcall(util,"Lcc/mallet/types/InstanceList;","loadDataset",
        .jcast(ldaconfig,"cc.mallet.configuration.LDAConfiguration"),fn);
  return(ds)
}

#' @importFrom rJava .jnew .jcall
#' @export
create_lda_dataset <- function(doclines, ldaconfig, stoplist_fn = "stoplist.txt") {
  stringIterator <- .jnew("cc.mallet.util.StringClassArrayIterator", doclines)
  util <- .jnew("cc.mallet.util.LDAUtils")
  pipe <- .jcall(util,"Lcc/mallet/pipe/Pipe;","buildSerialPipe", stoplist_fn,
                 .jcast(.jnull(),"cc.mallet.types.Alphabet"))

  il <- .jnew("cc.mallet.types.InstanceList",pipe)
  .jcall(il,"V","addThruPipe", .jcast(stringIterator,"java.util.Iterator"))
  .jcall(il,"V","addThruPipe", .jcast(stringIterator,"java.util.Iterator"))
  #trainingInstances.getAlphabet().stopGrowth();

  return(il)
}

#' @importFrom rJava .jnew .jcall
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

#' @importFrom rJava .jnew .jcall
#' @export
print_top_words <- function(word_matrix) {
  util <- .jnew("cc.mallet.util.LDAUtils")
  return(util$formatTopWords(.jarray(word_matrix,dispatch = T)))
}

#' @importFrom rJava .jnew .jcall
#' @export
extract_vocabulary <- function(alphabet) {
  util <- .jnew("cc.mallet.util.LDAUtils")
  #.jcall(util,"[Ljava/lang/String;", "extractVocabulaty", alph)
  return(.jcall(util,"[S", "extractVocabulaty", alphabet))
}

#' @importFrom rJava .jnew .jcall
#' @export
get_alphabet <- function(lda) {
  return(.jcall(lda,"Lcc/mallet/types/Alphabet;","getAlphabet"))
}

#' @importFrom rJava .jcall
#' @export
get_theta_estimate <- function(lda) {
  theta  <- .jcall(lda,"[[D","getThetaEstimate",simplify = TRUE)
  return(theta)
}

#' @importFrom rJava .jcall
#' @export
get_z_means <- function(lda) {
  zb  <- .jcall(lda,"[[D","getZbar",simplify = TRUE)
  return(zb)
}

#' @importFrom rJava .jcall
#' @export
get_type_topics <- function(lda) {
  ttm  <- .jcall(lda,"[[I","getTypeTopicMatrix",simplify = TRUE)
  return(ttm)
}

#' @importFrom rJava .jcall
#' @export
get_phi <- function(lda) {
  phi  <- .jcall(lda,"[[D","getPhi",simplify = TRUE)
  return(phi)
}

#' @importFrom rJava .jnew .jcall
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

#' @importFrom rJava .jnew .jcall
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

#' @importFrom rJava .jcall
#' @export
calculate_ttm_density <- function(typeTopicMatrix) {
  util <- .jnew("cc.mallet.util.LDAUtils")
  return(.jcall(util,"D", "calculateMatrixDensity",.jarray(typeTopicMatrix,dispatch = T)))
}




