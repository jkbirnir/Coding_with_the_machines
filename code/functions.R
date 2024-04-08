
## ---------------------------
##
## Script name: AMAR Expansion Functions
##
## Purpose of script: Load utilities functions required for AMAR analysis in analysis.R file
##
## Author: Henry Overos
##
## Date Created: 2021-10-05
##
## Email: hoveros@umd.edu
##
## ---------------------------
##
## Notes:
## Date Edited 2021-10-05   
## Date Edited 2021-11-17 -21 original functions file generated and error
# Error in eval(ei, envir) : object 'lis_add' not found Oja fixed it
## ---------------------------

#' Clean Corpus for Newsmap Analysis
#'
#' @param corpus_object An object that containst the corpus to be cleaned.
#' @return The corpus, tokenized, punctuation removed, symbols removed, stop words removed, and lower cased.
#'
clean_corpus <- function(corpus_object) {
    toks <- quanteda::tokens(corpus_object, remove_punct = T, remove_symbols = T) %>% tokens_tolower() # create tokens object
    
    toks <- quanteda::tokens_select(toks, "[^a-z_]", selection = "remove", valuetype = "regex") # cleaning text with regex
    
    toks <- quanteda::tokens_select(toks, stopwords("english"), selection = "remove") # removing stopwords
    
    toks <- quanteda::tokens_select(toks, stopwords, selection = "remove") # removing extra stopwords
    
    toks <- quanteda::tokens_select(toks, letters, selection = "remove") # remove individual letters
    
    return(toks)
}

#' Predict function for Newsmap, specifically with All Minorities at Risk
#'
#' @param newsmap_model A Newsmap model
#' @param new_dfm A document-feature matrix of the new data to be categorized with Newsmap_model
#' @param class_prediction A string describing the category of the Newsmap_model. Defaults to ethnicity but can be replaced with other variables in AMAR.
#' @return The corpus, tokenized, punctuation removed, symbols removed, stop words removed, and lower cased.
#'
newsmap_prediction <- function(newsmap_model, new_dfm, class = "ethnicity") {
    prediction <- predict(newsmap_model,
                          newdata = new_dfm,
                          confidence.fit = TRUE
    ) # generates predictions and confidence intervals for new_dfm
    
    prediction <- as.data.frame(prediction) # converts the predictions into a data frame
    
    class_prediction1 <- paste0(".", class) # creates class label
    
    class_prediction2 <- paste0(class, ".") # creates second class label
    
    levels(prediction$class) <- c(levels(prediction$class), "none") # adds none as the lowest level in the categories
    
    prediction$class[prediction$confidence.fit < 0] <- "none" # turns any less than 0 confidence fit into none
    
    prediction$class <- stringr::str_remove(prediction$class,
                                            pattern = class_prediction2
    ) # removes the class from the end of the name of the category
    
    prediction$class <- as.factor(prediction$class) # converts the predictions into factor
    
    nms <- names(prediction) # creates names vector
    
    names(prediction) <- ifelse(nms %in% c("class", "confidence.fit"),
                                paste0(nms, class_prediction1), nms
    )
    
    return(prediction)
}

# Get Initial Dictionary Function
get_initial <- function(x, n = 1) {
    result <- lapply(x, function(x) head(x, n))
    attr(result, "value") <- NA
    attr(result, "key") <- NA
    return(result)
}

# Get Coverage Function
get_coverage <- function(x) {
    x <- dfm_weight(x, "boolean")
    d <- colSums(x) / ndoc(x)
    return(mean(d))
}

# Append the Dictionary Function
append_list <- function(x, y, i, j) {
    stopifnot(length(x) == length(y))
    x[[i]] <- c(x[[i]], y[[i]][j])
    attr(x, "value") <- y[[i]][j]
    attr(x, "key") <- names(x[i])
    return(x)
}

# Grow Dictionary Function
grow_list <- function(x, y, sequential = FALSE) {
    stopifnot(length(x) == length(y))
    if (all(lengths(x) == 0)) {
        names(x) <- names(y)
        for (i in seq_len(length(y))) {
            new <- setdiff(y[[i]], x[[i]])
            if (sequential) {
                add <- head(new, 1)
            } else {
                add <- sample(new, min(length(new), 1))
            }
            x[[i]] <- add
        }
        attr(x, "value") <- unlist(x, use.names = FALSE)
        attr(x, "key") <- names(x)
    } else {
        is_exhausted <- mapply(function(x, y) length(setdiff(y, x)) == 0, x, y)
        if (all(is_exhausted)) {
            warning("No more words to sample", immediate. = TRUE, call. = FALSE)
            attr(x, "value") <- character()
            attr(x, "key") <- character()
        } else {
            i <- which(!is_exhausted)
            if (sequential) {
                i <- min(i)
            } else {
                if (length(i) > 1) {
                    i <- sample(i, 1)
                }
            }
            new <- setdiff(y[[i]], x[[i]])
            if (sequential) {
                add <- head(new, 1)
            } else {
                add <- sample(new, 1)
            }
            x[[i]] <- c(x[[i]], add)
            attr(x, "value") <- add
            attr(x, "key") <- names(x[i])
        }
    }
    return(x)
}


## ---------------------------
##
## Script name: AMAR Expansion Functions
##
## Purpose of script: Load utilities functions required for AMAR analysis in analysis.R file
##
## Author: Henry Overos
##
## Date Created: 2021-10-05
##
## Email: hoveros@umd.edu
##
## ---------------------------
##
## Notes:
## Date Edited 2021-10-05   
##
## ---------------------------

#' Clean Corpus for Newsmap Analysis
#'
#' @param corpus_object An object that contains the corpus to be cleaned.
#' @return The corpus, tokenized, punctuation removed, symbols removed, stop words removed, and lower cased.
#'
clean_corpus <- function(corpus_object,words) {
    toks <- quanteda::tokens(corpus_object, remove_punct = T, remove_symbols = T) %>% tokens_tolower() # create tokens object
    
    toks <- quanteda::tokens_select(toks, "[^a-z_]", selection = "remove", valuetype = "regex") # cleaning text with regex
    
    toks <- quanteda::tokens_select(toks, stopwords("english"), selection = "remove") # removing stopwords
    
    toks <- quanteda::tokens_select(toks, words, selection = "remove") # removing extra stopwords
    
    toks <- quanteda::tokens_select(toks, letters, selection = "remove") # remove individual letters
    
    return(toks)
}

#' Predict function for Newsmap, specifically with All Minorities at Risk
#'
#' @param newsmap_model A Newsmap model
#' @param new_dfm A document-feature matrix of the new data to be categorized with Newsmap_model
#' @param class_prediction A string describing the category of the Newsmap_model. Defaults to ethnicity but can be replaced with other variables in AMAR.
#' @return The corpus, tokenized, punctuation removed, symbols removed, stop words removed, and lower cased.
#'
newsmap_prediction <- function(newsmap_model, new_dfm, class = "ethnicity") {
    prediction <- predict(newsmap_model,
                          newdata = new_dfm,
                          confidence.fit = TRUE
    ) # generates predictions and confidence intervals for new_dfm
    
    prediction <- as.data.frame(prediction) # converts the predictions into a data frame
    
    class_prediction1 <- paste0(".", class) # creates class label
    
    class_prediction2 <- paste0(class, ".") # creates second class label
    
    levels(prediction$class) <- c(levels(prediction$class), "none") # adds none as the lowest level in the categories
    
    prediction$class[prediction$confidence.fit < 0] <- "none" # turns any less than 0 confidence fit into none
    
    prediction$class <- stringr::str_remove(prediction$class,
                                            pattern = class_prediction2
    ) # removes the class from the end of the name of the category
    
    prediction$class <- as.factor(prediction$class) # converts the predictions into factor
    
    nms <- names(prediction) # creates names vector
    
    names(prediction) <- ifelse(nms %in% c("class", "confidence.fit"),
                                paste0(nms, class_prediction1), nms
    )
    
    return(prediction)
}

# Get Initial Dictionary Function
get_initial <- function(x, n = 1) {
    result <- lapply(x, function(x) head(x, n))
    attr(result, "value") <- NA
    attr(result, "key") <- NA
    return(result)
}

# Get Coverage Function
get_coverage <- function(x) {
    x <- dfm_weight(x, "boolean")
    d <- colSums(x) / ndoc(x)
    return(mean(d))
}

# Append the Dictionary Function
append_list <- function(x, y, i, j) {
    stopifnot(length(x) == length(y))
    x[[i]] <- c(x[[i]], y[[i]][j])
    attr(x, "value") <- y[[i]][j]
    attr(x, "key") <- names(x[i])
    return(x)
}

append_list_binary <- function(x, y, j) {
  stopifnot(length(x) == length(y))
  x[[1]] <- c(x[[1]], y[[1]][j])
  attr(x, "value") <- y[[1]][j]
  attr(x, "key") <- names(x[1])
  return(x)
}


# Grow Dictionary Function
grow_list <- function(x, y, sequential = FALSE) {
    stopifnot(length(x) == length(y))
    if (all(lengths(x) == 0)) {
        names(x) <- names(y)
        for (i in seq_len(length(y))) {
            new <- setdiff(y[[i]], x[[i]])
            if (sequential) {
                add <- head(new, 1)
            } else {
                add <- sample(new, min(length(new), 1))
            }
            x[[i]] <- add
        }
        attr(x, "value") <- unlist(x, use.names = FALSE)
        attr(x, "key") <- names(x)
    } else {
        is_exhausted <- mapply(function(x, y) length(setdiff(y, x)) == 0, x, y)
        if (all(is_exhausted)) {
            warning("No more words to sample", immediate. = TRUE, call. = FALSE)
            attr(x, "value") <- character()
            attr(x, "key") <- character()
        } else {
            i <- which(!is_exhausted)
            if (sequential) {
                i <- min(i)
            } else {
                if (length(i) > 1) {
                    i <- sample(i, 1)
                }
            }
            new <- setdiff(y[[i]], x[[i]])
            if (sequential) {
                add <- head(new, 1)
            } else {
                add <- sample(new, 1)
            }
            x[[i]] <- c(x[[i]], add)
            attr(x, "value") <- add
            attr(x, "key") <- names(x[i])
        }
    }
    return(x)
}

# assessment 
dictionary_assessment <- function(init, list_add, train_dfm, test_dfm, classes){
    for (i in seq_along(lis_add)) {
        for (j in seq_along(lis_add[[i]])) {
            lis_temp <- append_list(lis_temp, lis_add, i, j)
            
            label_dfm <- dfm_lookup(train_dfm, dictionary(lis_temp), levels = 1)
            
            map <- textmodel_newsmap(train_dfm, label_dfm)
            
            predict <- predict(map, newdata = test_dfm, confidence.fit = TRUE)
            
            predict <- as.data.frame(predict)
            
            levels(predict$class) <- c(levels(predict$class), "none")
            
            predict$class[predict$confidence.fit < 0] <- "none"
            
            predict$class <- stringr::str_remove(predict$class, pattern = "ethnicity.")
            
            predict$class <- as.factor(predict$class)
            
            results <- caret::confusionMatrix(predict$class,
                                              us_test_and_coded$icr_group,
                                              mode = "prec_recall"
            )
            
            overall_results <- as.data.frame(results$overall)
            
            t_overall <- transpose(overall_results)
            
            colnames(t_overall) <- rownames(overall_results)
            
            rownames(t_overall) <- colnames(overall_results)
            
            class_res <- results$byClass
            
            class_res <- as.data.frame(class_res)
            
            dat_temp <- data.frame(
                i = i, j = j, noise = FALSE,
                word_added = attr(lis_temp, "value"),
                topic_added = attr(lis_temp, "key"),
                coverage = get_coverage(label_dfm),
                afe = afe(train_dfm, label_dfm, smooth = 1),
                p_asian = class_res$Precision[1],
                r_asian = class_res$Recall[1],
                f1_asian = class_res$F1[1],
                p_black = class_res$Precision[2],
                r_black = class_res$Recall[2],
                f1_black = class_res$F1[2],
                p_jewish = class_res$Precision[3],
                r_jewish = class_res$Recall[3],
                f1_jewish = class_res$F1[3],
                p_la = class_res$Precision[4],
                r_la = class_res$Recall[4],
                f1_la = class_res$F1[4],
                p_muslim = class_res$Precision[5],
                r_muslim = class_res$Recall[5],
                f1_muslim = class_res$F1[5],
                accuracy = t_overall$Accuracy,
                stringsAsFactors = FALSE
            )
            
            cat(sprintf(
                "%d %d Test Class %s: Word Added %s (AFE %.3f, Coverage %.3f, Accuracy %.3f)", i, j,
                stri_trans_toupper(dat_temp$topic_added), dat_temp$word_added,
                dat_temp$afe, dat_temp$coverage
            ), "\n")
        }
    }
    
}

"%nin%" <- Negate("%in%")


