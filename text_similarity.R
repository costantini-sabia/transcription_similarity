#Functions to estimate word-level, annotation-level, and combined similarity
# -------------------------------
# 1. CLEAN WORDS (remove annotated text, other markers, and tokenise)
# -------------------------------
clean_words <- function(text) {
  # Remove annotated fragments but keep actual words
  text <- str_replace_all(text, "&[-+][A-Za-z0-9_]+", "")       # remove &+ / &- fragments
  text <- str_replace_all(text, "\\[/{1,2}\\]", "")             # remove [/] or [//]
  text <- str_replace_all(text, "<[^>]+>\\s*\\[/{1,2}\\]", "\\1") # keep content inside <...> but remove [/].
  text <- str_replace_all(text, "\\bxxx\\b", "")                # remove 'xxx'
  text <- str_replace_all(text, "\\[:\\s*[A-Za-z0-9_]+\\]", "") # remove [: word]
  text <- str_replace_all(text, "\\b[A-Za-z0-9_]+@o\\b", "")    # remove word@o
  text <- gsub("\\.", " . ", text)                              # separate periods
  text <- str_trim(text)                                        # trim whitespace
  tokens <- unlist(strsplit(text, "\\s+"))                      # split into tokens
  tokens <- tokens[tokens != ""]                                # remove empty tokens
  tolower(tokens)                                               # convert to lowercase
}


# -------------------------------
# 2. WORD EDIT DISTANCE WITH SWAPS & PRINT OPERATIONS
# -------------------------------
edit_distance_with_swaps <- function(text1, text2) {
  words1 <- clean_words(text1)
  words2 <- clean_words(text2)
  
  m <- length(words1)
  n <- length(words2)
  dp <- array(Inf, dim = c(m + 1, n + 1))
  
  for (i in 1:(m + 1)) dp[i, 1] <- i - 1
  for (j in 1:(n + 1)) dp[1, j] <- j - 1
  
  for (i in 2:(m + 1)) {
    for (j in 2:(n + 1)) {
      if (words1[i - 1] == words2[j - 1]) {
        dp[i, j] <- dp[i - 1, j - 1]
      } else {
        dp[i, j] <- 1 + min(
          dp[i - 1, j], dp[i, j - 1], dp[i - 1, j - 1]
        )
        if (i > 2 && j > 2 &&
            words1[i - 1] == words2[j - 2] &&
            words1[i - 2] == words2[j - 1]) {
          dp[i, j] <- min(dp[i, j], dp[i - 2, j - 2] + 1)
        }
      }
    }
  }
  
  # Traceback operations
  operations <- c()
  i <- m + 1
  j <- n + 1
  
  while (i > 1 || j > 1) {
    if (i > 1 && j > 1 && words1[i - 1] == words2[j - 1]) {
      operations <- c(paste0("MATCH: '", words1[i - 1], "'"), operations)
      i <- i - 1; j <- j - 1
    } else if (i > 2 && j > 2 &&
               words1[i - 1] == words2[j - 2] &&
               words1[i - 2] == words2[j - 1] &&
               dp[i, j] == dp[i - 2, j - 2] + 1) {
      operations <- c(paste0("SWAP: '", words1[i - 2], "' ↔ '", words1[i - 1], "'"), operations)
      i <- i - 2; j <- j - 2
    } else if (i > 1 && j > 1 && dp[i, j] == dp[i - 1, j - 1] + 1) {
      operations <- c(paste0("SUBSTITUTE: '", words1[i - 1], "' → '", words2[j - 1], "'"), operations)
      i <- i - 1; j <- j - 1
    } else if (i > 1 && dp[i, j] == dp[i - 1, j] + 1) {
      operations <- c(paste0("DELETE: '", words1[i - 1], "'"), operations)
      i <- i - 1
    } else if (j > 1 && dp[i, j] == dp[i, j - 1] + 1) {
      operations <- c(paste0("INSERT: '", words2[j - 1], "'"), operations)
      j <- j - 1
    }
  }
  
  distance <- dp[m + 1, n + 1]
  max_len <- max(m, n, 1)
  similarity <- 1 - distance / max_len
  
  list(similarity = similarity, operations = operations)
}

# -------------------------------
# 3. EXTRACT ANNOTATION TOKENS
# -------------------------------
extract_annotations <- function(text) {
  pattern <- "
    &[-+][A-Za-z0-9_]+ |            # &+ or &- (fragment or filler)
    <[^>]+>\\s*\\[/{1,2}\\] |       # <...> [/] or [//] (revised or repeated pharase)
    [A-Za-z0-9_]+\\s*\\[/{1,2}\\] | # word followed by [/] or [//] (revised or repeated word)
    xxx |                           # 'xxx' (unintellegible)
    \\[:\\s*[A-Za-z0-9_]+\\] |      # [: word] (paraphasia)
    [A-Za-z0-9_]+@o                 # word@o (anom)
  "
  out <- str_extract_all(text, regex(pattern, comments = TRUE))[[1]]
  out <- out[out != ""]  # remove empty strings
  if (length(out) == 0) return(character(0))
  return(out)
}


# -------------------------------
# 4. ANNOTATION SIMILARITY & PRINT OPERATIONS
# -------------------------------
annotation_similarity <- function(text1, text2) {
  ann1 <- extract_annotations(text1)
  ann2 <- extract_annotations(text2)
  
  # If either annotation list is empty, create insert/delete operations
  if (length(ann1) == 0 && length(ann2) == 0) {
    return(list(similarity = 1, operations = c()))  # nothing to edit
  } else if (length(ann1) == 0) {
    operations <- paste0("INSERT annotation: '", ann2, "'")
    similarity <- 0
    return(list(similarity = similarity, operations = operations))
  } else if (length(ann2) == 0) {
    operations <- paste0("DELETE annotation: '", ann1, "'")
    similarity <- 0
    return(list(similarity = similarity, operations = operations))
  }
  m <- length(ann1)
  n <- length(ann2)
  dp <- array(Inf, dim = c(m + 1, n + 1))
  
  for (i in 1:(m + 1)) dp[i, 1] <- i - 1
  for (j in 1:(n + 1)) dp[1, j] <- j - 1
  
  for (i in 2:(m + 1)) {
    for (j in 2:(n + 1)) {
      if (ann1[i - 1] == ann2[j - 1]) {
        dp[i, j] <- dp[i - 1, j - 1]
      } else {
        dp[i, j] <- 1 + min(
          dp[i - 1, j], dp[i, j - 1], dp[i - 1, j - 1]
        )
        if (i > 2 && j > 2 &&
            ann1[i - 1] == ann2[j - 2] &&
            ann1[i - 2] == ann2[j - 1]) {
          dp[i, j] <- min(dp[i, j], dp[i - 2, j - 2] + 1)
        }
      }
    }
  }
  
  operations <- c()
  i <- m + 1
  j <- n + 1
  while (i > 1 || j > 1) {
    if (i > 1 && j > 1 && ann1[i - 1] == ann2[j - 1]) {
      operations <- c(paste0("MATCH annotation: '", ann1[i - 1], "'"), operations)
      i <- i - 1; j <- j - 1
    } else if (i > 2 && j > 2 &&
               ann1[i - 1] == ann2[j - 2] &&
               ann1[i - 2] == ann2[j - 1] &&
               dp[i, j] == dp[i - 2, j - 2] + 1) {
    } else if (i > 1 && j > 1 && dp[i, j] == dp[i - 1, j - 1] + 1) {
      operations <- c(paste0("SUBSTITUTE annotation: '", ann1[i - 1], "' → '", ann2[j - 1], "'"), operations)
      i <- i - 1; j <- j - 1
    } else if (i > 1 && dp[i, j] == dp[i - 1, j] + 1) {
      operations <- c(paste0("DELETE annotation: '", ann1[i - 1], "'"), operations)
      i <- i - 1
    } else if (j > 1 && dp[i, j] == dp[i, j - 1] + 1) {
      operations <- c(paste0("INSERT annotation: '", ann2[j - 1], "'"), operations)
      j <- j - 1
    }
  }
  
  distance <- dp[m + 1, n + 1]
  max_len <- max(m, n, 1)
  similarity <- 1 - distance / max_len
  
  list(similarity = similarity, operations = operations)
}

# -------------------------------
# 5. COMBINED SIMILARITY
# -------------------------------
combined_similarity <- function(text1, text2) {
  # -----------------------------
  # Step 0: Strip annotation markup but keep words
  # -----------------------------
  strip_annotation <- function(text) {
    gsub("<([^>]+)>\\s*\\[/{1,2}\\]", "\\1", text)
  }
  clean_text1 <- strip_annotation(text1)
  clean_text2 <- strip_annotation(text2)
  
  # -----------------------------
  # Step 1: Word-level similarity
  # -----------------------------
  words1 <- clean_words(clean_text1)
  words2 <- clean_words(clean_text2)
  
  word_res <- edit_distance_with_swaps(paste(words1, collapse=" "), 
                                       paste(words2, collapse=" "))
  
  # -----------------------------
  # Step 2: Annotation-level similarity
  # -----------------------------
  # Compare annotation spans directly
  extract_annotations <- function(text) {
    regmatches(text, gregexpr("<[^>]+>\\s*\\[/{1,2}\\]", text))[[1]]
  }
  ann1 <- extract_annotations(text1)
  ann2 <- extract_annotations(text2)
  
  ann_res <- annotation_similarity(text1, text2)
  
  # -----------------------------
  # Step 3: Print edits
  # -----------------------------
  cat("\n--- WORD EDITS ---\n")
  if(length(word_res$operations) == 0) cat("No word edits needed.\n") else
    for (i in seq_along(word_res$operations)) cat(i, ". ", word_res$operations[i], "\n", sep = "")
  
  cat("\n--- ANNOTATION EDITS ---\n")
  if(length(ann_res$operations) == 0) cat("No annotation edits needed.\n") else
    for (i in seq_along(ann_res$operations)) cat(i, ". ", ann_res$operations[i], "\n", sep = "")
  
  # -----------------------------
  # Step 4: Combined similarity
  # -----------------------------
  combined <- (word_res$similarity + ann_res$similarity)/2
  cat("\n--- SIMILARITY SCORES ---\n")
  cat("Word similarity:       ", round(word_res$similarity,3), "\n")
  cat("Annotation similarity: ", round(ann_res$similarity,3), "\n")
  cat("Combined similarity:   ", round(combined,3), "\n")
  
#  return(list(combined = combined, word_res = word_res, ann_res = ann_res))
}


