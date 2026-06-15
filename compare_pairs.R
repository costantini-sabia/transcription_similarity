# function that takes two transcription texts and a pair name, computes similarity at two levels—word-level and annotation-level—and returns a summary data frame with similarity scores and edit operations.
# -------------------------------
# 1. COMPARE PAIR FUNCTION 
# -------------------------------
compare_pair <- function(text1, text2, pair_name) {
  
  strip_ann <- function(x)
    gsub("<([^>]+)>\\s*\\[/{1,2}\\]", "\\1", x)
  
  word_res <- edit_distance_with_swaps(
    strip_ann(text1),
    strip_ann(text2)
  )
  
  ann_res <- annotation_similarity(text1, text2)
  
  combined <- (word_res$similarity + ann_res$similarity) / 2
  
  data.frame(
    pair = pair_name,
    combined_similarity = round(combined, 2),
    word_similarity = round(word_res$similarity, 2),
    annotation_similarity = round(ann_res$similarity, 2),
    word_operations = paste(word_res$operations, collapse = " | "),
    annotation_operations = paste(ann_res$operations, collapse = " | "),
    stringsAsFactors = FALSE
  )
}