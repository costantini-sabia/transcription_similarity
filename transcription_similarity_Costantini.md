## Overview

Code in R that allows researchers to quantify the similarity between two
transcribed and annotated texts, considering differences in transcripts
with respect to word choice, word order, utterance segmentation, and
CLAN annotation marks (i.e., repetitions, revisions, fillers, fragmented
speech, paraphasias and onomatopoeas).

## Method

To assess consistency among transcribers along these dimensions, I used
a modified Levenshtein distance algorithm. The standard Levenshtein
distance algorithm measures the difference between two strings by
calculating the minimum number of single-character edits, that is
insertions, deletions, or substitutions, needed to transform one string
into the other (e.g., transforming ‘kitten’ into ‘mitten’ requires
substituting the ‘k’ with an ‘m,’ which counts as a single edit
operation). I instead measured the number of operations required to
transform an entire text (at the word level and at the annotation level)
into another, additionally incorporating adjacent word swaps as an edit
operation to capture differences in word order across transcriptions.

The code estimates inter-rater transcription similarity in the following
manner. Each pair of transcriptions is tokenized into words and full
stops (denoting C-unit boundaries). The code estimates the minimum
number of edit operations—insertions, deletions, substitutions, and
adjacent word swaps—required to make the two transcriptions identical.
This edit count is divided by the maximum number of edits (i.e., the
length of the longer transcription) to obtain a distance measure between
the two texts. Subtracting this distance from 1 produces a similarity
score ranging from 0 to 1, where higher scores indicate greater
similarity across transcriptions in word usage, word order, and
placement of C-unit boundaries. The code subsequently tokenizes all CLAN
annotation markers, and calculates similarity of annotations across
transcriptions using a similar approach (i.e., dividing the number of
annotation-level edits by the maximum number of annotation tokens in
either transcription). Finally, word-level and annotation-level
similarity scores are averaged to produce an overall similarity score
for each transcription pair.

# Set Up

Create a directory on your laptop where you store the transcriptions you
want to compare. Label these transcriptions in pairs like
`textp01_1.txt` and `textp01_2.txt`, etc. The code will automatically
compare each `_1` file with its corresponding `_2` file in the folder.

The data used to reproduce these examples are included in the GitHub
repository for this project. All speech transcripts are fictitious and
are provided for demonstration purposes only.

## Load Packages

    rm(list = ls())
    library(stringr)
    library(stringdist)

# Pipeline

## Step 1: Text Cleaning

We loop through a directory containing CHAT-formatted transcription
files and preprocess each file by removing non-linguistic metadata
(e.g., participant information and sample duration), speaker tier
markers (`*PAR:`), `@End` markers, and commas, which do not carry
linguistic content. Full stops are preserved, as they encode linguistic
information (i.e., utterance boundaries). This preprocessing step
outputs cleaned `.txt` files stored in the same directory as the
corresponding input files, saved in a subfolder named `cleaned_txt`.

    # load cleaning function
    source("clean_cha_file.R")

    # --- Set paths ---
    input_folder <- "/Users/sabiacostantini/Desktop/costantini-sabia.github.io/projects/R" # * CHANGE this with the path to your folder
    output_folder <- file.path(input_folder, "cleaned_txt")

    if (!dir.exists(output_folder)) dir.create(output_folder)

    # get all .cha files in the input folder
    cha_files <- list.files(input_folder, pattern = "\\.cha$", full.names = TRUE)

    # clean all files
    cleaned_texts <- lapply(cha_files, clean_cha_file, output_folder = output_folder)

    ## ✔ Cleaned text saved to: /Users/sabiacostantini/Desktop/costantini-sabia.github.io/projects/R/cleaned_txt/text1_1.txt

    ## ✔ Cleaned text saved to: /Users/sabiacostantini/Desktop/costantini-sabia.github.io/projects/R/cleaned_txt/text1_2.txt

    ## ✔ Cleaned text saved to: /Users/sabiacostantini/Desktop/costantini-sabia.github.io/projects/R/cleaned_txt/text2_1.txt

    ## ✔ Cleaned text saved to: /Users/sabiacostantini/Desktop/costantini-sabia.github.io/projects/R/cleaned_txt/text2_2.txt

## Demonstration: Text Cleaning

Let’s look at how the function works and how the output text would look
like.

    # define path to example file
    demo_file <- file.path(input_folder, "text1_1.cha")
    demo_cleaned_file <- file.path(output_folder, "text1_1.txt")

    # look at the raw .cha file
    cat(readChar(demo_file, file.info(demo_file)$size), "\n")

    ## @UTF8
    ## @Window: 79_77_668_685_-1_-1_174_0_253_0
    ## @Begin
    ## @Languages:  eng
    ## @Participants:   PAR xxx
    ## @Options:    IPA
    ## @ID: eng|xxx|PAR|||||xxx||xxx|
    ## @Time Duration:  00:00:00-00:00:30
    ## @Comment:    xxx_CatRescue.wav
    ## 
    ## *PAR:    I see a lady.
    ## *PAR:    she is running on the beach.
    ## *PAR:    she is &+uhm singing a beautiful song.
    ## @End
    ## 

    # apply cleaning function
    cleaned <- clean_cha_file(demo_file, output_folder)

    ## ✔ Cleaned text saved to: /Users/sabiacostantini/Desktop/costantini-sabia.github.io/projects/R/cleaned_txt/text1_1.txt

    # look at the cleaned output
    cat(readChar(demo_cleaned_file, file.info(demo_cleaned_file)$size), "\n")

    ## I see a lady. she is running on the beach. she is &+uhm singing a beautiful song.
    ## 

## Step 2: Compute Transcription Similarity

We compute edit distance between two transcriptions at two levels: first
at the word level (tokenizing words and full stops as well) and then at
the annotation level. Any annotated text is excluded from the word-level
calculation, as it is counted separately in the annotation-level
similarity. We calculate the overall between-text similarity as the
average of word-level and annotation-level similarity scores. We save
the overall similarity score, word-level and annotation-level similarity
scores, and the corresponding edit operations in a `.csv` file in the
output folder.

    # load similarity functions
    source("text_similarity.R")
    source("compare_pairs.R")

    # loop over files to compare transcription pairs
    files_a <- list.files(output_folder, pattern = "_1\\.txt$", full.names = TRUE)
    bases <- sub("_1\\.txt$", "", basename(files_a))

    results <- do.call(
      rbind,
      lapply(bases, function(b) {
        fa <- file.path(output_folder, paste0(b, "_1.txt"))
        fb <- file.path(output_folder, paste0(b, "_2.txt"))
        if (!file.exists(fb)) return(NULL)

        ta <- readChar(fa, file.info(fa)$size, useBytes = TRUE)
        tb <- readChar(fb, file.info(fb)$size, useBytes = TRUE)

        compare_pair(ta, tb, paste0(b, "_1", b, "_2"))
      })
    )

    # --- Save and display results ---
    output_file <- file.path(output_folder, "pairs_similarity_results.csv")
    write.csv(results, output_file, row.names = FALSE)

    results_display <- read.csv(output_file)
    knitr::kable(results_display, digits = 2)

<table>
<colgroup>
<col style="width: 2%" />
<col style="width: 3%" />
<col style="width: 2%" />
<col style="width: 3%" />
<col style="width: 74%" />
<col style="width: 12%" />
</colgroup>
<thead>
<tr>
<th style="text-align: left;">pair</th>
<th style="text-align: right;">combined_similarity</th>
<th style="text-align: right;">word_similarity</th>
<th style="text-align: right;">annotation_similarity</th>
<th style="text-align: left;">word_operations</th>
<th style="text-align: left;">annotation_operations</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">text1_1text1_2</td>
<td style="text-align: right;">0.70</td>
<td style="text-align: right;">0.90</td>
<td style="text-align: right;">0.5</td>
<td style="text-align: left;">MATCH: ‘i’ | MATCH: ‘see’ | MATCH: ‘a’ |
MATCH: ‘lady’ | MATCH: ‘.’ | MATCH: ‘she’ | MATCH: ‘is’ | MATCH:
‘running’ | MATCH: ‘on’ | MATCH: ‘the’ | INSERT: ‘sunny’ | SUBSTITUTE:
‘beach’ → ‘shore’ | MATCH: ‘.’ | MATCH: ‘she’ | MATCH: ‘is’ | MATCH:
‘singing’ | MATCH: ‘a’ | MATCH: ‘beautiful’ | MATCH: ‘song’ | MATCH:
‘.’</td>
<td style="text-align: left;">INSERT annotation: ‘<a lady> [//]’ | MATCH
annotation: ‘&amp;+uhm’</td>
</tr>
<tr>
<td style="text-align: left;">text2_1text2_2</td>
<td style="text-align: right;">0.44</td>
<td style="text-align: right;">0.88</td>
<td style="text-align: right;">0.0</td>
<td style="text-align: left;">MATCH: ‘i’ | MATCH: ‘see’ | MATCH: ‘a’ |
MATCH: ‘man’ | MATCH: ‘.’ | MATCH: ‘he’ | MATCH: ‘is’ | SUBSTITUTE:
‘laughing’ → ‘running’ | MATCH: ‘.’ | MATCH: ‘he’ | MATCH: ‘likes’ |
MATCH: ‘to’ | MATCH: ‘read’ | INSERT: ‘old’ | MATCH: ‘books’ | MATCH:
‘.’</td>
<td style="text-align: left;">INSERT annotation: ‘&amp;-uh’ | INSERT
annotation: ‘&amp;+ru’</td>
</tr>
</tbody>
</table>

## Demonstration: Text Similarity

The example below illustrates how the similarity function works. The two
texts are expected to differ with respect to an annotation marker
(present in `text2` only) and a full stop (present in `text1` only). At
the word level, we expect a similarity score of 0.89 (8 tokens match out
of 9). At the annotation level, we expect a similarity score of 0 (the
single annotation in text 1 is not present in text 2). The mean
similarity is 0.44.

    # load the similarity functions - to see functions' code, see function panel
    source("text_similarity.R")

    text1 <- 'I see a lady. she is running.'
    text2 <- 'I see <a lady> [/]. she is running' # we expect these two texts to vary with respect to annotation mark (text 2) and a full stop (text 1).

    combined_similarity(text1, text2)

    ## 
    ## --- WORD EDITS ---
    ## 1. MATCH: 'i'
    ## 2. MATCH: 'see'
    ## 3. MATCH: 'a'
    ## 4. MATCH: 'lady'
    ## 5. MATCH: '.'
    ## 6. MATCH: 'she'
    ## 7. MATCH: 'is'
    ## 8. MATCH: 'running'
    ## 9. DELETE: '.'
    ## 
    ## --- ANNOTATION EDITS ---
    ## 1. INSERT annotation: '<a lady> [/]'
    ## 
    ## --- SIMILARITY SCORES ---
    ## Word similarity:        0.889 
    ## Annotation similarity:  0 
    ## Combined similarity:    0.444
