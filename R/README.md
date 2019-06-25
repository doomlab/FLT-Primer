# Analysis

You will want to run the steps in the following order:

- `dependencies.R` - this file includes all the libraries necessary for the processing pipeline.
- `importData.R` - this file includes the import of the original cue-answer data.
- `spellCheck.R` - this file includes the steps to creating a spelling dictionary and updating spellings in your answers.
- `lemmatization.R` - this file includes the steps to creating the lemmas for each token and cleaning up output from TreeTagger.
- `stopWordRemoval.R` - this file includes information on how to remove stop words from the lemmatized data. 
- `multiwordSequencies.R` - this file shows how one might create multi-word sequences for a data combination approach.
- `bagOfWords.R` - this file shows how to process the final data using a bag of words approach. 