
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GPT Models for Identifying Protest Events in News Articles

<!-- badges: start -->
<!-- badges: end -->

This repository houses the scripts I use to build GPT models designed to
identify protest events in news articles.

> TODO: Convert to R.

## Out-of-the-box classification

You can clean and run the pretrained GPT model by first cleaning your
dataset using `munge/01_clean_text_data.R` and then running the GPT
model over that cleaned dataset using
`models/pretrained_text-davinci-003.R`.

## Fine-tuned model

### Set up your system

This process requires both R and python. You will need to run the
following in a python terminal to set your computer up to finetune the
model.

Download `openai`:

    pip install --upgrade openai

Set your API key:

    export OPENAI_API_KEY="<OPENAI_API_KEY>"

### Read in and clean annotated data

The human-annotated data set is stored in `data-raw`. Files to clean
this data set for use are stored in `munge`.

1.  First run `01_clean_text_data.R` to clean the raw article text data.

2.  These data then need to be converted to JSON line format. Run
    `02_convert_text_to_JSONL.R` to create a properly structured .json
    file. The resulting cleaned .json file is stored in `data`.

In a terminal, run the OpenAI [CLI data preparation
tool](https://platform.openai.com/docs/guides/fine-tuning/cli-data-preparation-tool).

Run:

    openai tools fine_tunes.prepare_data -f <LOCAL_FILE>

> The CLI data preparation tool will validate your training data. Make
> sure to read its output: it will let you know if any of your data are
> incorrectly formatted.

### Fine tune the model

In your terminal, run the following:

    openai api fine_tunes.create -t /Users/harrietgoers/Documents/gpt_protest/data/training_cleaned.json -m ada --suffix "amar protest"

### Access your fine tuned model

    openai api completions.create -m <FINE_TUNED_MODEL> -p <YOUR_PROMPT>

### Assess your model’s performance

    openai api fine_tunes.create \
      -t <TRAIN_FILE_ID_OR_PATH> \
      -v <VALIDATION_FILE_OR_PATH> \
      -m <MODEL> \
      --compute_classification_metrics \
      --classification_n_classes 2 \
      --classification_positive_class <POSITIVE_CLASS_FROM_DATASET>
