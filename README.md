**# Reproducible_LDA_Library_Circulation**
Reproducible LDA Topic Modeling for Library Book Circulation Data
**# Overview**
This repository provides a reproducible workflow for applying **Latent Dirichlet Allocation (LDA)** topic modeling in **R** to library physical book circulation data. The primary goal is to support libraries and similar institutions in analyzing thematic structure and thematic change over time using their own textual and circulation datasets.
The methodology is presented in an end‑to‑end, transparent manner, from data preparation and preprocessing through model fitting, evaluation, and interpretation. While the workflow is demonstrated using a specific institutional dataset, the approach is intentionally designed to be adaptable and reusable across library contexts.

**# Research Context**
This codebase accompanies the research paper:
**# Understanding the Thematic Clusters and Evolution of Research Interests at a Library: A Reproducible Methodology**

The paper’s primary contribution is the methodological framework for applying LDA topic modeling in a library setting. Results derived from local circulation data serve as a case study illustrating the workflow, rather than as broadly generalizable findings.

**# Intended Use**
This repository is intended for:
1. Academic and research libraries
2. Library assessment and analytics teams
3. Researchers interested in topic modeling of library data
4. Institutions seeking reproducible text analysis workflows

**# The workflow is suitable for analyzing:**
1. Physical book circulation data
2. Bibliographic metadata (e.g., titles, subject headings)
3. Longitudinal borrowing trends

**# Data Requirements**
Due to privacy and licensing considerations, raw circulation data are not included in this repository.
To replicate or adapt the methodology, users should provide a dataset containing:

Book‑level bibliographic text, such as:
1. Title
2. Subject headings
3. Abstracts or descriptive notes (if available)

**# Circulation information, such as:**
1. Borrowing counts or transactions
2. Time‑based fields (e.g., year or date of circulation)

**# Workflow Overview**
The analytical workflow consists of the following stages:

1. Load the necesary libraries
2. Text preprocessing
3. LDA topic modeling
  a. Determine optimal number of topics
  b. Fit the LDA model
  c. Calculate the beta-probability of a word given a topic
  d. Calculate the beta variance
  e. Calculate the topic distribution per document
  f. Calculate the topic trends over time


