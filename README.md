# CogPsych-Data-and-Scripts
 Data and analysis for semantic prime study done by the WashU Cog Psych

**User Guide**

In this guide DURATION will stand in for the four different possible durations (48, 64, 120, 500) that can either take the form of "48 ms (short)" or just "48".

**Files:**

*FINAL-fullwordlist.csv*: A list of all words used in this experiment and their related primes. For each experiment half of the target words are studied and half are used as unrelated primes. Which words were used for each function rotated depending on the verion of the experiment listed in *Semantic_FullWordListeditAV.xlsx*.

*Semantic_FullWordListeditAV.xlsx*: The previously mentioned assignment of blocks of words as either studied or unrelated primes.

*practiceword.xlsx*: The practice study words used in the practice block of the experiment.

Other files are related to git or R environments and can be ignored.

**Directories**

*DURATION duration*: These directories contain the raw and processed data for their respective durations. In each of these directories the raw data is stored in *Raw Data (Immediate and Delayed)* and the data that has been automatically processed is stored in *Updated Processing*. Other files and folders are likely outdated, and have not been included in any of the current analyses.

*R Scripts 2022*: This directory contains the scripts used to create the analyses store in *R Results 2022*.

*R Results 2022*: This directory contains the analyzed data, in all of these files rows represent participants and values are either totals or means.
 
- *Counts_and_proportion_for_presented_primes_DURATION*:
- *Counts_and_proportion_for_presented_primes_cutoff_DURATION*:
- *Delayed_Recall_Correct_Count_By_Prime_DURATION*: