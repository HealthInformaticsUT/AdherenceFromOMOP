
# Adherence from OMOP

Calculate adherence from OMOP CDM

## Overview

Adherence is a concept that indicates how well does the patient follow
the healthcare provider’s recommendation. Medication adherence
specifically shows if the patient follows their medication regimen or
are there any gaps. This is important for medications which are meant to
be consumed for chronic diseases, where the efficacy of the treatment is
closely correlated with the continuity of medication consumption. One
possibility to measure adherence is secondary database analysis.

AdherenceFromOMOP contains functions to measure chronic medication
adherence using drug_exposure table in data mapped to the OMOP Common
Data Model. The package supports:

- generating drug exposure records for cohorts and/or specific
  medications

- calculating medication adherence for observation period

- calculating medication adherence using sliding window approach to view
  specific periods in observation period

- add cohort features as columns to data returned by adherence functions

- view the number of people in areas of interest

