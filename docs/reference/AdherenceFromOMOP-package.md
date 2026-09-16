# AdherenceFromOMOP: Calculate medication adherence using data from OMOP CDM

Adherence is a concept that indicates how well does the patient follow
the healthcare provider's recommendation. Medication adherence
specifically shows if the patient follows their medication regimen or
are there any gaps. This is important for medications which are meant to
be consumed for chronic diseases, where the effecacy of the treatment is
closely correlated with the continuity of medication consumption. One
possibility to measure adherence is secondary database analysis. OMOP
common data model is a standardised model, which provides a clear
structure for healthcare data. It is possible to use OMOP CDM
drug_exposure data (which consists of purchased medication records) to
measure chronic medication adherence. This package makes it easy to
filter records where the subject has purchased chronic medication,
calculate the adherence for said people grouped by medication and used
year and visualize it with an interactive dashboard.

## See also

Useful links:

- <https://healthinformaticsut.github.io/AdherenceFromOMOP/index.html>

- <https://github.com/HealthInformaticsUT/AdherenceFromOMOP>,

- Report bugs at
  <https://github.com/HealthInformaticsUT/AdherenceFromOMOP/issues>

## Author

**Maintainer**: Greete Kelli Aava <greete.kelli.aava@ut.ee>

Authors:

- Greete Kelli Aava <greete.kelli.aava@ut.ee>

- Johannes Holm <yourself@somewhere.net>
