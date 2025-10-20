# Code for “SARS-CoV-2 Reinfections and Subsequent Risk of Hospital-Diagnosed Post-Acute Sequelae: A Nationwide Cohort Study”

**Link to paper:** [https://ssrn.com/abstract=5586132](https://ssrn.com/abstract=5586132)  
**DOI:** [https://doi.org/10.2139/ssrn.5586132](https://doi.org/10.2139/ssrn.5586132)

---

## Overview

This repository contains the code used to reproduce analyses from the study *“SARS-CoV-2 Reinfections and Subsequent Risk of Hospital-Diagnosed Post-Acute Sequelae: A Nationwide Cohort Study.”*

The goal of this project is to estimate the relationship between SARS-CoV-2 reinfection and risk of post-acute sequelae of COVID-19 (PASC, or long COVID) using comprehensive national register data from Denmark.  
Analyses quantify absolute and relative risks of long COVID and related outcomes (fatigue, headache), stratified by infection history, vaccination status, sex, and income level.

All data management and statistical analyses were conducted on secure servers at Statistics Denmark. This repository provides analysis scripts only.

---

## Folder Structure

| Folder | Description |
|---------|--------------|
| **baseline_cohort_identification/** | Scripts for defining the study cohort, applying inclusion/exclusion criteria, and constructing time-varying covariates (e.g., infection count, vaccination status). |
| **long_covid_analyses/** | Main analytical scripts estimating long COVID risk and absolute risk differences using cause-specific Cox models and G-computation. |
| **fatigue_and_headache_analyses/** | Secondary analyses evaluating fatigue and headache diagnoses following infection and reinfection. |
| **LICENSE** | Project license file (Apache 2.0). |
| **README.md** | Documentation for this repository. |

---


## Authors

- **Mark P. Khurana** – University of Copenhagen  
- **Mathilde Marie Brünnich Sloth** – University of Copenhagen  
- **Neil Scheidwasser** – Imperial College London  
- **Jacob Curran-Sebastian** – University of Copenhagen  
- **Christian Morgenstern** – Imperial College London  
- **Nicolas Banholzer** – ETH Zürich  
- **David Thein** – Copenhagen University Hospital  
- **Laust H. Mortensen** – University of Copenhagen  
- **Morten Rasmussen** – Statens Serum Institut  
- **Pikka Jokelainen** – Statens Serum Institut  
- **Frederik Trier Møller** – Statens Serum Institut  
- **Marc Stegger** – Statens Serum Institut  
- **Tyra Grove Krause** – Statens Serum Institut  
- **Ewan Cameron** – Curtin University  
- **David A. Duchêne** – University of Copenhagen  
- **Alexandros Katsiferis** – University of Copenhagen  
- **Samir Bhatt** – Imperial College London  

---

## License

This repository is licensed under the **Apache 2.0 License**.  
See the [LICENSE](LICENSE) file for details.

---

## Citation

If using this code, please cite the corresponding paper:

> Khurana, M. P., Sloth, M. M. B., Scheidwasser, N., Curran-Sebastian, J., Morgenstern, C., Banholzer, N., Thein, D., Mortensen, L. H., Rasmussen, M., Jokelainen, P., Møller, F. T., Stegger, M., Krause, T. G., Cameron, E., Duchêne, D. A., Katsiferis, A., & Bhatt, S. (2025). *SARS-CoV-2 Reinfections and Subsequent Risk of Hospital-Diagnosed Post-Acute Sequelae: A Nationwide Cohort Study.* SSRN. [https://ssrn.com/abstract=5586132](https://ssrn.com/abstract=5586132)

---
