# IPL-DRS-Umpire-Accuracy-Analysis

🔍 Project Overview
#This project analyzes on-field umpire decision accuracy in the Indian Premier League (IPL) using Decision Review System (DRS) outcomes from 2018–2025. DRS decisions are used as an external benchmark to evaluate whether an umpire’s original call was correct or incorrect.

❓ Research Question
Did on-field umpire accuracy change after the introduction of enhanced umpire training programs around 2021?

🧠 Methodology
- Filtered IPL DRS review data (2018–2025)
- Defined:
      - Umpire Correct → Decision overturned by DRS
      - Umpire Wrong → Decision upheld by DRS
- Grouped seasons into:
      - 2018–2021 (pre-training)
      - 2022–2025 (post-training)
- Visualized accuracy using percentage-based bar charts
- Conducted a two-proportion z-test
- Built predictive models:
       - Logistic regression
       - Random forest classifier

📈 Key Findings
- Umpire decisions are correct in the majority of reviewed cases (~60–70%).
- Post-2021 seasons show a modest improvement in accuracy compared to earlier seasons.
- Statistical testing suggests a difference in proportions, though causality cannot be conclusively established.
⚠️ Limitations
- Only reviewed decisions are observed (selection bias).
- Multiple factors beyond training may influence accuracy (technology, game pace, rule changes).
🛠 Technologies Used
- R
- ggplot2
- dplyr
- caret
- randomForest
