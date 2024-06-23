# Analyzing the Growth of an Emerging Artist on Spotify

## Overview
This project, developed as part of a university course at Università degli Studi di Padova, analyzes the growth trajectory of an emerging artist on Spotify from January 2021 to October 2023. Key factors such as streams, playlist reach, and follower counts are examined using time series forecasting models like TSLM, ARIMA, and GBM.

## Objectives
- Investigate the factors influencing the artist's growth.
- Identify strengths and weaknesses.
- Suggest strategies for enhancing visibility and audience engagement.

## Methodology
1. **Data Collection and Preparation**: Data was gathered from Spotify for Artists, cleaned, and transformed from daily to weekly metrics.
2. **Exploratory Data Analysis (EDA)**: Understanding relationships between variables and growth patterns.
3. **Modeling**: Applying various models (TSLM, ARIMA, GBM, Holt’s Exponential Smoothing) to capture growth dynamics.

## Key Findings
- Private playlists significantly impact stream volatility.
- Regular releases and active promotion are crucial for sustained growth.
- GBM + ARIMA is effective for medium to long-term predictions, while Damped Holt excels in short-term forecasts.

## Future Work
Incorporate social media activity and collaborations for a comprehensive analysis of growth dynamics.

## Team
- Federico Chiarello
- Andrea Marinelli
- Giorgia Rinaldi

## Institution
Università degli Studi di Padova

## Repository Contents
- `data/`: Contains the dataset used for analysis.
- `Playlist Analysis.R`: R script for analyzing playlist data.
- `Spotify Data Analysis.R`: R script for analyzing Spotify data.
- `Slides.pdf`: Presentation slides summarizing the project.

## License
This project is licensed under the MIT License.

---

For more details, visit the [project repository](https://github.com/andrea3425/business_project/).
