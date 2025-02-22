# Titanic-Analysis
Exploration of Titanic dataset with cleaning, visualization, and analysis. This project was created as part of our studies. It serves as a hands-on exercise to apply concepts such as statistical analysis in R.

---

## Installation
Use the following command to install required packages:
```bash
install.packages(scan("requirements.txt", what="character"))
```

---

## Project Structure
The project is organized into the following directories:

```
Titanic-Analysis/
├── data/
│   ├── raw/            # Contains the original, unmodified Titanic dataset.
│   └── processed/      # Contains cleaned and preprocessed datasets used in the analysis.
├── scripts/
│   ├── helpers/        # Utility scripts with helper functions for data processing and visualization.
│   └── analysis/       # Scripts for the main analysis tasks, such as exploratory data analysis and modeling.
├── output/
│   ├── plots/          # Exported visualizations, such as graphs and charts.
│   ├── reports/        # Generated reports in HTML, PDF, or Markdown formats.
│   └── tables/         # Exported tables and processed data files (e.g., CSVs).
├── requirements.txt    # List of required R packages for the project.
├── .Rproj              # RStudio project file (optional).
├── .gitignore          # Specifies files and folders to ignore in version control.
├── README.md
└── LICENSE
```

---

## Description of Key Folders
### `data/`
- **`raw/`**: Contains the original dataset in its raw form. These files should not be modified.
- **`processed/`**: Stores datasets that have been cleaned or transformed for analysis.

### `scripts/`
- **`helpers/`**: Includes reusable functions and utility scripts for common tasks (e.g., data cleaning, visualization).
- **`analysis/`**: Contains analysis scripts for specific tasks (e.g., exploratory data analysis, predictive modeling).

### `output/`
- **`plots/`**: Stores generated plots and visualizations.
- **`reports/`**: Contains final reports (e.g., RMarkdown output in PDF, HTML, or other formats).
- **`tables/`**: Stores tabular outputs, such as cleaned data or summaries.

---

## Usage
1. Clone this repository:
   ```bash
   git clone <repository-url>
   ```
2. Navigate to the project directory:
   ```bash
   cd Titanic-Analysis
   ```
3. Install the required packages:
   ```bash
   install.packages(scan("requirements.txt", what="character"))
   ```

---

## License
This project is licensed under the [MIT License](LICENSE).