# GitHub Repository Setup Instructions

## Current Status
Your local Git repository is initialized and ready. All analysis files are organized in the proper structure.

## Step 1: Create GitHub Repository Online

1. Go to [GitHub.com](https://github.com) and sign in
2. Click the green "New" button or "+" in the top right corner
3. Repository settings:
   - **Repository name**: `social-frames-analysis`
   - **Description**: `Analysis of social frames of reference in explore-exploit decision-making in non-human primates`
   - **Visibility**: Choose Public or Private
   - **Important**: Do NOT check "Add a README file" (we already have one)
   - **Important**: Do NOT add .gitignore or license (we already have them)
4. Click "Create repository"

## Step 2: Connect Your Local Repository to GitHub

After creating the repository, GitHub will show you commands. Run these in your terminal:

```bash
git remote add origin https://github.com/hildieleyser/social-frames-analysis.git
git branch -M main
git push -u origin main
```

**Replace YOURUSERNAME with your actual GitHub username**

## Step 3: Add Your Dataset

1. Copy your `Explore Exploit Dataset.csv` file into the `data/` directory
2. Add and commit it:
```bash
git add data/
git commit -m "Add behavioral dataset"
git push
```

## Step 4: Verify Everything Works

Your repository should now contain:
- Complete analysis scripts (Python and R)
- Documentation and methodology
- Proper directory structure
- All necessary configuration files

## What You Have Ready for GitHub

### Analysis Files
- `analysis/Social_Frames_Python_Analysis.ipynb` - Google Colab compatible notebook
- `analysis/Comprehensive_Analysis_Notebook.R` - Complete R analysis
- `analysis/Social_Frames_Analysis_Complete.R` - Extended R analysis

### Documentation
- `README.md` - Main project documentation
- `docs/methodology.md` - Detailed methodology
- `requirements.txt` - Python dependencies
- `LICENSE` - MIT license

### Directory Structure
```
├── README.md
├── requirements.txt
├── LICENSE
├── .gitignore
├── data/
│   └── README.md
├── analysis/
│   ├── Social_Frames_Python_Analysis.ipynb
│   ├── Comprehensive_Analysis_Notebook.R
│   └── Social_Frames_Analysis_Complete.R
├── results/
│   └── figures/
├── docs/
│   └── methodology.md
```

## Troubleshooting

If you get an error about the remote already existing:
```bash
git remote remove origin
git remote add origin https://github.com/YOURUSERNAME/social-frames-analysis.git
```

If you get permission denied:
- Make sure you're logged into GitHub
- Check that the repository name matches exactly
- Verify your GitHub username in the URL

## Next Steps

1. Follow the setup instructions above
2. Add your dataset to the data/ directory
3. Run the analysis notebooks to generate results
4. Share the repository link for collaboration or publication

Your repository will be ready for:
- Collaborative research
- Code sharing
- Publication supplementary materials
- Reproducible science 