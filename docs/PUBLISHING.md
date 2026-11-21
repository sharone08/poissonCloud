# Put everything in one Git repository

The repository root **is** the R package (so `remotes::install_github()` works); other deliverables live in sub-folders that `.Rbuildignore` excludes from the package tarball.

```bash
cd poissonCloud                      # the unzipped folder
git init -b main
git add .
git commit -m "poissonCloud 2.0: digital twin for cloud capacity planning"
# create an empty repo on GitHub (web UI), then:
git remote add origin https://github.com/<your-user>/poissonCloud.git
git push -u origin main
```

* Install from GitHub: `remotes::install_github("sharone08/poissonCloud")`
* Website: GitHub -> Settings -> Pages -> Source = **GitHub Actions** (workflow `.github/workflows/pages.yml` publishes `web/`).
* CI: `.github/workflows/R-CMD-check.yaml` runs `R CMD check` on every push.
* Large files: the Excel/CSV data are about 2 MB each; no Git LFS needed.
* Before the first push: replace `Sharone Vanelle Ndomtchi`,sharonevanelle@gmail.com and `sharone08` in `DESCRIPTION`, `LICENSE`, `README.md`.
