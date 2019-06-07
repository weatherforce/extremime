Xvfb :0 -screen 0 1024x768x24 &
export DISPLAY=":0"
Rscript -e 'install.packages(c("TTAinterfaceTrendAnalysis", "uHMM", "DTWBI"), repos="https://cran.r-project.org/")'
