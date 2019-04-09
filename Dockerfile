FROM jupyter/scipy-notebook

# Make new files created in $CONDA_DIR writable by group
USER root

# Package pre-requisites
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    fonts-dejavu \
    tzdata \
    unixodbc \
    unixodbc-dev \
    r-cran-rodbc \
    gfortran \
    vim-nox \
    lftp \
    less \
    wget \
    xvfb \
    gcc && apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# R packages including IRKernel which gets installed globally.
RUN conda install --quiet --yes \
    'r-base=3.5.1' \
    'r-rodbc=1.3*' \
    'unixodbc=2.3.*' \
    'r-irkernel=0.8*' \
    'r-plyr=1.8*' \
    'r-devtools=1.13*' \
    'r-tidyverse=1.2*' \
    'r-shiny=1.2*' \
    'r-rmarkdown=1.11*' \
    'r-forecast=8.2*' \
    'r-rsqlite=2.1*' \
    'r-reshape2=1.4*' \
    'r-nycflights13=1.0*' \
    'r-caret=6.0*' \
    'r-rcurl=1.95*' \
    'r-crayon=1.3*' \
    'r-randomforest=4.6*' \
    'r-htmltools=0.3*' \
    'r-sparklyr=0.9*' \
    'r-htmlwidgets=1.2*' \
    'r-formatr=1.6*' \
    'r-hexbin=1.27*' && \
    conda clean -tipsy && \
    fix-permissions $CONDA_DIR

COPY install_uHMM.sh .
RUN bash install_uHMM.sh && rm install_uHMM.sh
# Remove X lock file so that it can be started again when running a container
RUN rm /tmp/.X0-lock

# Override startup scripts to include virtual display server
COPY start-notebook.sh /usr/local/bin/
COPY start-singleuser.sh /usr/local/bin/

USER $NB_UID

#### Python 3
RUN conda install --quiet --yes --channel conda-forge \
    jupyter jupyterlab numpy pandas \
    # Install NetCDF libraries
    xarray dask \
    # Dataviz libraries
    cartopy bokeh ipywidgets ipyleaflet \
    seaborn geopandas affine rasterio \
    # Code prettyfiers
    yapf jupyter_contrib_nbextensions && \
    conda install -c plotly plotly && \
    jupyter contrib nbextension install --user && \
    jupyter nbextension enable code_prettify/code_prettify && \
    # ipyleaflet JupyterLab extensions
    jupyter labextension install jupyter-leaflet && \
    conda install --quiet --yes --channel pyviz/label/dev pyviz && \
    conda remove --quiet --yes --force qt pyqt && \
    conda clean -tipsy

    # Plotly JupyterLab extensions
    # Avoid "JavaScript heap out of memory" errors during extension installation
RUN export NODE_OPTIONS=--max-old-space-size=4096 && \
    # Jupyter widgets extension
    jupyter labextension install @jupyter-widgets/jupyterlab-manager@0.38 --no-build && \
    # FigureWidget support
    jupyter labextension install plotlywidget@0.7.0 --no-build && \
    # offline iplot support
    jupyter labextension install @jupyterlab/plotly-extension@0.18.1 --no-build && \
    # JupyterLab chart editor support (optional)
    jupyter labextension install jupyterlab-chart-editor@1.0 --no-build && \
    # Build extensions (must be done to activate extensions since --no-build is used above)
    jupyter lab build && \
    # Unset NODE_OPTIONS environment variable
    unset NODE_OPTIONS
