#!/bin/bash
# Copyright (c) Jupyter Development Team.
# Distributed under the terms of the Modified BSD License.

set -e

echo "Start virtual display server required by uHMM"
Xvfb :0 -screen 0 1024x768x24 &
export DISPLAY=":0"

if [[ ! -z "${JUPYTERHUB_API_TOKEN}" ]]; then
  # launched by JupyterHub, use single-user entrypoint
  exec /usr/local/bin/start-singleuser.sh "$@"
elif [[ ! -z "${JUPYTER_ENABLE_LAB}" ]]; then
  . /usr/local/bin/start.sh jupyter lab "$@"
else
  . /usr/local/bin/start.sh jupyter notebook "$@"
fi
