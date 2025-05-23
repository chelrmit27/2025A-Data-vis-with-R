#!/bin/bash
curl -L -o quarto.deb https://github.com/quarto-dev/quarto-cli/releases/download/v1.4.550/quarto-1.4.550-linux-amd64.deb
dpkg -i quarto.deb
