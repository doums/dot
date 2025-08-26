#!/bin/bash

rm -rf .deps/ build/
make CMAKE_BUILD_TYPE=Release CMAKE_INSTALL_PREFIX=/usr/local
sudo make install
