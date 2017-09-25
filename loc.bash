#!/bin/bash

(find ./src -name "*.hs" -print0 | xargs -0 cat) | wc -l
