#!/usr/bin/env bash

today=$(date "+%F")
dir=$(dirname "$0")
current_year=$(date "+%Y")

racket -y ${dir}/quotes-extract.rkt -p "$1"
racket -y ${dir}/quotes-transform-load.rkt -p "$1"

7zr a /var/local/mstar/quotes/${current_year}/${today}.7z /var/local/mstar/quotes/${today}/*.json
