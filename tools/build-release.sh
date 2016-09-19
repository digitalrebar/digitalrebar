#!/usr/bin/env bash

set -e

branch="$(git symbolic-ref -q HEAD)"
branch="${branch##refs/heads/}"
branch="${branch:-latest}"

DR_TAG="$branch"
if [[ $1 = tag ]]; then
    git fetch --all -t
    DR_TAG="$(go run go/version.go)"
    tag_ref=$(git show-ref "refs/tags/$DR_TAG")
    if [[ $tag_ref ]]; then
        echo "Tag $DR_TAG already exists at commit $tag_ref"
        exit 1
    fi
    do_tag=true
    git tag -a "$DR_TAG"
fi

export DR_TAG
(cd go && ./build-rebar.sh)
(cd containers && ./rebuild-containers --all --tag "$DR_TAG" --update-git)
