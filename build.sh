#!/bin/bash

docker build -f base.dockerfile -t base-haskell:lts-14.13 .

# docker-build-cacher needs some env vars to populate the cache
export APP_NAME="${APP_NAME:-loany}"
export GIT_BRANCH="${GIT_BRANCH:-master}"
export DOCKER_TAG="${DOCKER_TAG:-loany:latest}"
export DOCKERFILE=Dockerfile

docker-build-cacher build && docker-build-cacher cache
