FROM fpco/stack-build:lts-14.13

RUN apt-get update && apt-get install -y libpcre3-dev libpq-dev
RUN stack setup --resolver=lts-14.11
COPY ./stack.yaml ./package.yaml ./
RUN stack build --only-dependencies
