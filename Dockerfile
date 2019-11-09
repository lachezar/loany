FROM gcr.io/atlantica-social-da271/base-haskell:lts-14.13 AS builder

# Copy all local files and build the app
COPY . .

RUN stack install

FROM debian:stretch-slim AS distro
ENV LC_ALL=C.UTF-8

RUN apt-get update && apt-get install -y libpcre3-dev

# Copy the executable from the builder stage
COPY --from=builder /root/.local/bin/loany /bin/
CMD ["/bin/loany", "-"]