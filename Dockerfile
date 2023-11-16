# stack.yamlとltsを揃えること
FROM haskell:9.4.7 as base
WORKDIR /usr/src/app

######################################################
FROM base as deps

COPY stack.yaml package.yaml stack.yaml.lock ./

RUN stack build --system-ghc --dependencies-only

######################################################
FROM base as builder

COPY --from=deps /root/.stack /root/.stack
COPY ./ ./

RUN stack build --system-ghc --copy-bins --local-bin-path ./

######################################################
FROM ubuntu:latest

RUN apt update -y && apt upgrade -y && apt install -y libpq-dev

COPY --from=builder /usr/src/app/haskell-exe .

ENTRYPOINT [ "./haskell-exe" ]
