FROM haskell:slim

WORKDIR /app

RUN apt-get update && apt-get install -y git curl && rm -rf /var/lib/apt/lists/*

COPY stack.yaml .
COPY package.yaml .
COPY Main.hs .

RUN stack setup --install-ghc
RUN stack build --install-ghc

EXPOSE 8080
CMD ["stack", "run"]
