FROM rustlang/rust:nightly-slim

RUN apt-get update && apt-get install -y \
    cmake \
    g++ \
    libclang-dev \
    python3 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY . .

RUN cargo test
