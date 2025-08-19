FROM rust:slim-trixie AS builder
WORKDIR /usr/src/app
COPY . .
RUN cargo build --release

FROM debian:trixie-slim
WORKDIR /
COPY --from=builder /usr/src/app/target/release/ezcord ./
CMD ["./ezcord", "/bot.toml"]