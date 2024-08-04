.PHONY: all fmt fmt-check clippy test

default: all
all: fmt-check clippy test

fmt-check:
	cargo fmt --all -- --check

fmt:
	cargo fmt --all

clippy:
	cargo clippy --workspace -- -D warnings

test:
	cargo test
