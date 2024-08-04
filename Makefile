.PHONY: all fmt-check fmt clippy test
DEFAULT: check

fmt-check:
	cargo fmt --all -- --check

fmt:
	cargo fmt --all

clippy:
	cargo clippy --workspace -- -D warnings

test:
	cargo test
