
CARGO_PROFILE_RELEASE_DEBUG=true cargo flamegraph --bin==select
env PERF=/usr/lib/linux-tools-5.4.0-84/perf  flamegraph target/release/select