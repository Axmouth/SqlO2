cargo install trunk --locked
rustup target add wasm32-unknown-unknown
cd wasm-repl
trunk build --release
cd ..

mkdir -p docs
cp -r wasm-repl/dist/* docs