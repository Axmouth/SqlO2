cargo install trunk --locked
rustup target add wasm32-unknown-unknown
cd wasm-repl
trunk build --release --public-url SqlO2/
cd ..

mkdir -p docs
cp -r wasm-repl/dist/* docs