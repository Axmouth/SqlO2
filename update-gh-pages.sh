cargo install trunk --locked
cd wasm-repl
ls -l
trunk build --release
cd ..

mkdir -p docs
cp -r wasm-repl/dist* docs