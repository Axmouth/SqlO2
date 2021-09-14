cd wasm_client
wasm-pack build
cd ../ng-repl
ng build --prod=true --base-href=https://axmouth.github.io/postgrustql/ --deployUrl=https://axmouth.github.io/postgrustql/
cd ..

mkdir -p docs
cp -r ng-repl/dist/postgrustql-ng-repl/* docs