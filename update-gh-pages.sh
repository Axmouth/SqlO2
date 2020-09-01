cd postgrustql_wasm_client
wasm-pack build
cd ../postgrustql-ng-repl
ng build --prod=true --base-href=https://axmouth.github.io/postgrustql/ --deployUrl=https://axmouth.github.io/postgrustql/
cd ..

mkdir -p docs
cp -r postgrustql-ng-repl/dist/postgrustql-ng-repl/* docs