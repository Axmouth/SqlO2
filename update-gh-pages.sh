cd postgrustql_wasm_client
wasm-pack build
cd ../postgrustql-ng-repl
ng build --prod=true --base-href=/postgrustql/ --deployUrl=/postgrustql/
cd ..

cp -r postgrustql-ng-repl/dist/postgrustql-ng-repl docs