mod lib;

#[tokio::main]
async fn main() {
    lib::repl::run_repl();
}
