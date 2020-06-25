use postgrustql;
use tokio;

#[tokio::main]
async fn main() {
    postgrustql::repl::run_repl();
}
