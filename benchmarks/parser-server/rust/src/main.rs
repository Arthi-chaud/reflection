use std::env;
use std::fs::File;
use std::io::Read;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

use rouille::Response;
use rouille::router;
use scanf::sscanf;

#[derive(serde::Deserialize, Debug)]
struct DTO {
    name: String,
    boolean: bool,
}

fn main() {
    let port = env::var("PORT").expect("Missing or empty 'PORT' env. variable");
    rouille::start_server(format!("0.0.0.0:{}", port), move |request| {
        router!(request,
            (GET) (/) => {
                let file = File::open("index.html").unwrap();
                Response::from_file("text/html",  file)
            },
            (POST) (/serde) => {
                let mut body = request.data().expect("Expected a body");
                let mut body_str = String::default();
                body.read_to_string(&mut body_str).unwrap();
                let start = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
                let dto: DTO = serde_json::from_str(&body_str).unwrap();
                let end = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
                let duration_s: f64 = (end - start).as_secs_f64();
                Response::text(format!("{{ \"computationTime\": {} }}", duration_s))
            },
            (POST) (/reflection) => {
                let mut body = request.data().expect("Expected a body");
                let mut body_str = String::default();
                body.read_to_string(&mut body_str).unwrap();
                let start = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
                let dto: DTO = parse_dto(&body_str);
                let end = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
                let duration_s: f64 = (end - start).as_secs_f64();
                Response::text(format!("{{ \"computationTime\": {} }}", duration_s))
            },

            _ => Response::empty_404()
        )
    });
}

fn parse_dto(s: &String) -> DTO {
    let mut n: String = String::new();
    let mut b: bool = false;
    sscanf!(s.as_str(), "{{\"name\":\"{n}\",\"boolean\":{b}}}").unwrap();
    DTO {
        name: n,
        boolean: b,
    }
}
