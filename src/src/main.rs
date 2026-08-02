use std::collections::HashMap;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_if_and_for_scripts() {
        let src = r#"
            let host = "scanme.nmap.org"
            if (1 < 2) {
                scan(host, "22")
            }
            for (port = 1; port <= 2; port = port + 1) {
                print(port)
            }
        "#;

        let mut env = HashMap::new();
        let result = run_script(src, &mut env);
        assert!(result.is_ok());
    }

    #[test]
    fn parses_port_spec() {
        let spec = "21,22,80-82";
        let ports = parse_port_spec(spec);
        assert_eq!(ports, vec![21, 22, 80, 81, 82]);
    }
}

fn main() {
    println!("ozo network scanner");
}

fn run_script(_src: &str, _env: &mut HashMap<String, String>) -> Result<(), String> {
    Err("not implemented".to_string())
}

fn parse_port_spec(_spec: &str) -> Vec<u16> {
    vec![]
}
