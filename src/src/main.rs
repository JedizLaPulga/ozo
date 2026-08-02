use std::collections::HashMap;
use std::env;
use std::fmt;
use std::io::{self, BufRead, Read, Write};
use std::net::{TcpStream, ToSocketAddrs};
use std::process::Command;
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

type RuntimeEnv = HashMap<String, Value>;

#[derive(Clone, Debug, PartialEq)]
enum Value {
    Number(i64),
    String(String),
    Bool(bool),
    Null,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug)]
enum Expr {
    Number(i64),
    String(String),
    Variable(String),
    Bool(bool),
    BinaryOp { op: String, left: Box<Expr>, right: Box<Expr> },
    Call { name: String, args: Vec<Expr> },
}

#[derive(Debug)]
enum Statement {
    Let { name: String, value: Expr },
    Assign { name: String, value: Expr },
    If {
        condition: Expr,
        then_branch: Vec<Statement>,
        else_branch: Vec<Statement>,
    },
    For {
        name: String,
        start: Expr,
        end: Expr,
        step: Expr,
        body: Vec<Statement>,
    },
    ExprStmt(Expr),
}

#[derive(Debug, Clone)]
struct ScanResult {
    port: u16,
    state: String,
    service: String,
    banner: String,
}

#[derive(Debug, Clone)]
enum Token {
    Let,
    If,
    Else,
    For,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,
    Newline,
    Assign,
    Plus,
    Minus,
    Star,
    Slash,
    Comma,
    EqEq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Identifier(String),
    Number(i64),
    String(String),
    Bool(bool),
    Eof,
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(input: &str) -> Self {
        Self {
            tokens: tokenize(input),
            pos: 0,
        }
    }

    fn parse(&mut self) -> Result<Vec<Statement>, String> {
        let mut statements = Vec::new();
        loop {
            self.skip_separators();
            if self.peek().is_none() || matches!(self.peek(), Some(Token::Eof)) {
                break;
            }
            if matches!(self.peek(), Some(Token::RBrace)) {
                break;
            }
            if let Some(stmt) = self.parse_statement()? {
                statements.push(stmt);
            }
        }
        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Option<Statement>, String> {
        match self.peek() {
            Some(Token::Let) => {
                self.advance();
                let name = self.expect_identifier()?;
                self.expect_assign()?;
                let value = self.parse_expression()?;
                self.skip_separators();
                Ok(Some(Statement::Let { name, value }))
            }
            Some(Token::If) => {
                self.advance();
                self.expect(&Token::LParen)?;
                let condition = self.parse_expression()?;
                self.expect(&Token::RParen)?;
                let then_branch = self.parse_block()?;
                let else_branch = if self.peek_is(&Token::Else) {
                    self.advance();
                    self.parse_block()?
                } else {
                    Vec::new()
                };
                Ok(Some(Statement::If {
                    condition,
                    then_branch,
                    else_branch,
                }))
            }
            Some(Token::For) => {
                self.advance();
                self.expect(&Token::LParen)?;
                let name = self.expect_identifier()?;
                self.expect_assign()?;
                let start = self.parse_expression()?;
                self.expect(&Token::Semicolon)?;
                let end = self.parse_expression()?;
                self.expect(&Token::Semicolon)?;
                let step = self.parse_assignment_expression(&name)?;
                self.expect(&Token::RParen)?;
                let body = self.parse_block()?;
                Ok(Some(Statement::For {
                    name,
                    start,
                    end,
                    step,
                    body,
                }))
            }
            Some(Token::RBrace) => Ok(None),
            Some(Token::Newline) | Some(Token::Semicolon) => {
                self.advance();
                Ok(None)
            }
            _ => {
                let expr = self.parse_expression()?;
                self.skip_separators();
                Ok(Some(Statement::ExprStmt(expr)))
            }
        }
    }

    fn parse_block(&mut self) -> Result<Vec<Statement>, String> {
        self.expect(&Token::LBrace)?;
        let mut statements = Vec::new();
        loop {
            self.skip_separators();
            if self.peek_is(&Token::RBrace) {
                self.advance();
                break;
            }
            if let Some(stmt) = self.parse_statement()? {
                statements.push(stmt);
            }
        }
        Ok(statements)
    }

    fn parse_expression(&mut self) -> Result<Expr, String> {
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_additive()?;
        loop {
            match self.peek() {
                Some(Token::Less) => {
                    self.advance();
                    let right = self.parse_additive()?;
                    expr = Expr::BinaryOp { op: "<".into(), left: Box::new(expr), right: Box::new(right) };
                }
                Some(Token::LessEq) => {
                    self.advance();
                    let right = self.parse_additive()?;
                    expr = Expr::BinaryOp { op: "<=".into(), left: Box::new(expr), right: Box::new(right) };
                }
                Some(Token::Greater) => {
                    self.advance();
                    let right = self.parse_additive()?;
                    expr = Expr::BinaryOp { op: ">".into(), left: Box::new(expr), right: Box::new(right) };
                }
                Some(Token::GreaterEq) => {
                    self.advance();
                    let right = self.parse_additive()?;
                    expr = Expr::BinaryOp { op: ">=".into(), left: Box::new(expr), right: Box::new(right) };
                }
                Some(Token::EqEq) => {
                    self.advance();
                    let right = self.parse_additive()?;
                    expr = Expr::BinaryOp { op: "==".into(), left: Box::new(expr), right: Box::new(right) };
                }
                Some(Token::NotEq) => {
                    self.advance();
                    let right = self.parse_additive()?;
                    expr = Expr::BinaryOp { op: "!=".into(), left: Box::new(expr), right: Box::new(right) };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_additive(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_multiplicative()?;
        loop {
            match self.peek() {
                Some(Token::Plus) => {
                    self.advance();
                    let right = self.parse_multiplicative()?;
                    expr = Expr::BinaryOp { op: "+".into(), left: Box::new(expr), right: Box::new(right) };
                }
                Some(Token::Minus) => {
                    self.advance();
                    let right = self.parse_multiplicative()?;
                    expr = Expr::BinaryOp { op: "-".into(), left: Box::new(expr), right: Box::new(right) };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_multiplicative(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_primary()?;
        loop {
            match self.peek() {
                Some(Token::Star) => {
                    self.advance();
                    let right = self.parse_primary()?;
                    expr = Expr::BinaryOp { op: "*".into(), left: Box::new(expr), right: Box::new(right) };
                }
                Some(Token::Slash) => {
                    self.advance();
                    let right = self.parse_primary()?;
                    expr = Expr::BinaryOp { op: "/".into(), left: Box::new(expr), right: Box::new(right) };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        match self.peek() {
            Some(Token::Number(n)) => {
                let number = *n;
                self.advance();
                Ok(Expr::Number(number))
            }
            Some(Token::String(s)) => {
                let text = s.clone();
                self.advance();
                Ok(Expr::String(text))
            }
            Some(Token::Bool(b)) => {
                let value = *b;
                self.advance();
                Ok(Expr::Bool(value))
            }
            Some(Token::Identifier(name)) => {
                let name = name.clone();
                self.advance();
                if self.peek_is(&Token::LParen) {
                    self.advance();
                    let mut args = Vec::new();
                    if !self.peek_is(&Token::RParen) {
                        loop {
                            args.push(self.parse_expression()?);
                            if self.peek_is(&Token::Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect(&Token::RParen)?;
                    Ok(Expr::Call { name, args })
                } else {
                    Ok(Expr::Variable(name))
                }
            }
            Some(Token::LParen) => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(&Token::RParen)?;
                Ok(expr)
            }
            _ => Err(format!("unexpected token while parsing expression: {:?}", self.peek())),
        }
    }

    fn parse_assignment_expression(&mut self, name: &str) -> Result<Expr, String> {
        self.expect_assign()?;
        let value = self.parse_expression()?;
        Ok(Expr::Call {
            name: "assign".to_string(),
            args: vec![Expr::String(name.to_string()), value],
        })
    }

    fn expect_assign(&mut self) -> Result<(), String> {
        if !self.peek_is(&Token::Assign) {
            return Err("expected '='".into());
        }
        self.advance();
        Ok(())
    }

    fn expect(&mut self, expected: &Token) -> Result<(), String> {
        if self.peek_is(expected) {
            self.advance();
            Ok(())
        } else {
            Err(format!("expected {:?}", expected))
        }
    }

    fn expect_identifier(&mut self) -> Result<String, String> {
        match self.peek() {
            Some(Token::Identifier(name)) => {
                let name = name.clone();
                self.advance();
                Ok(name)
            }
            _ => Err("expected identifier".into()),
        }
    }

    fn peek_is(&self, token: &Token) -> bool {
        matches!(self.peek(), Some(t) if std::mem::discriminant(t) == std::mem::discriminant(token))
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }

    fn skip_separators(&mut self) {
        while matches!(self.peek(), Some(Token::Newline) | Some(Token::Semicolon)) {
            self.advance();
        }
    }
}

fn tokenize(input: &str) -> Vec<Token> {
    let chars: Vec<char> = input.chars().collect();
    let mut tokens = Vec::new();
    let mut idx = 0usize;

    while idx < chars.len() {
        let ch = chars[idx];
        if ch.is_whitespace() {
            if ch == '\n' {
                tokens.push(Token::Newline);
            }
            idx += 1;
            continue;
        }
        if ch == '/' && idx + 1 < chars.len() && chars[idx + 1] == '/' {
            while idx < chars.len() && chars[idx] != '\n' {
                idx += 1;
            }
            continue;
        }
        match ch {
            ';' => tokens.push(Token::Semicolon),
            '(' => tokens.push(Token::LParen),
            ')' => tokens.push(Token::RParen),
            '{' => tokens.push(Token::LBrace),
            '}' => tokens.push(Token::RBrace),
            ',' => tokens.push(Token::Comma),
            '+' => tokens.push(Token::Plus),
            '-' => tokens.push(Token::Minus),
            '*' => tokens.push(Token::Star),
            '/' => tokens.push(Token::Slash),
            '=' => {
                if idx + 1 < chars.len() && chars[idx + 1] == '=' {
                    tokens.push(Token::EqEq);
                    idx += 2;
                    continue;
                }
                tokens.push(Token::Assign);
            }
            '!' => {
                if idx + 1 < chars.len() && chars[idx + 1] == '=' {
                    tokens.push(Token::NotEq);
                    idx += 2;
                    continue;
                }
                tokens.push(Token::Identifier("!".into()));
            }
            '<' => {
                if idx + 1 < chars.len() && chars[idx + 1] == '=' {
                    tokens.push(Token::LessEq);
                    idx += 2;
                    continue;
                }
                tokens.push(Token::Less);
            }
            '>' => {
                if idx + 1 < chars.len() && chars[idx + 1] == '=' {
                    tokens.push(Token::GreaterEq);
                    idx += 2;
                    continue;
                }
                tokens.push(Token::Greater);
            }
            '"' => {
                let mut buf = String::new();
                idx += 1;
                while idx < chars.len() && chars[idx] != '"' {
                    buf.push(chars[idx]);
                    idx += 1;
                }
                idx += 1;
                tokens.push(Token::String(buf));
            }
            _ if ch.is_ascii_digit() => {
                let mut value = String::new();
                while idx < chars.len() && chars[idx].is_ascii_digit() {
                    value.push(chars[idx]);
                    idx += 1;
                }
                tokens.push(Token::Number(value.parse().unwrap()));
                continue;
            }
            _ if ch.is_ascii_alphabetic() || ch == '_' => {
                let mut ident = String::new();
                while idx < chars.len() && (chars[idx].is_ascii_alphanumeric() || chars[idx] == '_') {
                    ident.push(chars[idx]);
                    idx += 1;
                }
                let token = match ident.as_str() {
                    "let" => Token::Let,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "for" => Token::For,
                    "true" => Token::Bool(true),
                    "false" => Token::Bool(false),
                    _ => Token::Identifier(ident),
                };
                tokens.push(token);
                continue;
            }
            _ => {
                idx += 1;
            }
        }
        idx += 1;
    }
    tokens.push(Token::Eof);
    tokens
}

fn run_script(src: &str, env: &mut RuntimeEnv) -> Result<(), String> {
    let mut parser = Parser::new(src);
    let statements = parser.parse()?;
    exec_statements(&statements, env)
}

fn exec_statements(statements: &[Statement], env: &mut RuntimeEnv) -> Result<(), String> {
    for stmt in statements {
        exec_statement(stmt, env)?;
    }
    Ok(())
}

fn exec_statement(stmt: &Statement, env: &mut RuntimeEnv) -> Result<(), String> {
    match stmt {
        Statement::Let { name, value } => {
            let v = eval_expr(value, env)?;
            env.insert(name.clone(), v);
        }
        Statement::Assign { name, value } => {
            let v = eval_expr(value, env)?;
            env.insert(name.clone(), v);
        }
        Statement::If { condition, then_branch, else_branch } => {
            let cond = eval_expr(condition, env)?;
            if is_truthy(&cond) {
                exec_statements(then_branch, env)?;
            } else if !else_branch.is_empty() {
                exec_statements(else_branch, env)?;
            }
        }
        Statement::For { name, start, end, step, body } => {
            let start_value = eval_expr(start, env)?;
            let end_value = eval_expr(end, env)?;
            let mut current = start_value;
            env.insert(name.clone(), current.clone());
            while compare_values(&current, &end_value, "<=")? {
                exec_statements(body, env)?;
                let step_value = eval_expr(step, env)?;
                let next = apply_binary(&current, &step_value, "+")?;
                current = next;
                env.insert(name.clone(), current.clone());
            }
        }
        Statement::ExprStmt(expr) => {
            let _ = eval_expr(expr, env)?;
        }
    }
    Ok(())
}

fn eval_expr(expr: &Expr, env: &mut RuntimeEnv) -> Result<Value, String> {
    match expr {
        Expr::Number(n) => Ok(Value::Number(*n)),
        Expr::String(s) => Ok(Value::String(s.clone())),
        Expr::Bool(b) => Ok(Value::Bool(*b)),
        Expr::Variable(name) => env
            .get(name)
            .cloned()
            .ok_or_else(|| format!("undefined variable: {name}")),
        Expr::BinaryOp { op, left, right } => {
            let left_v = eval_expr(left, env)?;
            let right_v = eval_expr(right, env)?;
            apply_binary(&left_v, &right_v, op)
        }
        Expr::Call { name, args } => eval_call(name, args, env),
    }
}

fn eval_call(name: &str, args: &[Expr], env: &mut RuntimeEnv) -> Result<Value, String> {
    match name {
        "print" => {
            let value = eval_expr(&args[0], env)?;
            println!("{value}");
            Ok(Value::Null)
        }
        "scan" => {
            if args.len() < 2 {
                return Err("scan expects target and port spec".into());
            }
            let target = match eval_expr(&args[0], env)? {
                Value::String(s) => s,
                other => other.to_string(),
            };
            let ports = match eval_expr(&args[1], env)? {
                Value::String(s) => s,
                Value::Number(n) => n.to_string(),
                other => other.to_string(),
            };
            let results = scan_target(&target, &ports, 600, 8, true)?;
            print_scan_report(&results);
            Ok(Value::Null)
        }
        "ping" => {
            let target = match eval_expr(&args[0], env)? {
                Value::String(s) => s,
                other => other.to_string(),
            };
            Ok(Value::Bool(ping_host(&target)))
        }
        "assign" => {
            let name = match &args[0] {
                Expr::String(s) => s.clone(),
                Expr::Variable(name) => name.clone(),
                _ => return Err("assign expects identifier".into()),
            };
            let value = eval_expr(&args[1], env)?;
            env.insert(name.clone(), value.clone());
            Ok(value)
        }
        _ => Err(format!("unknown function: {name}")),
    }
}

fn apply_binary(left: &Value, right: &Value, op: &str) -> Result<Value, String> {
    match op {
        "+" => {
            let left_n = as_number(left)?;
            let right_n = as_number(right)?;
            Ok(Value::Number(left_n + right_n))
        }
        "-" => {
            let left_n = as_number(left)?;
            let right_n = as_number(right)?;
            Ok(Value::Number(left_n - right_n))
        }
        "*" => {
            let left_n = as_number(left)?;
            let right_n = as_number(right)?;
            Ok(Value::Number(left_n * right_n))
        }
        "/" => {
            let left_n = as_number(left)?;
            let right_n = as_number(right)?;
            if right_n == 0 {
                return Err("division by zero".into());
            }
            Ok(Value::Number(left_n / right_n))
        }
        "<" => Ok(Value::Bool(as_number(left)? < as_number(right)?)),
        "<=" => Ok(Value::Bool(as_number(left)? <= as_number(right)?)),
        ">" => Ok(Value::Bool(as_number(left)? > as_number(right)?)),
        ">=" => Ok(Value::Bool(as_number(left)? >= as_number(right)?)),
        "==" => Ok(Value::Bool(left == right)),
        "!=" => Ok(Value::Bool(left != right)),
        _ => Err(format!("unsupported operator: {op}")),
    }
}

fn as_number(value: &Value) -> Result<i64, String> {
    match value {
        Value::Number(n) => Ok(*n),
        Value::String(s) => s.parse::<i64>().map_err(|_| format!("cannot parse number from {s}")),
        _ => Err("number required".into()),
    }
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Bool(false) | Value::Number(0) => false,
        Value::String(s) if s.is_empty() => false,
        Value::Null => false,
        _ => true,
    }
}

fn compare_values(left: &Value, right: &Value, op: &str) -> Result<bool, String> {
    match op {
        "<" => Ok(as_number(left)? < as_number(right)?),
        "<=" => Ok(as_number(left)? <= as_number(right)?),
        ">" => Ok(as_number(left)? > as_number(right)?),
        ">=" => Ok(as_number(left)? >= as_number(right)?),
        _ => Ok(false),
    }
}

fn parse_port_spec(spec: &str) -> Vec<u16> {
    let mut ports = Vec::new();
    for part in spec.split(',').map(str::trim).filter(|s| !s.is_empty()) {
        if part.contains('-') {
            let mut range = part.split('-');
            let start = range.next().unwrap_or("0").parse::<u16>().unwrap_or(0);
            let end = range.next().unwrap_or("0").parse::<u16>().unwrap_or(0);
            for port in start..=end {
                ports.push(port);
            }
        } else if let Ok(port) = part.parse::<u16>() {
            ports.push(port);
        }
    }
    ports
}

fn scan_target(target: &str, spec: &str, timeout_ms: u64, _concurrency: usize, banner: bool) -> Result<Vec<ScanResult>, String> {
    let ports = parse_port_spec(spec);
    if ports.is_empty() {
        return Err("no ports to scan".into());
    }
    let timeout = Duration::from_millis(timeout_ms);
    let (tx, rx) = mpsc::channel();
    let mut handles = Vec::new();
    let target = target.to_string();
    for port in ports.iter().copied() {
        let tx = tx.clone();
        let target = target.clone();
        let timeout = timeout;
        let banner = banner;
        handles.push(thread::spawn(move || {
            let result = scan_port(&target, port, timeout, banner);
            let _ = tx.send(result);
        }));
    }
    drop(tx);
    let mut results = Vec::new();
    for _ in 0..ports.len() {
        if let Ok(result) = rx.recv() {
            results.push(result);
        }
    }
    for handle in handles {
        let _ = handle.join();
    }
    results.sort_by_key(|r| r.port);
    Ok(results)
}

fn scan_port(target: &str, port: u16, timeout: Duration, banner: bool) -> ScanResult {
    let address = if target.contains(':') {
        target.to_string()
    } else {
        format!("{target}:{port}")
    };
    if let Ok(addrs) = address.to_socket_addrs() {
        for addr in addrs {
            if let Ok(mut stream) = TcpStream::connect_timeout(&addr, timeout) {
                let _ = stream.set_read_timeout(Some(timeout));
                let banner_text = if banner {
                    let mut buf = [0u8; 128];
                    let _ = stream.read(&mut buf).unwrap_or(0);
                    String::from_utf8_lossy(&buf).trim().to_string()
                } else {
                    String::new()
                };
                let service = identify_service(port);
                return ScanResult {
                    port,
                    state: "open".into(),
                    service,
                    banner: banner_text,
                };
            }
        }
    }
    ScanResult {
        port,
        state: "closed".into(),
        service: identify_service(port),
        banner: String::new(),
    }
}

fn identify_service(port: u16) -> String {
    match port {
        21 => "ftp".into(),
        22 => "ssh".into(),
        23 => "telnet".into(),
        25 => "smtp".into(),
        53 => "dns".into(),
        80 => "http".into(),
        110 => "pop3".into(),
        143 => "imap".into(),
        443 => "https".into(),
        3306 => "mysql".into(),
        5432 => "postgres".into(),
        6379 => "redis".into(),
        8080 => "http-alt".into(),
        27017 => "mongodb".into(),
        _ => "unknown".into(),
    }
}

fn print_scan_report(results: &[ScanResult]) {
    println!("Scan report");
    println!("----------");
    for result in results {
        let banner = if result.banner.is_empty() {
            String::new()
        } else {
            format!(" | {}", result.banner)
        };
        println!("[{}] {} /tcp {}{}", result.state, result.port, result.service, banner);
    }
}

fn ping_host(target: &str) -> bool {
    #[cfg(target_family = "windows")]
    {
        Command::new("ping")
            .args(["-n", "1", target])
            .output()
            .map(|out| out.status.success())
            .unwrap_or(false)
    }
    #[cfg(not(target_family = "windows"))]
    {
        Command::new("ping")
            .args(["-c", "1", target])
            .output()
            .map(|out| out.status.success())
            .unwrap_or(false)
    }
}

fn print_help() {
    println!("ozo network scanner");
    println!("Usage:");
    println!("  ozo scan <target> <ports> [--timeout ms] [--banner] [--ping]");
    println!("  ozo script <file>");
    println!("  ozo repl");
    println!("  ozo help");
    println!();
    println!("Mini language:");
    println!("  let host = \"scanme.nmap.org\"");
    println!("  if (1 < 2) {{ scan(host, \"22\") }}");
    println!("  for (port = 1; port <= 3; port = port + 1) {{ print(port) }}");
}

fn dispatch_cli(args: &[String]) -> Result<(), String> {
    match args.first().map(String::as_str) {
        Some("scan") => {
            let target = args.get(1).cloned().ok_or("missing target")?;
            let ports = args.get(2).cloned().unwrap_or_else(|| "22,80,443".to_string());
            let mut timeout_ms = 600u64;
            let mut banner = false;
            let mut ping = false;
            let mut idx = 3;
            while idx < args.len() {
                match args[idx].as_str() {
                    "--timeout" => timeout_ms = args.get(idx + 1).ok_or("missing timeout value")?.parse().unwrap_or(600),
                    "--banner" => banner = true,
                    "--ping" => ping = true,
                    _ => {}
                }
                idx += 2;
            }
            if ping {
                println!("ping status: {}", ping_host(&target));
            }
            let results = scan_target(&target, &ports, timeout_ms, 8, banner)?;
            print_scan_report(&results);
            Ok(())
        }
        Some("script") => {
            let path = args.get(1).ok_or("missing script path")?;
            let src = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
            let mut env = RuntimeEnv::new();
            run_script(&src, &mut env)
        }
        Some("repl") | None => run_repl(),
        Some("help") => {
            print_help();
            Ok(())
        }
        _ => Err("unknown command".into()),
    }
}

fn run_repl() -> Result<(), String> {
    let mut env = RuntimeEnv::new();
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();
    println!("ozo shell. type help or exit");
    loop {
        print!("ozo> ");
        io::stdout().flush().map_err(|e| e.to_string())?;
        let Some(line) = lines.next() else { break; };
        let line = line.map_err(|e| e.to_string())?;
        if line.trim().is_empty() {
            continue;
        }
        if line.trim().eq_ignore_ascii_case("exit") {
            break;
        }
        if line.trim().eq_ignore_ascii_case("help") {
            print_help();
            continue;
        }
        if let Err(err) = run_script(&line, &mut env) {
            println!("error: {err}");
        }
    }
    Ok(())
}

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    if let Err(err) = dispatch_cli(&args) {
        eprintln!("error: {err}");
        std::process::exit(1);
    }
}

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

        let mut env = RuntimeEnv::new();
        let result = run_script(src, &mut env);
        if let Err(err) = &result {
            println!("script error: {err}");
        }
        assert!(result.is_ok());
    }

    #[test]
    fn parses_port_spec() {
        let spec = "21,22,80-82";
        let ports = parse_port_spec(spec);
        assert_eq!(ports, vec![21, 22, 80, 81, 82]);
    }
}
