pub fn html_escape(s: &str) -> String {
    let mut output = String::with_capacity(s.len() * 2);

    for c in s.chars() {
        match c {
            '"' => output.push_str("&quot;"),
            '&' => output.push_str("&amp;"),
            '/' => output.push_str("&#x2F;"),
            '<' => output.push_str("&lt;"),
            '>' => output.push_str("&gt;"),
            '\'' => output.push_str("&#x27;"),
            _ => output.push(c),
        }
    }

    output
}
