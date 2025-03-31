use colored::*;

#[derive(Debug)]
pub enum ErrorType {
    Syntax,
    Runtime,
    Type,
}

/// 単一の位置を指すエラーを表示する
pub fn point_error(code: &str, line: usize, column: usize, message: &str, error_type: ErrorType) {
    // コードのエラー箇所を^で提示する
    let mut error_line = String::new();
    let mut error_pointer = String::new();
    let mut line_number = 1;
    let mut char_count = 0;
    for (i, c) in code.chars().enumerate() {
        if line_number == line && char_count == column {
            error_pointer.push('^');
        } else {
            error_pointer.push(' ');
        }
        error_line.push(c);
        char_count += 1;
        if c == '\n' {
            line_number += 1;
            char_count = 0;
        }
    }

    // エラーメッセージの表示
    eprintln!(
        "{}: {}",
        format!("error[{}]", error_type.to_string()).red().bold(),
        message
    );
    eprintln!("{}", error_line);
    eprintln!("{}", error_pointer.red());
}

/// 範囲を指すエラーを表示する
pub fn range_error(
    lines: &Vec<String>,
    start_line: usize,
    start_column: usize,
    end_line: usize,
    end_column: usize,
    message: &str,
    error_type: ErrorType,
) {
    // エラーメッセージのヘッダーを表示
    eprintln!(
        "{}: {}",
        format!("error[{}]", error_type.to_string()).red().bold(),
        message
    );

    // 行番号の最大桁数を計算
    let max_line_num_width = (start_line + lines.len()).to_string().len();

    // 範囲内の各行を処理
    for (i, line) in lines.iter().enumerate() {
        let current_line = start_line + i;
        if current_line < start_line || current_line > end_line {
            continue;
        }

        // 行番号を表示
        eprint!("{:>width$} | ", current_line, width = max_line_num_width);
        eprintln!("{}", line);

        // アンダーラインを表示
        eprint!("{:>width$} | ", "", width = max_line_num_width);

        // アンダーラインの生成
        let mut underline = String::new();
        for (col, _) in line.chars().enumerate() {
            if current_line == start_line && current_line == end_line {
                // 単一行の範囲
                if col >= start_column && col <= end_column {
                    underline.push('~');
                } else {
                    underline.push(' ');
                }
            } else if current_line == start_line {
                // 開始行
                if col >= start_column {
                    underline.push('~');
                } else {
                    underline.push(' ');
                }
            } else if current_line == end_line {
                // 終了行
                if col <= end_column {
                    underline.push('~');
                } else {
                    underline.push(' ');
                }
            } else {
                // 中間行
                underline.push('~');
            }
        }
        eprintln!("{}", underline.red());
    }
}

impl ToString for ErrorType {
    fn to_string(&self) -> String {
        match self {
            ErrorType::Syntax => "SyntaxError".to_string(),
            ErrorType::Runtime => "RuntimeError".to_string(),
            ErrorType::Type => "TypeError".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_point_error() {
        let code = ";";
        point_error(code, 1, 0, "Unexpected token", ErrorType::Syntax);
    }

    #[test]
    fn test_range_error() {
        let code = "let x = 1;\nlet y = 2;\nlet z = x + y;";
        let lines: Vec<String> = code.lines().map(|s| s.to_string()).collect();
        range_error(&lines, 1, 0, 2, 10, "Unexpected token", ErrorType::Syntax);
    }
}
