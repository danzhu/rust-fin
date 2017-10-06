enum Token {
    Def,
    Lparen,
    Rparen,
    Int(i32),
    Id(String),
}

pub struct Lexer<'a, Reader: 'a + io::Read> {
    reader: &'a mut Reader,
}

impl<'a, Reader: io::Read> Lexer<'a, Reader> {
    pub fn new(reader: &mut Reader) -> Lexer<Reader> {
        Lexer {
            reader,
        }
    }
}

impl<'a, Reader: io::Read> Iterator<Token> for Lexer<'a, Reader> {
    fn next(&mut self) -> Option<Token> {

    }
}
