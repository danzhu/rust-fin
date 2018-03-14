use std::io;

use common::*;
use ctx::*;

pub struct ErrorBase<Kind> {
    pub kind: Kind,
    pub span: Span,
}

impl<Kind> Print for ErrorBase<Kind>
where
    Kind: Print,
{
    fn print<Out>(&self, f: &mut Out, ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        let start = self.span.start;
        write!(f, "{}: error: ", start.format(ctx))?;
        self.kind.print(f, ctx)?;
        writeln!(f)?;

        let line = &ctx.sources[start.file].lines[start.line];
        writeln!(f, "{}", line)?;

        let end = self.span.end;
        if start.file == end.file && start.line == end.line {
            for _ in 0..start.column {
                write!(f, " ")?;
            }

            let width = end.column - start.column;
            for _ in 0..width {
                write!(f, "^")?;
            }

            writeln!(f)?;
        }
        Ok(())
    }
}

pub trait Print {
    fn print<Out>(&self, f: &mut Out, ctx: &Context) -> io::Result<()>
    where
        Out: io::Write;
}
