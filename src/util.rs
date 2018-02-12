use std::iter::Peekable;

pub trait IterExt<T, E> {
    fn try_next(&mut self) -> Result<Option<T>, E>;
}

impl<T, E> IterExt<T, E> for Iterator<Item=Result<T, E>> {
    fn try_next(&mut self) -> Result<Option<T>, E> {
        match self.next() {
            Some(Err(err)) => Err(err),
            Some(Ok(item)) => Ok(Some(item)),
            None => Ok(None),
        }
    }
}

impl<T, E, Iter> IterExt<T, E> for Peekable<Iter>
    where Iter: Iterator<Item=Result<T, E>>
{
    fn try_next(&mut self) -> Result<Option<T>, E> {
        match self.next() {
            Some(Err(err)) => Err(err),
            Some(Ok(item)) => Ok(Some(item)),
            None => Ok(None),
        }
    }
}

pub trait PeekableExt<T, E> {
    fn try_peek(&mut self) -> Result<Option<&T>, E>;
}

impl<T, E, Iter> PeekableExt<T, E> for Peekable<Iter>
    where Iter: Iterator<Item=Result<T, E>>, E: Clone
{
    fn try_peek(&mut self) -> Result<Option<&T>, E> {
        match self.peek() {
            Some(&Err(ref err)) => Err(err.clone()),
            Some(&Ok(ref tok)) => Ok(Some(tok)),
            None => Ok(None),
        }
    }
}
