use super::parse::allocator::AllocError;
use super::parse::SourceASTParseError;

enum SourceASTError {
    ParseError(SourceASTParseError),
    AllocError(AllocError),
}

impl From<AllocError> for SourceASTError {
    fn from(error: AllocError) -> Self {
        SourceASTError::AllocError(error)
    }
}

impl From<SourceASTParseError> for SourceASTError {
    fn from(error: SourceASTParseError) -> Self {
        SourceASTError::ParseError(error)
    }
}
