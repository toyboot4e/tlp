/*!
Locations
*/

use camino::Utf8PathBuf;

/// TODO: Intern file paths
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FileId {
    path: Utf8PathBuf,
}

impl FileId {
    pub fn new(path: Utf8PathBuf) -> Self {
        Self { path }
    }
}
