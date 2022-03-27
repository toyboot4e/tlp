/*!
Vitual file system, input to the database
*/

use std::hash::BuildHasherDefault;

use indexmap::IndexSet;
use rustc_hash::FxHasher;

use camino::{Utf8Path, Utf8PathBuf};

/// Interned path, base input to the DB
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId {
    raw: u32,
}

/// Insert-only map of `Utf8PathBuf` and `FileId`
#[derive(Default)]
pub struct Vfs {
    map: IndexSet<Utf8PathBuf, BuildHasherDefault<FxHasher>>,
}

impl Vfs {
    /// Get the id corresponding to `path`.
    ///
    /// If `path` does not exists in `self`, returns [`None`].
    pub fn get(&self, path: &Utf8Path) -> Option<FileId> {
        self.map
            .get_index_of(path)
            .map(|i| FileId { raw: i as u32 })
    }

    pub fn intern(&mut self, path: Utf8PathBuf) -> FileId {
        let (raw, _added) = self.map.insert_full(path);
        assert!(raw < u32::MAX as usize);
        FileId { raw: raw as u32 }
    }

    pub fn lookup(&self, id: FileId) -> &Utf8Path {
        self.map.get_index(id.raw as usize).unwrap()
    }
}
