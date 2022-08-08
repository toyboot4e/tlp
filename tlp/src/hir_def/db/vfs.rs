//! Vitual file system, input to the database

use std::hash::BuildHasherDefault;

use indexmap::IndexSet;
use rustc_hash::FxHasher;

use camino::{Utf8Path, Utf8PathBuf};

/// Interned path, base input to the DB
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VfsFileId {
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
    pub fn get(&self, path: &Utf8Path) -> Option<VfsFileId> {
        self.map
            .get_index_of(path)
            .map(|i| VfsFileId { raw: i as u32 })
    }

    pub fn intern(&mut self, path: Utf8PathBuf) -> VfsFileId {
        let (raw, _added) = self.map.insert_full(path);
        assert!(raw < u32::MAX as usize);
        VfsFileId { raw: raw as u32 }
    }

    pub fn lookup(&self, id: VfsFileId) -> &Utf8Path {
        self.map.get_index(id.raw as usize).unwrap()
    }
}
