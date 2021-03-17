use std::fs;
use std::path::Path;

pub fn run_test_folder<P: AsRef<Path>>(folder: P, test_fn: impl Fn(&Path)) {
    for entry in fs::read_dir(folder).expect("couldn't read directory") {
        let entry = entry.unwrap();
        let path = entry.path().canonicalize().unwrap();
        test_fn(&path);
    }
}
