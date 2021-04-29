use std::fs;
use std::path::Path;

fn with_insta_settings(path: &Path, f: impl FnOnce()) {
    let mut settings = insta::Settings::clone_current();
    settings.set_prepend_module_to_snapshot(false);
    settings.set_snapshot_path(path);
    settings.set_input_file(path);
    settings.bind(f)
}

pub fn run_test_folder<P: AsRef<Path>>(folder: P, test_fn: impl Fn(&Path)) {
    for entry in fs::read_dir(folder).expect("couldn't read directory") {
        let entry = entry.unwrap();
        let path = entry.path().canonicalize().unwrap();
        dbg!(entry.path().to_string_lossy());
        with_insta_settings(&path, || test_fn(&path))
    }
}
