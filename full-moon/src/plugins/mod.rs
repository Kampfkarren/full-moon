mod default;
mod never;
mod plugin;

#[cfg(test)]
mod tests;

pub use default::DefaultPlugin;
pub use never::Never;
pub use plugin::*;
