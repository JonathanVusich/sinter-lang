#[cfg(target_os = "windows")]
mod windows;

#[cfg(target_os = "macos")]
mod unix;

#[cfg(target_os = "windows")]
pub use self::windows::*;

#[cfg(target_os = "macos")]
pub use self::unix::*;
