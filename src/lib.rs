//! Little snippets of code that i find myself needing often, so i compiled them into a crate.
//!
//! Main features are the `join` macro and the `FancyText` / `colors` system for text formatting. 

use std::{fs, io};
use std::fs::File;
use std::io::Write;
use std::ops::{Deref, DerefMut};
use crate::colors::*;
use crate::Magnet::*;

/// Joins an array of elements
///
/// Uses a specified separator, or `""` if none
/// # Examples
/// ```
/// let age = 18;
/// println!("{}", remtools::join!["Hello, I am ", &*age.to_string(), " years old"]);
/// //prints "Hello, I am 18 years old
///
///
/// struct Foo {
///     bar: i32,
///     baz: &'static str,
/// }
///
/// let foo = Foo {
///     bar: 19,
///     baz: "mwrp"
/// };
/// // optional seperator can be defined join![seperator: ...]
/// let path = remtools::join!["/": "./src", &*foo.bar.to_string(), foo.baz, "temp.txt"];
/// // path results in "./src/19/mwrp/temp.txt"
/// ```
#[macro_export]
macro_rules! join {
    ($s:literal: $( $x:expr ),*) => {
            [$($x,)*].join($s)
    };
    ( $( $y:expr ),* ) => {
            [$($y,)*].join("")
    };
}

/// ?: operator in languages like Java
///
/// *Planning to rework this in the future so it actually uses ?: rather than 3 inputs*
/// # Examples
/// ```
/// use remtools::qc;
/// fn min(a: i32, b: i32) -> i32 {
///     qc!(a < b, a, b)
/// }
/// ```
#[macro_export]
macro_rules! qc {
    ($s:expr, $t:expr, $f:expr) => {
        if $s {$t} else {$f}
    };
}

// todo
// chain operators:
// "text".background(str::RED).foreground(str::AQUA).end();
// "text".prepend(color_codes::background(str::WHITE)).end();
// FancyText : trait for allowing functions on str + string
// color_codes : mod for getting data for control codes 
// foreground, background, foreground_rgb, background_rgb, modifier, italic, bold, underline, 
// 

/// Allows String and &str to have formatting applied
///
/// Uses control characters (`\x1b[m`). Any formatting calls on an `&str` 
/// will call `.to_string()`. Also see [colors]. Use `.end()` to prevent spillage.
/// # Examples
/// ```
/// use remtools::{FancyText, join};
/// use remtools::colors::*;
/// println!("{} {}", "Error: ".foreground(RED).modifier(UNDERLINE).end(), "No bitches found!".foreground(ORN).modifier(ITALIC).end());
///
/// let format = join![BOLD, &*PNK.to_string(), UNDERLINE];
/// let use1 = "first format".prepend(format.clone());
/// let use2 = "second format".prepend(format.clone()).background(GRY);
/// ```
pub trait FancyText {
    fn prepend(self, data: String) -> String;
    fn append(self, data: String) -> String;
    /// Appends the reset control code (`\x1b[m`) to prevent spillage
    fn end(self) -> String;
    fn foreground(self, color: u8) -> String;
    fn background(self, color: u8) -> String;
    fn modifier(self, data: &str) -> String;
}

impl FancyText for String {
    fn prepend(self, data: String) -> String {
        join![data, self]
    }

    fn append(self, data: String) -> String {
        join![self, data]
    }

    fn end(self) -> String {
        join![&*self, colors::END]
    }

    fn foreground(self, color: u8) -> String {
        self.prepend(foreground(color))
    }

    fn background(self, color: u8) -> String {
        self.prepend(background(color))
    }

    fn modifier(self, data: &str) -> String {
        self.prepend(data.to_string())
    }
}

impl FancyText for &str {
    fn prepend(self, data: String) -> String {
        join![data, self.to_string()]
    }

    fn append(self, data: String) -> String {
        join![self.to_string(), data]
    }

    fn end(self) -> String {
        join![self, colors::END]
    }

    fn foreground(self, color: u8) -> String {
        self.to_string().prepend(foreground(color))
    }

    fn background(self, color: u8) -> String {
        self.to_string().prepend(background(color))
    }

    fn modifier(self, data: &str) -> String {
        self.to_string().prepend(data.to_string())
    }
}

/// Color codes and formatting chars. Used in conjunction with the FancyText trait
///
/// # Examples
/// ```
/// use remtools::colors::*;
/// use remtools::FancyText;
/// // format can be directly injected into strings
/// println!("{}{}this text will be pink and italic{}", foreground(PNK), ITALIC, END);
/// // the color functions can be used with FancyText functions
/// println!("{}", "im gay so i need more than 8 colors!".prepend(foreground_rgb(250, 50, 200)).end());
/// println!("{}", "my console has really obscure feature support".prepend(custom("\x1b[8m")).end());
/// ```

pub mod colors {
    pub const GRY: u8 = 0;
    pub const RED: u8 = 1;
    pub const GRN: u8 = 2;
    pub const ORN: u8 = 3;
    pub const BLU: u8 = 4;
    pub const PNK: u8 = 5;
    pub const AQU: u8 = 6;
    pub const WHT: u8 = 7;

    pub static CONTROL_STR: &str = "\x1b[";
    pub static END: &str = "\x1b[m";

    pub static BOLD: &str = "\x1b[1m";
    pub static FAINT: &str = "\x1b[2m";
    pub static ITALIC: &str = "\x1b[3m";
    pub static UNDERLINE: &str = "\x1b[4m";
    pub static FRAME: &str = "\x1b[51m";
    pub static ENCIRCLE: &str = "\x1b[52m";
    pub static OVERLINE: &str = "\x1b[53m";

    pub fn custom(data: &str) -> String {
        join!("\x1b[", data, "m")
    }

    pub fn foreground(color: u8) -> String {
        join!(CONTROL_STR, &(30 + color).to_string(), "m")
    }

    pub fn background(color: u8) -> String {
        join!(CONTROL_STR, &(40 + color).to_string(), "m")
    }

    pub fn foreground_rgb(r: u8, g: u8, b: u8) -> String {
        join!(CONTROL_STR, "38;2;", &r.to_string(), ";", &g.to_string(), ";", &b.to_string(), "m")
    }

    pub fn background_rgb(r: u8, g: u8, b: u8) -> String {
        join!(CONTROL_STR, "48;2;", &r.to_string(), ";", &g.to_string(), ";", &b.to_string(), "m")
    }

    pub fn dark_foreground(color: u8) -> String {
        join!(CONTROL_STR, &(90 + color).to_string(), "m")
    }

    pub fn dark_background(color: u8) -> String {
        join!(CONTROL_STR, &(100 + color).to_string(), "m")
    }

    /// converts hsv to rgb, badly
    #[deprecated]
    pub fn bad_hsv_to_rgb(h: u8, s: u8, v: u8) -> (u8, u8, u8) {
        let s = s as f32 / 255.0;
        let v = v as f32 / 255.0;
        let c = v * s;
        let x = c * (1.0 - f32::abs((h as f32 / 60f32) % 2f32 - 1.0));
        let m = v - c;
        let rgb = match h {
            0..=42 => (c, x, 0.0),
            0..=84 => (x, c, 0.0),
            0..=127 => (0.0, c, x),
            0..=170 => (0.0, x, c),
            0..=212 => (x, 0.0, c),
            0..=255 => (c, 0.0, x)
        };
        (((rgb.0 + m) * 255.0) as u8, ((rgb.1 + m) * 255.0) as u8, ((rgb.2 + m) * 255.0) as u8)
    }
}


/// Quick file writing
/// # Examples
/// ```
/// let mut file = remtools::MFile::new("./output/string.txt".to_string());
/// file.save("UwU OwO :3".to_string()).unwrap();
///
/// let mut file2 = remtools::MFile::new("./output/utf-8.txt".to_string());
/// file2.save_bytes(&[0x55, 0x77, 0x55, 0x7e]).unwrap();
/// ```
#[derive(Debug)]
pub struct MFile {
    pub path: String,
    pub file: File,
}

impl MFile {
    /// Creates an MFile located at `path`. Will auto generate containing folders. 
    /// Will panic if the create function fails. 
    pub fn new(path: String) -> MFile {
        Self::make_folder(&*path.rsplit_once("/").unwrap_or(("", "")).0);
        MFile {
            file: File::create(&*path).expect(&*join!["Could not make '\x1b[93m", &*path, "\x1b[m'"]),
            path,
        }
    }

    /// Creates an MFile located at `path`. Will auto generate containing folders. 
    /// Will return an error if the create function fails.
    pub fn attempt_new(path: String) -> io::Result<MFile> {
        Self::make_folder(&*path.rsplit_once("/").unwrap_or(("", "")).0);
        Ok(MFile {
            file: File::create(&*path)?,
            path,
        })
    }

    /// Consumes the MFile object and writes `T` to the file.
    pub fn save<T: ToString>(self, write: T) -> io::Result<()> {
        self.internal_save(write.to_string().as_bytes())
    }

    /// Consumes the MFile object and writes to the file.
    pub fn save_bytes(self, write: &[u8]) -> io::Result<()> {
        self.internal_save(write)
    }

    fn internal_save(mut self, bytes: &[u8]) -> io::Result<()> {
        self.file.write_all(bytes)
    }

    /// Creates all the containing folders for a file.
    pub fn make_folder(path: &str) {
        fs::create_dir_all(path).unwrap_or_else(|e| {
            panic!("Could not generate '{path}' folder: {e}");
        });
    }
}

/// Separate a field from the parent object
/// # Examples
/// ```
/// use remtools::*;
/// struct Foo {
///     bar: i32,
///     baz: Magnet<String>,
/// }
/// fn main() {
///     let mut foo = Foo {
///         bar: 0,
///         baz: Magnet::new(Some("mwrp".to_string())),
///     };
///     let nya = foo.baz.unattach();
///     let new_baz = nya.foreground(colors::RED).end();
///     foo.baz.attach(new_baz);
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Magnet<T> {
    Attached(T),
    Unattached,
}

impl<T> Magnet<T> {
    /// Creates a Magnet from an Option
    pub fn new(value: Option<T>) -> Magnet<T> {
        match value {
            Some(v) => Attached(v),
            None => Unattached,
        }
    }

    /// Gets the internal `T` value as an `Option<&mut T>`.
    pub fn value(&mut self) -> Option<&mut T> {
        match self {
            Attached(v) => Some(v),
            Unattached => None,
        }
    }

    /// Unattaches the Magnet and returns the internal data. Panics if the magnet is already unattached.
    pub fn unattach(&mut self) -> T {
        match std::mem::replace(self, Unattached) {
            Attached(t) => t,
            Unattached => panic!("Attempted to unattach an unattached magnet"),
        }
    }

    /// Unattaches the Magnet and returns the internal data, or an Err(String) is the magnet is already unattached.
    pub fn attempt_unattach(&mut self) -> Result<T, String> {
        match std::mem::replace(self, Unattached) {
            Attached(t) => Ok(t),
            Unattached => Err("Attempted to unattach an unattached magnet".into()),
        }
    }

    /// Attaches the Magnet with the provided data
    pub fn attach(&mut self, value: T) {
        *self = Attached(value);
    }

    /// Is the Magnet attached?
    pub fn is_attached(&self) -> bool {
        match *self {
            Attached(_) => true,
            Unattached => false,
        }
    }

    /// Moves the data into a new Magnet, leaving an unattached magnet in its place. 
    /// Returns unattached if the current Magnet is unattached.
    pub fn pull_data(&mut self) -> Self {
        match *self {
            Attached(_) => Attached(self.unattach()),
            Unattached => Unattached,
        }
    }
}

impl<T> Deref for Magnet<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        match *self {
            Attached(ref x) => x,
            Unattached => panic!("Attempted to deref an unattached magnet"),
        }
    }
}

impl<T> DerefMut for Magnet<T> {
    fn deref_mut(&mut self) -> &mut T {
        match *self {
            Attached(ref mut x) => x,
            Unattached => panic!("Attempted to deref an unattached magnet"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn pretty_print() {
        let age = 18;
        println!("{}", join![" | ": "Hello, I am", &*age.to_string(), "years old"]);
        println!("{}catgirl{}: omg {} {}!!!", UNDERLINE, END, "haiiii".to_string().foreground(RED).modifier(ITALIC).end(), ":3".to_string().foreground(PNK).prepend(background_rgb(0, 20, 200)).end());
        println!("{}", String::from_utf8_lossy(&[0x55, 0x77, 0x55, 0x7e]));
        println!("{}{}this text will be pink and italic{}", foreground(PNK), ITALIC, END);
        println!("{}im gay so i need more than 8 colors!{}", foreground_rgb(250, 50, 200), END);
        let mut text = "################################################################################################################################".to_string();
        for i in (1..=text.len()).rev() {
            let color = bad_hsv_to_rgb((i * 4) as u8, 255, 255);
            text.insert_str(i - 1, &*foreground_rgb(color.0, color.1, color.2));
        }
        println!("{}{}", text, END);
    }
}