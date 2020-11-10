#[macro_export]
macro_rules! multiply {

    ($x:expr, $y:expr) => {
        x * y;
    }
}

#[macro_export]
macro_rules! divide {

    ($x:expr, $y:expr) => {
        x / y;
    }
}

#[macro_export]
macro_rules! add {

    ($x:expr, $y:expr) => {
        x + y;
    }
}

#[macro_export]
macro_rules! subtract {

    ($x:expr, $y:expr) => {
        x - y;
    }
}