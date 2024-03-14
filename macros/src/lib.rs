#[macro_export]
macro_rules! named_slice {
    ($lf:lifetime, $ident:ident, $ty:ty) => {
        #[derive(Clone, Debug, PartialEq, ::serde::Serialize)]
        #[serde(transparent)]
        pub struct $ident<$lf> {
            inner: ::std::sync::Arc<[$ty]>,
        }

        impl<$lf> From<Vec<$ty>> for $ident<$lf> {
            fn from(value: Vec<$ty>) -> Self {
                Self {
                    inner: value.into(),
                }
            }
        }

        impl<$lf> $ident<$lf> {
            pub fn empty() -> Self {
                Self {
                    inner: Vec::new().into(),
                }
            }

            pub fn len(&self) -> usize {
                self.inner.len()
            }

            pub fn is_empty(&self) -> bool {
                self.inner.is_empty()
            }

            pub fn iter(&self) -> impl Iterator<Item = &$ty> {
                self.inner.iter()
            }

            pub fn as_slice(&self) -> &[$ty] {
                self.inner.as_ref()
            }
        }

        impl<$lf> ::std::ops::Index<usize> for $ident<$lf> {
            type Output = $ty;

            fn index(&self, index: usize) -> &Self::Output {
                &self.inner[index]
            }
        }

        impl<'anon, $lf> IntoIterator for &'anon $ident<$lf> {
            type Item = &'anon $ty;
            type IntoIter = ::std::slice::Iter<'anon, $ty>;

            fn into_iter(self) -> Self::IntoIter {
                self.inner.iter()
            }
        }
    };
    ($ident:ident, $ty:ty) => {
        #[derive(Clone, Debug, PartialEq, ::serde::Serialize, ::serde::Deserialize)]
        #[serde(transparent)]
        pub struct $ident {
            inner: ::std::sync::Arc<[$ty]>,
        }

        impl From<Vec<$ty>> for $ident {
            fn from(value: Vec<$ty>) -> Self {
                Self {
                    inner: value.into(),
                }
            }
        }

        impl $ident {
            pub fn empty() -> Self {
                Self {
                    inner: Vec::new().into(),
                }
            }

            pub fn len(&self) -> usize {
                self.inner.len()
            }

            pub fn is_empty(&self) -> bool {
                self.inner.is_empty()
            }

            pub fn iter(&self) -> impl Iterator<Item = &$ty> {
                self.inner.iter()
            }

            pub fn as_slice(&self) -> &[$ty] {
                self.inner.as_ref()
            }
        }

        impl ::std::ops::Index<usize> for $ident {
            type Output = $ty;

            fn index(&self, index: usize) -> &Self::Output {
                &self.inner[index]
            }
        }

        impl<'a> IntoIterator for &'a $ident {
            type Item = &'a $ty;
            type IntoIter = ::std::slice::Iter<'a, $ty>;

            fn into_iter(self) -> Self::IntoIter {
                self.inner.iter()
            }
        }
    };
}

#[macro_export]
macro_rules! named_strmap {
    ($ident:ident, $ty:ty) => {
        #[derive(Clone, Debug, PartialEq, Serialize)]
        #[serde(transparent)]
        pub struct $ident {
            inner: ::std::sync::Arc<StrMap<$ty>>,
        }

        impl $ident {
            pub fn empty() -> Self {
                Self {
                    inner: ::std::sync::Arc::new(StrMap::new()),
                }
            }
        }

        impl From<StrMap<$ty>> for $ident {
            fn from(inner: StrMap<$ty>) -> Self {
                Self {
                    inner: Arc::new(inner),
                }
            }
        }

        impl Deref for $ident {
            type Target = StrMap<$ty>;

            fn deref(&self) -> &Self::Target {
                &self.inner
            }
        }
    };
    ($lf:lifetime, $ident:ident, $ty:ty) => {
        #[derive(Clone, Debug, PartialEq, Serialize)]
        #[serde(transparent)]
        pub struct $ident<$lf> {
            inner: ::std::sync::Arc<StrMap<$ty>>,
        }

        impl<$lf> $ident<$lf> {
            pub fn empty() -> Self {
                Self {
                    inner: ::std::sync::Arc::new(StrMap::new()),
                }
            }
        }

        impl<$lf> From<StrMap<$ty>> for $ident<$lf> {
            fn from(inner: StrMap<$ty>) -> Self {
                Self {
                    inner: Arc::new(inner),
                }
            }
        }

        impl<$lf> Deref for $ident<$lf> {
            type Target = StrMap<$ty>;

            fn deref(&self) -> &Self::Target {
                &self.inner
            }
        }
    };
}
