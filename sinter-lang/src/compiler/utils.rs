macro_rules! named_slice {
    ($ident:ident, $ty:ty) => {
        #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
        pub struct $ident {
            inner: Arc<[$ty]>,
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
        }

        impl Deref for $ident {
            type Target = [$ty];

            fn deref(&self) -> &Self::Target {
                &self.inner
            }
        }

        impl DerefMut for $ident {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.inner
            }
        }
    };
}

macro_rules! named_strmap {
    ($ident:ident, $ty:ty) => {
        #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
        #[serde(transparent)]
        pub struct $ident {
            inner: StrMap<$ty>,
        }

        impl From<StrMap<$ty>> for $ident {
            fn from(inner: StrMap<$ty>) -> Self {
                Self { inner }
            }
        }

        impl Deref for $ident {
            type Target = StrMap<$ty>;

            fn deref(&self) -> &Self::Target {
                &self.inner
            }
        }
    };
}

pub(crate) use named_slice;
pub(crate) use named_strmap;
