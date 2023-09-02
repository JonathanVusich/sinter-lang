macro_rules! named_slice {
    ($ident:ident, $ty:ty) => {
        #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
        #[serde(transparent)]
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

            pub fn is_empty(&self) -> bool {
                self.inner.is_empty()
            }
        }

        impl Deref for $ident {
            type Target = [$ty];

            fn deref(&self) -> &Self::Target {
                &self.inner
            }
        }
    };
}

macro_rules! named_strmap {
    ($ident:ident, $ty:ty) => {
        #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
        #[serde(transparent)]
        pub struct $ident {
            inner: Arc<StrMap<$ty>>,
        }
        
        impl $ident {
            pub fn empty() -> Self {
                Self {
                    inner: Arc::new(HashMap::new())
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
}

pub(crate) use named_slice;
pub(crate) use named_strmap;
